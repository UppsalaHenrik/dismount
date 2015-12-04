#' Full workflow with parallel retries, result categorization and dismount/precond run
#'
#'
#'
#' @param modFileName The model file to use. Default is "run83.mod".
#' @param retries The number of retries to run as part of the initial parallel retries.
#' @param doParaRetries Whether or not the parallel retries should be run. Default is TRUE. If FALSE then a rerunDirName is expected.
#' @param doPrecond Whether or not preconditioning should be run on all samples. Default is FALSE
#' @param doDismount Whether or not dismount should be run on all samples. Default is TRUE.
#' @param rerunDirName If doParaRetries is FALSE, then this directory will be assumed to contain a parallel retries run with model files and outputs to use for dismount and/or precond. No default.  
#'
#'
#' @export
#'
#' @author Henrik Bjugård Nyberg - henrik.b.nyberg@@farmbio.uu.se
#'

dismountWorkflow <- function(modFileName, retries = 9, doParaRetries = TRUE, 
                             doPrecond = FALSE, doDismount = TRUE, rerunDirName){
  
  # Save the current working directory for later
  userWD <- getwd()
  
  #Set up folders for the workflow
  dismountRunsDir <- "dismountRuns"
  precondRunsDir <- "precondRuns"
  modFileNameNoExt <- fileSysSetup(modFileName, "massRun", c(dismountRunsDir, precondRunsDir))
  
  workflowWD <- getwd()
  
  # Run initial para retries (wait = TRUE)
  if(doParaRetries){
    print("Running parallel retries")
    paraRetriesDirName <- runParaRetries(modFileName, min_retries = retries, degree = 0.99,
                                         slurm_partition = "standard", local = FALSE, 
                                         nm_output = c("rmt", "ext"), seed = 20150806)
    
    # Wait for the queue to have only the master job left
    waitForSlurmQ(targetLength = 1)
  }else{
    
    paraRetriesDirName <- rerunDirName
    
  }
  
  # Find rawres file and parse it. Just in case there is more than one matched I take the first one.
  paraRetriesRawresPath <- findRawres(paraRetriesDirName)
  paraRetriesRawres <- parseRawres(paraRetriesRawresPath)
  
  paraRetriesRawresNoNA <- subset(paraRetriesRawres, ofv != 'NA')
  
  # Find the minimum OFV value to use as reference
  minOfv <- min(paraRetriesRawresNoNA$ofv)
  
  # add retry number and group to rawres dataframe, and write it out
  retry <- paraRetriesRawresNoNA$model - 1
  paraRetriesRawresNoNA <- cbind(retry, paraRetriesRawresNoNA)
  paraRetriesRawresNoNA <- assignExecutionGroups(paraRetriesRawresNoNA)
  
  write.csv(paraRetriesRawresNoNA, "paraRetriesRawres.csv", row.names = FALSE)
  
  # List the Retry files for dismount and/or precond runs
  # This might need to be more strict
  retryModFilePaths <- list.files(path = paraRetriesDirName, pattern = "retry.+mod$")
  retryFilePaths <- list.files(path = paraRetriesDirName, pattern = "retry.+$")
  
  if(doPrecond){
    
    # Copy those files into the precond runs directory
    file.copy(paste0(paraRetriesDirName, "/", retryFilePaths), precondRunsDir)
    
    # Set the precond runs directory as WD
    setwd(precondRunsDir)
    
    # Run precond on the models
    precondDirList <- sapply(retryModFilePaths, runPrecond)
    # I've had issues with the runs not strting before I start the wait below, so here is a little initial wait
    Sys.sleep(10)
    
    # Wait for the queue to have only the master job left
    waitForSlurmQ(targetLength = 1)
    
    # Find and parse the rawres files, and then put them together
    precondRawresFiles <- list.files(recursive = TRUE)[grep("raw_results.+retry.+csv", 
                                                            list.files(recursive = TRUE))]
    
    precondRawresList <- lapply(precondRawresFiles, parseRawres)
    precondRawres <- do.call("rbind", precondRawresList)
    
    # New way of adding retry info. 
    # I take it from the path that is included in the rawres file created above.
    # Still a little dodgy though
    precondRetry <- as.numeric(gsub(".csv", "", gsub(".+retry", "", precondRawres$rawresPath)))
    
    precondRawres <- cbind(precondRawres, retry = precondRetry)
    
    precondRawres <- assignExecutionGroups(precondRawres)
    
    setwd(workflowWD)
    
    write.csv(precondRawres, "precondRawres.csv", row.names = FALSE)
    
  }

  
  if(doDismount){  
    # Copy those files into the dismount runs directory
    file.copy(paste0(paraRetriesDirName, "/", retryFilePaths), dismountRunsDir)
    
    # Set the dismount runs directory as WD
    setwd(dismountRunsDir)
    
    # Run dismount on the models
    dismountDirList <- sapply(retryModFilePaths, runDismount)
    # I've had issues with the runs not strting before I start the wait below, so here is a little initial wait
    Sys.sleep(10)
    
    # Wait for the queue to have only the master job left
    waitForSlurmQ(targetLength = 1)
    
    # Find and parse the rawres files, and then put them together
    dismountRawresFiles <- list.files(recursive = TRUE)[grep("pert_init_est_modelfit/raw_results.csv", list.files(recursive = TRUE))]
    dismountRawresList <- lapply(dismountRawresFiles, parseRawres)
    dismountRawres <- do.call("rbind", dismountRawresList)
    
    dismountRetry <- as.numeric(gsub("/.+$", "", gsub(".+retry", "", dismountRawres$rawresPath)))
    
    dismountRawres <- cbind(dismountRawres, retry = dismountRetry)
    
    dismountRawres <- assignExecutionGroups(dismountRawres)
        
    setwd(workflowWD)
    
    write.csv(dismountRawres, "dismountRawres.csv", row.names = FALSE)
  }

  
  # I need to put together an OFV comparison DF.
  
  paraRetriesOFVsGroups <- cbind(retry = paraRetriesRawresNoNA$retry, 
                                 paraRetriesOFV = paraRetriesRawresNoNA$ofv,
                                 paraRetriesGroup = paraRetriesRawresNoNA$group)
  
  precondOFVsGroups <- cbind(retry = precondRawres$retry, 
                             precondOFV = precondRawres$ofv,
                             precondGroup = precondRawres$group)
  
  dismountOFVsGroups <- cbind(retry = dismountRawres$retry, 
                              dismountOFV = dismountRawres$ofv,
                              dismountGroup = dismountRawres$group)
  
  compOFVsGroups <- merge(merge(paraRetriesOFVsGroups, precondOFVsGroups, by = "retry"), 
                          dismountOFVsGroups, by = "retry")
  
  write.csv(compOFVsGroups, "OFVandGroupComparison.csv", row.names = FALSE)
  
  setwd(userWD)
  
}
