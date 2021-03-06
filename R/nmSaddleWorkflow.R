#' Full workflow with parallel retries, result categorization and dismount/precond run
#'
#'
#'
#' @param modFileName The model file to use. Default is "run83.mod".
#' @param retries The number of retries to run as part of the initial parallel 
#' retries.
#' @param doParaRetries Whether or not the parallel retries should be run. 
#' Default is TRUE. If FALSE then a rerunDirName is expected.
#' @param doDismount Whether or not dismount should be run on all samples. 
#' Default is TRUE.
#' @param rerunDirName If doParaRetries is FALSE, then this directory will be 
#' assumed to contain a parallel retries run with model files and outputs to 
#' use for dismount and/or precond. No default.  
#' @param dismountPertDirections Perturbation direction.
#'
#' @export
#'
#' @author Henrik Bjugård Nyberg - henrik.b.nyberg@@farmbio.uu.se
#'

nmSaddleWorkflow <- function(modFileName, retries = 9, doParaRetries = TRUE, 
                             doDismount = FALSE, doNmDismount = TRUE,
                             doCompParaRetries = TRUE, rerunDirName,
                             degree = 0.99, seed = 20170115, dismountPertDirections = 1,
                             saddleReset = 0, saddleHess = 0, slurm_partition = "standard"){
  
  # Save the current working directory for later
  userWD <- getwd()
  
  #Set up folders for the workflow
  dismountRunsDir <- "dismountRuns"
  nmDismountRunsDir <- "nmDismountRuns"
  compParaRetriesDir <- "compParaRetries"
  modFileNameNoExt <- fileSysSetup(modFileName, "massRun", c(dismountRunsDir, 
                                                             nmDismountRunsDir, 
                                                             compParaRetriesDir))
  
  
  workflowWD <- getwd()
  
  # Run initial para retries (wait = TRUE)
  if(doParaRetries){
    print("Running parallel retries")
    paraRetriesDirName <- runParaRetries(modFileName, min_retries = retries, degree = degree,
                                         paraRetriesCmd = paste0("perl /blue/home/USER/bjuny231/",
                                                                 "PrecondProject/_HackedPsN7/",
                                                                 "PsN4_4_ver_YA/bin/parallel_retries"),
                                         slurm_partition = slurm_partition, nm_output = c("rmt", "ext"), 
                                         seed = seed)
    
    #An initial wait for it to reach the queue. One thenth of a second per job is assumed as minimum
    Sys.sleep(retries/10)
    
    # Wait for the queue to have only the master job left
    waitForSlurmQ(targetLength = 0)
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
  retryModFilePaths <- list.files(path = paraRetriesDirName, 
                                  pattern = "retry.+mod$")
  retryFilePaths <- list.files(path = paraRetriesDirName, 
                               pattern = "retry.+$")
  
  
  # Initialize a list to store all the OFVs from the different procedures
  ofvList <- list()
  
  #add paraRetriesOfvs to that list 
  paraRetriesOfvs <- cbind(retry = paraRetriesRawresNoNA$retry, 
                           paraRetriesOfv = paraRetriesRawresNoNA$ofv)
  
  write.csv(paraRetriesOfvs, "paraRetriesOfvs.csv", row.names = FALSE)
  
  ofvList[length(ofvList)+1] <- list(paraRetriesOfvs)
  
  if(doNmDismount){
    
    # Copy the retries files into theNM dismount runs directory
    file.copy(paste0(paraRetriesDirName, "/", retryFilePaths), nmDismountRunsDir)
    
    # Set the NM dismount runs directory as WD
    setwd(nmDismountRunsDir)
    
    # Spin over all the combinations of saddleReset and saddleHess
    saddleOptionDf <- expand.grid(saddleReset, saddleHess)
    
    nmDismountOfvs <- apply(saddleOptionDf, 1, function(x){
      
      # Create the directrory and set wd to it
      dirName <- paste0("reset", x[1], "_hess", x[2])

      # Run the Nonmem runs
      ofvs <- workflowNmDismountRuns(retryModFilePaths = list.files(pattern = ".mod$"), 
                                     dirName = dirName, saddleReset = x[1], 
                                     saddleHess = x[2], slurm_partition = slurm_partition)

      # Return the OFVs
      return(ofvs)
    })
    
    ofvList[length(ofvList)+1] <- list(nmDismountOfvs)
    
    setwd(workflowWD)
    
    # Write out a file
    write.csv(nmDismountOfvs, "nmDismountOfvs.csv", row.names = FALSE)
    
  }
  
  
  if(doDismount){  
    
    # Copy the retries files into the precond runs directory
    file.copy(paste0(paraRetriesDirName, "/", retryFilePaths), dismountRunsDir)
    
    # Set the dismount runs directory as WD
    setwd(dismountRunsDir)
    
    # Run dismount on all the retries
    dismountOfvsList <- lapply(dismountPertDirections, function(x){
      
      dismountOfvs <- workflowDismountRuns(retryModFilePaths, 
                                           dismountPertDirection = x)  
      
    })

    # Merge the list, compare the ofvs and put together a data frame with the 
    # lowest ofvs from the different directions.
    dismountOfvPairs <- Reduce(function(...) merge(..., by = "retry", 
                                                   all = TRUE), dismountOfvsList)
    
    dismountOfvs <- data.frame(dismountOfvPairs$retry,
                               pmin(dismountOfvPairs$dismountOFV.x, 
                                    dismountOfvPairs$dismountOFV.y, 
                                    na.rm = TRUE))
    
    
    ### TODO Set retry to 0 for the original, not NA.
    
    
    names(dismountOfvs) <- c("retry", "dismountOfv")
    
    # Add the dismount OFVs to the list of ofvs to compare
    ofvList[length(ofvList)+1] <- list(dismountOfvs)
    
    #Set back the wd
    setwd(workflowWD)
    
    # Write out a file
    write.csv(dismountOfvs, "dismountOfvs.csv", row.names = FALSE)
    
    
  }
  
  if(doCompParaRetries){
    
    # Copy those files into the compParaRetries runs directory
    # TODO: Should put the bolean vector from file.copy into something and 
    # check for failures
    file.copy(paste0(paraRetriesDirName, "/", retryFilePaths), 
              compParaRetriesDir)
    
    # Set the comp para retries directory as WD
    setwd(compParaRetriesDir)
    
    # Run update on all of them to get the right initial values
    # Not super clean since theoretically there could be other model files in this directory
    sapply(list.files(pattern = ".mod"), function(x){
      
      system(paste("update", x))
      
    })
    
    # Run para retries
    compParaRetriesDirList <- sapply(retryModFilePaths, function(x){
      
      # Wait for the SLURM queue to have less than a certain number of jobs in it
      waitForSlurmQ(targetLength = 100)
      
      #system(paste0("update_inits ", x))
      
      runParaRetries(x, wait = FALSE, min_retries = 1, 
                     slurm_partition = slurm_partition, 
                     extraOptions = "-nm_version=7_40_g51")
      
      Sys.sleep(10)
      
    })
    
    # I've had issues with the runs not strting before I start the wait below, 
    # so here is a little initial wait
    Sys.sleep(10)
    
    # Wait for the queue to have only the master job left
    waitForSlurmQ(targetLength = 0)
    Sys.sleep(5)
    
    # Here I use the ext files rather than the rawres files
    extFiles <- list.files(pattern = ".+retr.+.ext$")
    
    paraRetriesCompOfvRows <- lapply(extFiles, function(x){
      
      retry <- gsub("^.+retry", "", x)
      retry <- as.integer(gsub("\\....$", "", retry))
      
      extFileDFList <- parseExtFile(x)
      
      # Get the last table in the ext file
      extFileDF <- extFileDFList[[length(extFileDFList)]]
      
      # Pick out the final parameter values row
      paramVectorRow <- subset(extFileDF, ITERATION == -1e+9)
      
      # Pick out the OBJ values and package it with the original values of 
      # paramsToCompare
      paraRetriesCompOfv <- cbind(paramVectorRow[length(paramVectorRow)])
      
      print(x)
      
      ofvRow <- unlist(c(retry, paraRetriesCompOfv))
      names(ofvRow) <- c("retry", "paraRetriesCompOfv")
      
      return(ofvRow)
      
    })
    
    compParaRetriesOfvs <- data.frame(do.call("rbind", paraRetriesCompOfvRows))
    
    setwd(workflowWD)
    
    write.csv(compParaRetriesOfvs, "compParaRetriesOfvs.csv", row.names = FALSE)
    
    ofvList[length(ofvList)+1] <- list(compParaRetriesOfvs)
  }
  
  # I put together an OFV comparison DF, write it out for future reference and 
  # return it.
  compOfvs <- Reduce(function(x, y) merge(x, y, all=TRUE), ofvList)
  
  write.csv(compOfvs, "OFVComparison.csv", row.names = FALSE)
  
  ### TODO Code the cleanup, either all here or in each subsection
  
  setwd(userWD)
  
  return(list(workflowWD, compOfvs))
}