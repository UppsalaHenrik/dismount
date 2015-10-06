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
  
  # add retry number to rawres dataframe
  retry <- paraRetriesRawresNoNA$model - 1
  paraRetriesRawresNoNA <- cbind(retry, paraRetriesRawresNoNA)
  
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
    precondRawresFiles <- list.files(recursive = TRUE)[grep("pert_init_est_modelfit/raw_results.csv", list.files(recursive = TRUE))]
    precondRawresList <- lapply(precondRawresFiles, parseRawres)
    precondRawres <- do.call("rbind", precondRawresList)
    
    # I bind in the retry number as well
    # This is a little dangerous as it assumes the order is the same...
    # I could do this within the apply above instead, and parse the actual number...
    precondRawres <- cbind(retry[-1], precondRawres)
    
    # Pick out the
    overMinOfvPrecondRetries <- subset(precondRawres, ofv > minOfv + 1)
    nOverMinOfvPrecondRetries <- nrow(overMinOfvPrecondRetries)
    
    # Print a message about the number of retries over
    print(paste("After precond on", length(retryModFilePaths), "samples,",
                nOverMinOfvPrecondRetries, "samples were over minimum OFV by 1 or more"))
    
  }
  
  setwd(workflowWD)
  
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
    
    # I bind in the retry number as well
    # This is a little dangerous as it assumes the order is the same...
    # I could do this within the apply above instead, and parse the actual number...
    dismountRawres <- cbind(retry[-1], dismountRawres)
    
    # Pick out the
    overMinOfvDismountRetries <- subset(dismountRawres, ofv > minOfv + 1)
    nOverMinOfvDismountRetries <- nrow(overMinOfvDismountRetries)
    
    # Print a message about the number of retries over
    print(paste("After dismount on", length(retryModFilePaths), "samples,",
                nOverMinOfvDismountRetries, "samples were over minimum OFV by 1 or more"))
  }
  setwd(userWD)
}









# Below is some ancient code that was the starting point for the above. Please disregard





# 
# # List the retries that are above min ofv
# overMinOfvRetries <-  subset(rawresNoNA, ofv > minOfv + 1)
# nOverMinOfvRetries <- nrow(overMinOfvRetries)
# 
# # Print a message about the number of retries over
# print(paste("After parallel retries with", retries, "samples,",
#             nOverMinOfvRetries, "samples were over minimum OFV by 1 or more"))
# 


# The below block of code was there for categorization, which I am now skipping.



# Categorize retries, pick out runs with min success, cov fail and an ofv higher than minimum
# These are potential saddle points
#   minOfv <- min(rawresNoNA$ofv)
#   covFailOverMLERawres <- subset(rawresNoNA, minimization_successful == 1 &
#                                  covariance_step_successful == 0 &
#                                  ofv > minOfv + 1)
#
#   # Pick out runs at minimum with min success and cov fail, These can probably be bettered with precond
#   covFailAtMLERawres <-subset(rawresNoNA, minimization_successful == 1 &
#                               covariance_step_successful == 0 &
#                               ofv < minOfv + 1)
#
#
#
#   # Run dismount of all the models.
#
#   # list the model files
#   retryModFileNames <- list.files(pattern = paste0("retry.+\\.mod"))
#
#   # run dismount on them
#   dismountDirs <- sapply(retryModFileNames, runDismount)
#
#   # Wait for the queue to be empty
#   waitForSlurmQ(targetLength = 1)
#
#
#
#
#   print("done... so far")
# parse isestimable files

# If certain situation (error messages?), run precond

# parse precond rawres file

# Build new rawres with all the runs (hopefully they are now all at MLE)


# Set back the working directory

#
#   # parse relevant lst files for saddle point covariance step error messages
#   saddleRetriesList <- lapply(covFailOverMLERawres$retry, function(x){
#
#     lstFileName <- list.files(pattern = paste0("retry", x, ".lst"))
#
#     covMessages <- parseCovMessages(lstFileName)
#
#     # Look for the saddle point message and, if found, return x, the retry number
#     if(grepl("R MATRIX ALGORITHMICALLY NON-POSITIVE-SEMIDEFINITE BUT NONSINGULAR", covMessages)){
#       return(x)
#     }
#
#   # If the saddle point message isn't found, return NULL
#   return(NULL)
#   })
#
#   saddleRetries <- unlist(saddleRetriesList)
#
#   # Find the model files that correspond to the saddle point retries
#   saddleModelFiles <- unlist(lapply(saddleRetries, function(x){
#     list.files(pattern = paste0("retry", x, ".mod"))
#   }))
#
#   # Run isestimable (dismount) on them
#
#   dismountDirs <- sapply(saddleModelFiles, runDismount)



#
#
# read.csv()
#
# ofvLim <- min(rawres$ofv, na.rm = TRUE) + 1
#
# modNums <- rawres$model[rawres$minimization_successful == 1 &
#                           rawres$covariance_step_successful == 0 &
#                           rawres$ofv >= ofvLim]
#
# retryNums <- modNums - 1
#
#
#
# lstFiles <- getParaRetryFiles(path = "./", fileExt = ".lst", retryNumsToParse = retryNums)
#
# covMessageList <- sapply(lstFiles, parseCovMessages)
#
#
# list.files()[]
#
#
# rMatFiles <- list.files()[grep("\\.rmt", list.files())]
#
# rMatList <- lapply(rMatFiles, parseNonmemMat)
#
# rMatListNoZero <- lapply(lapply(rMatList, stripZeroRowsCols), '[[', 1)
#
# rMatEigenDecompList <- lapply(rMatListNoZero, eigen)
#
# lapply(rMatEigenDecompList, '[', "values").
