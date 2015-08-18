#' Full workflow with parallel retries, result categorization and dismount/precond run
#'
#'
#'
#' @param modFileName The model file to use. Default is "run83.mod".
#' @param retries The number of retries to run as part of the initial parallel retries.
#'
#' @author Henrik Bjugård Nyberg
#'
#'

dismountWorkflow <- function(modFileName = "run83.mod", retries = 9, ...){

  #Set up a folder for the workflow
  modFileNameNoExt <- fileSysSetup(modFileName, "massDismount")


  # Move into that directory
  setwd(dirName)

  print("Run para retries")
  # Run initial para retries (wait = TRUE)
  paraRetriesDirName <- runParaRetries(modFileName, min_retries = retries, degree = 0.99,
                            slurm_partition = "standard", local = FALSE, nm_output = "rmt",
                            seed = 20150806)

  # Find rawres file and parse it. Just in case there is more than one matched I take the first one.
  rawresPath <- findRawres(".")
  rawres <- parseRawres(rawresPath)

  rawresNoNA <- subset(rawres, ofv != 'NA')

  # add retry number to rawres dataframe
  retry <- rawresNoNA$model - 1
  rawresNoNA <- cbind(retry, rawresNoNA)

  # Categorize retries, pick out runs with min success, cov fail and an ofv higher than minimum
  # These are potential saddle points
  minOfv <- min(rawresNoNA$ofv)
  covFailOverMLERawres <- subset(rawresNoNA, minimization_successful == 1 &
                                 covariance_step_successful == 0 &
                                 ofv > minOfv + 1)

  # Pick out runs at minimum with min success and cov fail, These can probably be bettered with precond
  covFailAtMLERawres <-subset(rawresNoNA, minimization_successful == 1 &
                              covariance_step_successful == 0 &
                              ofv < minOfv + 1)



  # Run dismount of all the models.

  # list the model files
  retryModFileNames <- list.files(pattern = paste0("retry.+\\.mod"))

  # run dismount on them
  dismountDirs <- sapply(retryModFileNames, runDismount)

  # Wait for the queue to be empty
  waitForSlurmQ(targetLength = 1)




  print("done... so far")
    # parse isestimable files

  # If certain situation (error messages?), run precond

    # parse precond rawres file

  # Build new rawres with all the runs (hopefully they are now all at MLE)


  # Set back the working directory

  setwd("..")

}









# Below is some ancient code that was the starting point for the above. Please disregard










# The below block of code was there for catego
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
