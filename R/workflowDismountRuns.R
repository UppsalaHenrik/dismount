


workflowDismountRuns <- function(retryModFilePaths, 
                                 dismountPertDirection){

  # Save current working directory, create a subdirectory, copy the model files 
  # to it and set it as wd
  cwd <- getwd()
  
  pertDirectionDirName <- paste0("pertDirection_", dismountPertDirection)
  dir.create(pertDirectionDirName)
  
  file.copy(list.files(), pertDirectionDirName)
  
  setwd(pertDirectionDirName)
  
  # Run dismount on the models
  dismountDirList <- sapply(retryModFilePaths, function(x){
    
    runDismount(x, pertDir = dismountPertDirection)
    
    Sys.sleep(0.5)
    
  })

  # I've had issues with the runs not starting before I start the waitForSlurmQ 
  # below, so here is a little initial wait
  Sys.sleep(10)
  
  # Wait for the queue to have only the master job left
  waitForSlurmQ(targetLength = 1)
  
  # Expected rawres file path
  dismountRawresPath <- "pert_init_est_modelfit/raw_results.csv"
  
  # Find and parse the rawres files, and then put them together
  dismountRawresFiles <- list.files(recursive = TRUE)[grep(dismountRawresPath, 
                                                           list.files(recursive = TRUE))]
  
  dismountRawresList <- lapply(dismountRawresFiles, parseRawres)
  dismountRawres <- do.call("rbind", dismountRawresList)
  
  # Parse the retry number from the path
  dismountRetry <- as.numeric(gsub("/.+$", "", 
                                   gsub(".+retry", "", 
                                        dismountRawres$rawresPath)))
  
  # Combine into a data frame
  dismountRawres <- cbind(dismountRawres, retry = dismountRetry)
  
  # Add group information about estimation and cov step success
  dismountRawres <- assignExecutionGroups(dismountRawres)
  
  # Move WD back to the parent dir and write out the rawres file
  setwd(cwd)
  
  write.csv(dismountRawres, paste0(pertDirectionDirName, ".csv"), 
            row.names = FALSE)
  
  dismountOFVs <- cbind(retry = dismountRawres$retry, 
                        dismountOFV = dismountRawres$ofv)
  
  return(dismountOFVs)
}