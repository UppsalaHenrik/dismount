#' @export


workflowNmDismountRuns <- function(retryModFilePaths, saddleReset, saddleHess, 
                                   nm_version = "7_40_g51_alpha14"){
  
  # Set the right saddle options
  setModList <- sapply(retryModFilePaths, function(x){
    setSaddleReset(x, saddleReset, printMessage = FALSE)
    setSaddleHess(x, saddleHess, printMessage = FALSE)
  })
  
  modFiles <- as.vector(setModList)
  
  # Run dismount on the models
  dismountDirList <- sapply(modFiles, function(x){
    dirName <- gsub("\\..+", "", x)
    
    system(paste0("execute ", x, " -dir=", dirName))
    
    Sys.sleep(0.5)
    
  })
  
  # I've had issues with the runs not starting before I start the waitForSlurmQ 
  # below, so here is a little initial wait
  Sys.sleep(10)
  
  # Wait for the queue to have only the master job left
  waitForSlurmQ(targetLength = 0)
  
  # Expected rawres file path
  nmDismountRawresPath <- "/raw_results.csv"
  
  # Find and parse the rawres files, and then put them together
  nmDismountRawresFiles <- list.files(recursive = TRUE)[grep(nmDismountRawresPath, 
                                                           list.files(recursive = TRUE))]
  
  nmDismountRawresList <- lapply(nmDismountRawresFiles, function(x){
    
    parseRawres(rawresPath = x, addPath = TRUE)
    
  })
  
  nmDismountRawres <- do.call("rbind", nmDismountRawresList)
  
  # Parse the retry number from the path
  nmDismountRetry <- as.numeric(gsub("/.+$", "", 
                                   gsub(".+retry", "", 
                                        nmDismountRawres$rawresPath)))
  
  # I am correcting an NA to 0 for the original run. 
  # This is potentially dangerous code... 
  ### TODO Do this in a better way
  
  for(i in seq_along(nmDismountRetry)){
    
    if(is.na(nmDismountRetry[i])){
      nmDismountRetry[i] <- 0
      break()
    }
    
  }
  
  # Combine into a data frame
  nmDismountRawres <- cbind(nmDismountRawres, retry = nmDismountRetry)
  
  # Add group information about estimation and cov step success
  nmDismountRawres <- assignExecutionGroups(nmDismountRawres)
  
  # Move WD back to the parent dir and write out the rawres file
  setwd(cwd)
  
  write.csv(nmDismountRawres, paste0(pertDirectionDirName, ".csv"), 
            row.names = FALSE)
  
  nmDismountOFVs <- cbind(retry = nmDismountRawres$retry, 
                        nmDismountOFV = nmDismountRawres$ofv)
  
  return(nmDismountOFVs)
}