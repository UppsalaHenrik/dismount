

setSaddleReset <- function(modFilePath, saddleReset = 1){
  
  # Check that the file exists
  if(!file.exists(modFilePath)){
    stop("File ", modFilePath, " not found.")
  }
  
  # Print a message
  print(paste0("Preparing model file ", modFilePath, " by ",
               "setting SADDLE_RESET=", saddleReset))
  
  modFileOrig <- readLines(modFilePath)
  
  # Get the $ statement rows. 
  dollarAndLastRows <- c(grep("^\\$", modFileOrig), length(modFileOrig)+1)
  
  # Pick out the index of the EST rows in the dollarAndLastRows
  estRowsIndex <- grep("^\\$EST", modFileOrig[dollarAndLastRows])
  
  estRowsList <- unlist(lapply(estRowsIndex, function(x){
    rows <- dollarAndLastRows[x]:(dollarAndLastRows[x+1]-1)
  }))
  
  estRows <- modFileOrig[estRowsList]
  
  # Check for existing instances of SADDLE_RESET and remove them
  gsub("\\s+SADDLE_RESET=[0-9]+\\s", " ", estRows)
  
  # Add the new SADDLE_RESET option last on the first row.
  newEstRows <- estRows
  newEstRows[1] <- paste0(estRows[1], " SADDLE_RESET=", saddleReset)
  
  modFile <- modFileOrig
  modFile[estRowsList] <- newEstRows
  
  writeLines(modFile, basename(modFilePath))
  
  return(basename(modFilePath))

}


setSaddleHess <- function(modFilePath, saddleHess = 1){
  
  # Check that the file exists
  if(!file.exists(modFilePath)){
    stop("File ", modFilePath, " not found.")
  }
  
  # Print a message
  print(paste0("Preparing model file ", modFilePath, " by ",
               "setting SADDLE_HESS=", saddleHess))
  
  modFileOrig <- readLines(modFilePath)
  
  # Get the $ statement rows. 
  dollarAndLastRows <- c(grep("^\\$", modFileOrig), length(modFileOrig)+1)
  
  # Pick out the index of the EST rows in the dollarAndLastRows
  estRowsIndex <- grep("^\\$EST", modFileOrig[dollarAndLastRows])
  
  estRowsList <- unlist(lapply(estRowsIndex, function(x){
    rows <- dollarAndLastRows[x]:(dollarAndLastRows[x+1]-1)
  }))
  
  estRows <- modFileOrig[estRowsList]
  
  # Check for existing instances of SADDLE_HESS and remove them
  gsub("\\s+SADDLE_HESS=[0-9]+\\s", " ", estRows)
  
  # Add the new SADDLE_HESS option last on the first row.
  newEstRows <- estRows
  newEstRows[1] <- paste0(estRows[1], " SADDLE_HESS=", saddleHess)
  
  modFile <- modFileOrig
  modFile[estRowsList] <- newEstRows
  
  writeLines(modFile, basename(modFilePath))
  
  return(basename(modFilePath))
  
}