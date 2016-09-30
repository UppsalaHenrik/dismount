

setSaddleReset <- function(modFilePath, saddleReset = 1){
  
  # Check that the file exists
  if(!file.exists(modFilePath)){
    stop("File ", modFilePath, " not found.")
  }
  
  # Print a message
  print(paste0("Preparing model file ", modFilePath, " by ",
              "setting SADDLE_RESET=", saddleReset))

  modFileOrig <- readLines(modFilePath)
  
  # Check for existing instances of SADDLE_RESET and remove them
  grep("SADDLE_RESET")
  
  
  
  
  
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
  
  
  
  
  
  
  
}