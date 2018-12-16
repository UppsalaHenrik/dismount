#' @export
#' 

setSAEM <- function(modFilePath, saemString = "$EST METH=SAEM AUTO=1", 
                    evalStepString = "", printMessage = FALSE){
  
  # Check that the file exists
  if(!file.exists(modFilePath)){
    stop("File ", modFilePath, " not found.")
  }
  
  # if(printMessage){
  #   # Print a message
  #   print(paste0("Preparing model file ", modFilePath, " by ",
  #                "setting SADDLE_RESET=", saddleReset))
  # }

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
  estRows <- gsub("SADDLE_RESET=[0-9]+\\s", " ", estRows)
  
  # Clean up whitespace
  estRows <- gsub("\\s+", " ", estRows)
  
  # Add the new SADDLE_RESET option last on the first row.
  newEstRows <- paste0(saemString, "\n", evalStepString)
  
  modFile <- modFileOrig
  modFile[estRowsList] <- c(newEstRows, rep("", length(modFile[estRowsList])-1))
  
  writeLines(modFile, basename(modFilePath))
  
  return(basename(modFilePath))
  
}
