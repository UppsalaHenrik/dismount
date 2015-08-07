
parseCovMessages <- function(lstFilePath){
  
  # Read the file in
  lstFile <- readLines(lstFilePath)
  
  # The messages seem to start after the estimation time message.
  # This should hopefully be unique enough
  startRow <- grep("^ Elapsed est", lstFile) + 1
  
  # I read in rows until the covariance step time message.
  endRow <- grep("^ Elapsed cov", lstFile) - 1
  
  messages <- gsub("0", "", paste(lstFile[startRow:endRow], collapse = " "))
  
  # Remove surplus whitespace
  messages <- gsub('\\s+', " ", messages)
  
  # I return the messages as a vector of strings 
  return(messages)
}