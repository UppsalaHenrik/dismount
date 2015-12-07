


parseExtFile <- function(extFilePath){
  
  # Check to make sure the referenced file exists
  if (!file.exists(extFilePath)) {
    stop("Failed to parse ext file; expected input file \"", 
         extFilePath, "\" does not exist.")
  }
  
  # Parsing the .ext using readLines because of different types of 
  # pesky whitespace. Also remove the first row and separate header
  rawExtFile <- readLines(extFilePath)
  header <- unlist(strsplit(gsub("^\\s+", "", rawExtFile[2]), "\\s+"))
  extFile <- rawExtFile[3:length(rawExtFile)]
  
  # Remove leading whitespace
  extFile <- gsub("^\\s+", "", extFile)
  
  # split the lines up and pack them into a df
  extFileCharDF <- data.frame(do.call("rbind", strsplit(extFile, "\\s+")), 
                              stringsAsFactors = FALSE)
  names(extFileCharDF) <- header
  
  # I need to convert the characters to numerics
  extFileDF <- as.data.frame(sapply(extFileCharDF, as.numeric))
  
  # Return the ext file contents as a dataframe
  return(extFileDF)
}