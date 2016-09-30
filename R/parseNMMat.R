#' parseNMMat
#' 
#' Parses a matrix as output by NONMEM and return a matrix. 
#'  
#' @param nmMatFilePath File Path to the ext file to be parsed.
#' 
#' @export

parseNMMat <- function(nmMatFilePath){
  
  rawMatFile <- readLines(nmMatFilePath)
  header <- unlist(strsplit(gsub("^\\s+", "", rawMatFile[2]), "\\s+"))
  matFile <- rawMatFile[3:length(rawMatFile)]
  
  # Remove leading whitespace
  matFile <- gsub("^\\s+", "", matFile)
  
  # split the lines up and pack them into a df
  matFileCharDF <- data.frame(do.call("rbind", strsplit(matFile, "\\s+")), 
                              stringsAsFactors = FALSE)[,-1]
  names(matFileCharDF) <- header[-1]
  
  # I need to convert the characters to numerics
  mat <- as.matrix(sapply(matFileCharDF, as.numeric))
  row.names(mat) <- header[-1]
  
  # Return the ext file contents as a dataframe
  return(mat)
}
