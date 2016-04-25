#' Parse a PsN raw results file
#'
#'
#' @param rawresPath The file path for the rawres file to parse. No default.
#' @param cols A vector of column names or numbers to be parsed. If set to vector of length 1 or longer all other columns will be discarded. Please observe that column names must match the read.csv interpretation of the column header.
#' @param skipRows Number of rows to skip after the header. Not to be confused with 'skip' which is a read.csv option that will skip rows from the top of the file before looking for a header.
#' @param addPath If set the the returned data frame will include a column containing the file name that was parsed. Default is FALSE.
#' @param addGroup If set the returned data frame will include a group column with a group number based on minimization and covariance step success. Default is FALSE
#'
#' parseRawres()
#'
#'
#' @export
#'
#' @author Henrik Bjugård Nyberg - henrik.b.nyberg@@farmbio.uu.se


parseRawres <- function(rawresPath, cols = NULL, skipRows = 0,
                        addPath = FALSE, addGroup = FALSE){

  print(paste("Parsing", rawresPath))

  # Check wether the relevant folder and files exist
  rawresExists <- file.exists(rawresPath)

  # If the raw results file cannot be found write out a message and return NULL
  if(rawresExists == FALSE){
    rawresMessage <- paste("Could not find raw results file", rawresPath)
    print(rawresMessage)
    return(NULL)
  }

  rowsToInclude <- skipRows
  
  # Parse rawres into data frame

  if(length(cols) > 0){
    
    rawres <- read.csv(rawresPath, header = TRUE, check.names = FALSE)[cols]
    
  }else{
    
    rawres <- read.csv(rawresPath, header = TRUE, check.names = FALSE)
    
  }
  
  
  # If skipRows is set then subset that many rows from the top, keeping the header
  if(skipRows > 0){
    
    rawres <- rawres[-(1:skipRows),]
    
  }
  
  # If the addPath option is set, append it
  if(addPath == TRUE){
    rawres$rawresPath <- rawresPath
  }
  
  # If the addGroup option is set, append the group assigned 
  if(addGroup == TRUE){
    rawres <- assignExecutionGroups(rawres)
  }

  # Return the data frame
  return(rawres)
}
