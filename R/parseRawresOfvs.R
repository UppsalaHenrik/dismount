#' parseRawresOfvs
#' 
#' @param rawresPath Path to the raw results file to parse OFV values from.
#' 
#' @export
#' 

parseRawresOfvs <- function(rawresPath){
  
  # Check wether the relevant raw res file exist
  rawresExists <- file.exists(rawresPath)
  
  # If the raw results file cannot be found, return NULL
  if(rawresExists == FALSE){
    rawresMessage <- paste("Could not find raw results file at ", rawresPath)
    print(rawresMessage)
    return(NULL)
  }
  
  # Read the OFV column of the raw res file and store it as a vector 
  ofvVector <- read.csv(rawresPath)$ofv[-1]
  
  # Return the vector
  return(ofvVector)
}