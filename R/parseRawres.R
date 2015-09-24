#' Parse a PsN raw results file
#'
#'
#' @param rawresPath The file path for the rawres file to parse. No default.
#' @param addPath If set the the returned data frame will include the file 
#'        name that was parsed. Default is TRUE
#'
#' parseRawres()
#'
#' @author Henrik Bjugård Nyberg - henrik.b.nyberg@@farmbio.uu.se


parseRawres <- function(rawresPath, addPath = TRUE){

  print(paste("Parsing", rawresPath))

  # Check wether the relevant folder and files exist
  rawresExists <- file.exists(rawresPath)

  # If the raw results file cannot be found, return NULL
  if(rawresExists == FALSE){
    rawresMessage <- paste("Could not find raw results file", rawresPath)
    print(rawresMessage)
    return(NULL)
  }

  # Parse rawres into data frame
  rawres <- read.csv(rawresPath)
  
  # If the addPath option is set, append it
  if(addPath == TRUE){
    rawres$rawresPath <- rawresPath
  }

  # Return the data frame
  return(rawres)
}
