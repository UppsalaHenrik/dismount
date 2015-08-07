
parseRawres <- function(rawresPath){
  
  print(paste("Parsing", rawresPath))
  
  # Check wether the relevant folder and files exist
  rawresExists <- file.exists(rawresPath)
  
  # If the raw results file cannot be found, return NULL
  if(rawresExists == FALSE){
    rawresMessage <- paste("Could not find raw results file", rawresPath)
    print(rawresMessage)
    return(NULL)
  }
  
  # Parse rawres into data frame and return it
  rawres <- read.csv(rawresPath)
 
  return(rawres)
}