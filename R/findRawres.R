

findRawres <- function(dirName){

  # Find the relevant file
  rawresFileName <- list.files(dirName)[grep("^raw_results_.+\\.csv", list.files(dirName))]
  rawresPath <- paste0(dirName, "/", rawresFileName)

  # Check wether the relevant folder and files exist
  dirExists <- file.exists(dirName)
  rawresExists <- file.exists(rawresPath)

  # If the directory cannot be found, return NULL
  if(dirExists == FALSE){
    dirMessage <- paste("Could not find directory", dirName)
    print(dirMessage)
    return(NULL)
  }

  # If the raw results file cannot be found, return NULL
  if(rawresExists == FALSE){
    rawresMessage <- paste("Could not find raw results file in ", dirName)
    print(rawresMessage)
    return(NULL)
  }

  # If there is more than one matching file, pick the first one and print message
  if(length(rawresPath) > 1 ){

    print("More than one matching file found:")
    cat(rawresPath, sep = "\n")
    print("Selecting the first one.")

    rawresPath <- rawresPath[1]
  }
  return(rawresPath)

}
