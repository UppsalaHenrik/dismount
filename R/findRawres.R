#' Find the raw results csvs in a specified location
#'
#' @param path The path in which to look.
#'
#'
#' @export
#'
#' @author Henrik Bjugård Nyberg - henrik.b.nyberg@@farmbio.uu.se

findRawres <- function(path){

  # Find the relevant file
  rawresFileName <- list.files(path)[grep("^raw_results_.+\\.csv", list.files(path))]
  rawresPath <- paste0(path, "/", rawresFileName)

  # Check wether the relevant folder and files exist
  dirExists <- file.exists(path)
  rawresExists <- file.exists(rawresPath)

  # If the directory cannot be found, return NULL
  if(dirExists == FALSE){
    dirMessage <- paste("Could not find directory", path)
    print(dirMessage)
    return(NULL)
  }

  # If the raw results file cannot be found, return NULL
  if(rawresExists == FALSE){
    rawresMessage <- paste("Could not find raw results file in ", path)
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
