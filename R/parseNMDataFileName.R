#' Parse the data file name from a NONMEM model
#'
#' Reads the data file name from a NONMEM control file.
#'
#' @param modFilePath The model file Path.
#'
#'
#' parseNMDataFileName()

parseNMDataFileName <- function(modFilePath){

  # Make sure the model file exists
  if(file.exists(modFilePath) != TRUE){

    print(paste("The NONMEM control file", modFilePath, "was not found."))

    return(NULL)
  }

  # Parse the data file name from the model file
  modelFile <- readLines(modFilePath)

  # Find the data row
  dataRowNum <- grep('^\\$DATA', modelFile)

  # Pick out that line and remove the $DATA and any space after it
  dataRow <- modelFile[dataRowNum]
  cutDataRow <- gsub("^\\$DATA+\\s+", "", dataRow)

  # This is maybe a bit dangerous... picks the first
  # word (after $DATA is removed above) as the file name
  dataFileName <- strsplit(cutDataRow, " ")[[1]][1]


  return(dataFileName)
}
