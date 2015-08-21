#' Set up directories for a set of executions
#'
#' @param modFilePath The model file Path. No default.
#' @param runType The type of run to be executed. This string is used
#'        in the folder name cerated. Default is an empty string.
#' @param subDirs additional subdirectories to create under the main one.
#'        Default is an empty vector.
#'
#' @author Henrik Bjugård Nyberg - henrik.b.nyberg@@farmbio.uu.se

fileSysSetup <- function(modFilePath, runType = "", subDirs = c()){

  # Get the mod file name without extension for various uses
  modFileNameNoExt <- sub("\\.[[:alnum:]]+$", "", basename(as.character(modFilePath)))

  # Create a directory to do everything in
  dirName <- paste0(modFileNameNoExt, "_", runType, "_", format(Sys.time(), "%y%m%d_%H%M%S"))
  dir.create(dirName)

  # Parse the data file name from the mod.
  dataFileName <- parseNMDataFileName(modFilePath)

  # Put together a vector of file names to copy
  filePattern <- paste0(modFileNameNoExt, "\\.")
  filesToCopy <- list.files(pattern = eval(filePattern))
  filesToCopy <- c(filesToCopy, dataFileName)

  #Copy all the relevant files to the new dir
  file.copy(filesToCopy, dirName)

  # Set working directory to the new folder
  setwd(dirName)

  # Create all the requested subfolders and
  # copy into the subdirectories as well
  sapply(subDirs, function(x){
    dir.create(x)
    file.copy(filesToCopy, x)
  })

  return(modFileNameNoExt)
}
