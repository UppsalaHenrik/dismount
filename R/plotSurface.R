#' plotSurface
#' 
#' Plots a surface using Plotly and returns the URL
#' 
#' @param plotlyAccount A username and key for Plotly access on the form plotly(username='user', key='key').
#' @param modFilePath Model file to use. The called function createRawresInput assumes that there is an ext file with the same base file name.
#' @param paramsToCompare Parameters to compare. A vector of two parameter names following the NONMEM ext file standard names. Default is c("THETA1", "THETA2"). Model file parameter labels will be removed.
#' @param resol Resolution on each axis. Default is 10 and will use 10^2 = 100 sets of parameter values, NONMEM runs, and ofv values to create the plot.
#' @param ... Further options to createRawresInput
#' 
#' @export


plotSurface <- function(plotlyAccount, modFilePath, paramsToCompare = c("THETA1", "THETA2"), 
                        resol = 10, local = FALSE, ...){
  
  require(plotly)
  
  print(paste("Preparing model file", modFilePath, "by removing commented out code and setting MAXEVALS"))
  modFileOrig <- readLines(modFilePath)
  modFile <- setMaxEvals(modFileOrig, 0) 
  modFile <- gsub(";.+", "", modFile)
  newModFileName <- paste0("new_", basename(modFilePath))
  writeLines(modFile, newModFileName)
    
  print("Creating the rawres input file")
  rawresInputList <- createRawresInput(modFilePath = modFilePath, paramsToCompare = paramsToCompare, resol = resol, ...)
  
  print("Running Parallel retries")
  dirName <- runParaRetries(newModFileName, rawres_input = rawresInputList[[1]], clean = 3, local = local)
  
  print("Parsing OFVs")
  rawresPath <- findRawres(dirName)
  ofvVector <- parseRawresOfvs(rawresPath)
  
  print("Creating Plotly plot")
  url <- createPlotlyObj(ofvVector, rawresInputList[[2]], rawresInputList[[3]])
  
  return(url)
}