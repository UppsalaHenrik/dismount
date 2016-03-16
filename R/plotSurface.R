#' plotSurface
#' 
#' Plots a surface using Plotly and returns the URL
#' 
#' @param plotlyUsername Plotly online user name
#' @param plotlyKey Plotly online key.
#' @param modFilePath Model file to use. The called function createRawresInput assumes that there is an ext file with the same base file name.
#' @param paramsToCompare Parameters to compare. A vector of two parameter names following the NONMEM ext file standard names. Default is c("THETA1", "THETA2"). Model file parameter labels will be removed.
#' @param resol Resolution on each axis. Default is 10 and will use 10^2 = 100 sets of parameter values, NONMEM runs, and ofv values to create the plot.
#' @param ofvScaling If true OFVs are scaled to between zero and one. Default is FALSE.
#' @param ... Further options to createRawresInput
#' 
#' @export


plotSurface <- function(plotlyUsername, plotlyKey, modFilePath, paramsToCompare = c("Param1", "Param2"), 
                        resol = 10, local = FALSE, ofvScaling = FALSE,...){
  
  require(plotly)
  Sys.setenv("plotly_username" = plotlyUsername)
  Sys.setenv("plotly_api_key" = plotlyKey)
  
  print(paste("Preparing model file", modFilePath, "by removing commented out code and setting MAXEVALS"))
  modFileOrig <- readLines(modFilePath)
  modFile <- setMaxEvals(modFileOrig, 0) 
  modFile <- gsub("[[:space:]];.+", "", modFile)
  newModFileName <- paste0("new_", basename(modFilePath))
  writeLines(modFile, newModFileName)
    
  print("Creating the rawres input file")
  rawresInputList <- createRawresInput(modFilePath = modFilePath, paramsToCompare = paramsToCompare, resol = resol, ...)
  
  print("Running Parallel retries")
  dirName <- runParaRetries(newModFileName, rawres_input = rawresInputList[[1]], clean = 3, local = local)
  
  print("Parsing OFVs")
  rawresPath <- findRawres(dirName)
  ofvVector <- parseRawresOfvs(rawresPath)
  
  if(ofvScaling){

    # Subtract smallest number    
    ofvVector <- ofvVector - min(ofvVector)
    
    # Calculate a scaling factor that brings the smallest non-zero number up
    ofvScalingFactor <- 10^abs(min(log10(ofvVector[ofvVector > 0])))
    
    # Scale it so that the numbers become easier to handle
    ofvVector <- ofvVector * ofvScalingFactor
    
    # Scale it to a fraction of the maximum
    ofvVector <- ofvVector/max(ofvVector)
  }
  
  print("Creating Plotly plot")
  url <- createPlotlyObj(plotlyAccount, ofvVector, xParamVals = rawresInputList[[2]], 
                         yParamVals = rawresInputList[[3]], paramsToCompare = paramsToCompare)
  
  return(url)
}