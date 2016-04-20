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
#' @param cleanLevel PsN clean script will be run on the parallel retries folder. See psn_clean documentation. Default is 4, the highest cleaning level.
#' @param ... Further options to createRawresInput
#' 
#' @export


plotSurface <- function(plotlyUsername, plotlyKey, modFilePath, 
                        paramsToCompare = c("Param1", "Param2"), 
                        resol = 10, local = FALSE, ofvScaling = FALSE, 
                        slurm_partition = "standard",
                        cleanLevel = 4, ...){
  
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
  dirName <- runParaRetries(newModFileName, rawres_input = rawresInputList[[1]], clean = 3, 
                            slurm_partition = slurm_partition, local = local)
  
  print("Parsing OFVs")
  rawresPath <- findRawres(dirName)
  ofvVector <- parseRawresOfvs(rawresPath)
  
  print("Creating Plotly plot")
  plotlyObj <- createPlotlyObj(ofvVector, xParamVals = rawresInputList[[2]], 
                         yParamVals = rawresInputList[[3]], 
                         origVals = rawresInputList[[4]],
                         paramsToCompare = paramsToCompare,
                         ofvScaling = ofvScaling,
                         plotTitle = paste("OFV surface for para retries run\\n", dirName))
  
  plotly_POST(plotlyObj, fileopt = "new")
  
  write(url, file = paste0(dirName, "_URL", ".txt"))
  
  # Clean up using the psn_clean 
  
  runPsnClean(dirName, level = cleanLevel, interact = FALSE)
  
  return(list(url, dirName))
}