#' plotSurface
#' 
#' Plots a surface using Plotly and returns the URL
#' 
#' @param plotlyUsername Plotly online user name. Not needed if environment variables already set. See plotly instructions.
#' @param plotlyKey Plotly online key. Not needed if environment variables already set. See plotly instructions.
#' @param modFilePath Model file to use. The called function createRawresInput assumes that there is an ext file with the same base file name.
#' @param paramsToCompare Parameters to compare. A vector of two parameter names following the NONMEM ext file standard names. Default is c("THETA1", "THETA2"). Model file parameter labels will be removed.
#' @param resol Resolution on each axis. Default is 10 and will use 10^2 = 100 sets of parameter values, NONMEM runs, and ofv values to create the plot.
#' @param ofvScaling If true OFVs are scaled to between zero and one. Default is FALSE.
#' @param cleanLevel PsN clean script will be run on the parallel retries folder. See psn_clean documentation. Default is 4, the highest cleaning level.
#' @param origVals Whether or not to plot the original model final estimate as a point in the plot.
#' @param ... Further options to createRawresInput
#' 
#' @export


plotSurface <- function(plotlyUsername, plotlyKey, modFilePath, 
                        paramsToCompare = c("Param1", "Param2"), 
                        resol = 10, ofvScaling = FALSE, 
                        slurm_partition = "standard", cleanLevel = 4, 
                        plotOrigVals = FALSE, ...){
  
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
                            slurm_partition = slurm_partition)
  
  print("Parsing OFVs")
  rawresPath <- findRawres(dirName)
  rawres <- parseRawres(rawresPath, cols = c(paramsToCompare, "ofv"), skipRows = 1)

  xParamValsInput <- rawresInputList[[2]]
  xParamValsOutput <- order(unique(rawres[[paramsToCompare[1]]]))
  
  yParamValsInput <- rawresInputList[[3]]
  yParamValsOutput <- order(unique(rawres[[paramsToCompare[2]]]))
  
  
  # Checking that input and output parameter values are the same (NONMEM does change them sometimes)
  sapply(seq_along(xParamValsInput), function(x){
    
    if(!identical(xParamValsInput[x], xParamValsOutput[x])){
      paramMessage <- paste("Input and output values are different:\n", 
                            "Input ", x, ":", xParamValsInput[x],
                            "Output ", x, ":", xParamValsOuput[x],
                            "Using output values") 
      print(paramMessage)
    }
  })
  sapply(seq_along(yParamValsInput), function(x){
    
    if(!identical(yParamValsInput[x], yParamValsOutput[x])){
      paramMessage <- paste("Input and output values are different:\n", 
                            "Input ", x, ":", yParamValsInput[x],
                            "Output ", x, ":", yParamValsOuput[x],
                            "Using output values") 
      print(paramMessage)
    }
  })
  
  plotTitle <- paste0("\n<b>OFV Surface for ", modFilePath, "</b><br>", resol, "x", resol, 
                      "resolution. Retries folder ", dirName)
  
  print("Creating Plotly plot")
  plotlyObj <- createPlotlyObj(ofvVector = rawres[["ofv"]], 
                               xParamVals = xParamValsOutput, 
                               yParamVals = yParamValsOutput, 
                               origVals = rawresInputList[[4]],
                               plotOrigVals = plotOrigVals,
                               paramsToCompare = paramsToCompare,
                               ofvScaling = ofvScaling,
                               plotTitle = plotTitle)
  
  plotly_POST(plotlyObj, fileopt = "new")

  # Clean up using the psn_clean 
  print(paste("Cleaning up", dirName, "with level =", cleanLevel))
  runPsnClean(dirName, level = cleanLevel, interact = FALSE)
  
  return(list(plotlyObj, dirName))
}