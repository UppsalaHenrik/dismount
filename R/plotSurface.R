#' plotSurface
#' 
#' Plots a surface using Plotly and returns the URL
#' 
#' @param plotlyAccount A username and key for Plotly access on the form plotly(username='user', key='key').
#' @param modFilePath Model file to use. The called function createRawresInput assumes that there is an ext file with the same base file name.
#' @param resol Resolution on each axis. Default is 10 and will use 10^2 = 100 sets of parameter values, NONMEM runs, and ofv values to create the plot.
#' @param ... Further options to createRawresInput
#' 
#' @export


plotSurface <- function(plotlyAccount, modFilePath, paramsToCompare = c("THETA1", "THETA2"), 
                        resol = 10, local = FALSE, ...){
  
  require(plotly)
  
  print("Creating the rawres input file")
  rawresInputList <- createRawresInput(modFilePath, paramsToCompare, resol = 10, ...)
  
  print("Running Parallel retries")
  dirName <- runParaRetries(modFilePath, rawres_input = rawresInputList[[1]], clean = 3, local = local)
  
  print("Parsing OFVs")
  ofvVector <- parseRawresOfvs(rawresPath)
  
  print("Creating Plotly plot")
  url <- createPlotlyObj(ofvVector, rawresInputList[[2]], rawresInputList[[3]])
  
  return(url)
}