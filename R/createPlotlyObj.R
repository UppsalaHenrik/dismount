#' createPlotlyObj
#' 
#' @param ofvVector Vector of OFV values for the plot.
#' @param xParamVals Vector of X axis values.
#' @param yParamVals Vector of Y axis values.
#' @param paramsToCompare A vector of two parameters to plot against.
#' @param ofvScaling If true OFVs are scaled to between zero and one. Default is FALSE.
#' @param zlab Label for Z axis.
#' @param plotTitle Plot title.
#' 
#' @export
#' 



createPlotlyObj <- function(ofvVector, xParamVals, yParamVals, origVals,
                            paramsToCompare = c("Param1", "Param2"), 
                            ofvScaling = FALSE,
                            zlab = "Z: OFV", 
                            plotTitle = "OFV Surface"){
  
  xSide <- length(xParamVals)
  xlab <- paste("X:", paramsToCompare[1])
  
  ySide <- length(yParamVals)
  ylab <- paste("Y:", paramsToCompare[2])
  
  origOfv <- origVals[[3]]
  
  labelVector <- paste("OFV =", format(ofvVector, digits = 2))
  
  if(ofvScaling){
    
    # Subtract smallest number    
    ofvVector <- ofvVector - min(ofvVector)
    origOfv <- origOfv - min(ofvVector)
    
    # Calculate a scaling factor that brings the smallest non-zero number up
    ofvScalingFactor <- 10^abs(min(log10(ofvVector[ofvVector > 0])))
    
    # Scale it so that the numbers become easier to handle
    ofvVector <- ofvVector * ofvScalingFactor
    origOfv <- origOfv * ofvScalingFactor
    
    # Scale it to a fraction of the maximum
    ofvVector <- ofvVector/max(ofvVector)
    origOfv <- origOfv/max(ofvVector)
    
    # Change the z axis label
    zlab = paste0(zlab, ", scaled")
  }
  
  
  ofvMatrix <- matrix(ofvVector, 
                      nrow = ySide, 
                      ncol = xSide, 
                      byrow = TRUE)
  labelMatrix <- matrix(labelVector, 
                        nrow = ySide, 
                        ncol = xSide, 
                        byrow = TRUE)
  
  p <- plot_ly(x = xParamVals,
               y = yParamVals,
               z = ofvMatrix, 
               type = "surface",
               text = labelMatrix
  ) %>%
    add_trace(x = origVals[[1]], 
              y = origVals[[2]], 
              z = origOfv, 
              type = "scatter3d", 
              marker = list(opacity = 0.65,
                            color = "#FF0000",
                            symbol = "x"
              )
    ) %>%
    layout(title = plotTitle,
           scene = list(
             xaxis = list(title = xlab),
             yaxis = list(title = ylab),
             zaxis = list(title = zlab)
           )
    )
  
  return(p)
}
