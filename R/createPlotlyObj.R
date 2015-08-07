
createPlotlyObj <- function(ofvVector, paramVals1, paramVals2){
  
  xSide <- length(paramVals1)
  
  ySide <- length(paramVals2)
  
  zMatrix <- matrix(ofvVector, nrow = ySide, ncol = xSide, byrow = TRUE)
  
  plotlyTrace <- list(z = zMatrix, 
                      x = paramVals1, 
                      y = paramVals2,
                      colorscale = list(c(0, "rgb(255,0,0)"),
                                        list(0.01, "rgb(200,0,0)"),
                                        list(0.02, "rgb(150,0,0)"),
                                        list(0.03, "rgb(100,0,0)"),
                                        list(0.05, "rgb(50,0,0)"), 
                                        list(0.20, "rgb(255,255,255)"),
                                        list(0.75, "rgb(255,255,255)"),
                                        list(1, "rgb(255,255,255)")),
                      type = "surface"
  )
  
  ofvRange <- c(min(ofvVector), max(ofvVector))
  
  layout <- list(
    title = "OFV Surface", 
    titlefont = list(
      family = "'Open sans', verdana, arial, sans-serif", 
      size = 17, 
      color = "#444"
    ), 
    font = list(
      family = "'Open sans', verdana, arial, sans-serif", 
      size = 12, 
      color = "#444"
    ), 
    showlegend = TRUE, 
    autosize = TRUE, 
    width = 657, 
    height = 549, 
    legend = list(
      x = 0.98, 
      y = 0.5, 
      traceorder = "normal", 
      font = list(
        family = "'Open sans', verdana, arial, sans-serif", 
        size = 12, 
        color = "#444"
      ), 
      bgcolor = "rgb(255,255,255)", 
      bordercolor = "transparent", 
      borderwidth = 0, 
      xanchor = "center", 
      yanchor = "auto"
    ), 
    margin = list(
      l = 0, 
      r = 0, 
      b = 0, 
      t = 80, 
      pad = 0, 
      autoexpand = TRUE
    ), 
    paper_bgcolor = "rgb(255,255,255)", 
    plot_bgcolor = "rgb(229,229,229)", 
    hovermode = "z", 
    dragmode = "turntable", 
    separators = ".,", 
    scene = list(
      xaxis = list(
        title = "X: Theta 1 - V1", 
        titlefont = list(
          family = "'Open sans', verdana, arial, sans-serif", 
          size = 14, 
          color = "rgb(102, 102, 102)"
        ), 
        type = "linear", 
        rangemode = "normal", 
        autorange = TRUE, 
        showgrid = TRUE, 
        zeroline = TRUE, 
        showline = FALSE, 
        autotick = TRUE, 
        nticks = 0, 
        ticks = "", 
        showticklabels = TRUE, 
        tick0 = 0, 
        dtick = 20, 
        tickangle = "auto", 
        tickfont = list(
          family = "'Open sans', verdana, arial, sans-serif", 
          size = 12, 
          color = "rgb(102, 102, 102)"
        ), 
        exponentformat = "B", 
        showexponent = "all", 
        gridcolor = "rgb(255, 255, 255)", 
        gridwidth = 2, 
        zerolinecolor = "#444", 
        zerolinewidth = 1, 
        showbackground = FALSE, 
        showspikes = TRUE, 
        spikesides = TRUE, 
        spikethickness = 2
      ), 
      yaxis = list(
        title = "Y: Theta 2 - Q", 
        titlefont = list(
          family = "'Open sans', verdana, arial, sans-serif", 
          size = 14, 
          color = "rgb(102, 102, 102)"
        ), 
        type = "linear", 
        rangemode = "normal", 
        autorange = TRUE, 
        showgrid = TRUE, 
        zeroline = TRUE, 
        showline = FALSE, 
        autotick = TRUE, 
        nticks = 0, 
        ticks = "", 
        showticklabels = TRUE, 
        tick0 = 0, 
        dtick = 20, 
        tickangle = "auto", 
        tickfont = list(
          family = "'Open sans', verdana, arial, sans-serif", 
          size = 12, 
          color = "rgb(102, 102, 102)"
        ), 
        exponentformat = "B", 
        showexponent = "all", 
        gridcolor = "rgb(255, 255, 255)", 
        gridwidth = 2, 
        zerolinecolor = "#444", 
        zerolinewidth = 1, 
        showbackground = FALSE, 
        showspikes = TRUE, 
        spikesides = TRUE, 
        spikethickness = 2
      ), 
      zaxis = list(
        title = "Z: OFV", 
        titlefont = list(
          family = "'Open sans', verdana, arial, sans-serif", 
          size = 14, 
          color = "rgb(102, 102, 102)"
        ), 
        range = ofvRange,
        type = "linear", 
        rangemode = "normal", 
        autorange = FALSE, 
        showgrid = TRUE, 
        zeroline = TRUE, 
        showline = FALSE, 
        autotick = TRUE, 
        nticks = 0, 
        ticks = "", 
        showticklabels = TRUE, 
        tick0 = 0, 
        dtick = 500, 
        tickangle = "auto", 
        tickfont = list(
          family = "'Open sans', verdana, arial, sans-serif", 
          size = 12, 
          color = "rgb(102, 102, 102)"
        ), 
        exponentformat = "B", 
        showexponent = "all", 
        gridcolor = "rgb(255, 255, 255)", 
        gridwidth = 2, 
        zerolinecolor = "#444", 
        zerolinewidth = 1, 
        showbackground = FALSE, 
        showspikes = TRUE, 
        spikesides = TRUE, 
        spikethickness = 2
      ), 
      cameraposition = list(
        c(-0.62049958136, -0.195799695764, -0.19172102932, -0.734769212465), c(-0.0345517705009, 0.0540832558181, -0.0687389292161), 1.825423821), 
      bgcolor = "rgba(238, 238, 238, 0.2)"
    ), 
    hidesources = FALSE
  )
  
  data <- list(plotlyTrace)
  
  plotlyObj <- py$plotly(data, kwargs=list(layout=layout))
  
  return(plotlyObj$url)
}
