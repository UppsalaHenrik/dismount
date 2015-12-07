

createRawresInputRNMImport <- function(nmRun, paramsToCompare, resol){
  
  # Use RNMImport to get parameter
  runThetas <- getThetas(nmRun)
  names(runThetas) <- paste0("THETA", 1:length(runThetas))
  runOmegas <- getOmegas(nmRun)
  runSigmas <- getSigmas(nmRun)
  
  # Create a vector of parameters in the right order
  # This involves picking out the matrix 
  
  # Make two matrices with indices that I can pull out and create Omega names from...
  
  
  
  runOmegaVector <- getOmegas(nmRun)[upper.tri(getOmegas(nmRun), diag = TRUE)]
  names(runOmegaVector) <- paste0("OM", 1:length(runOmegaVector))
  runSigmaVector <- getSigmas(nmRun)[upper.tri(getSigmas(nmRun), diag = TRUE)]
  paramVector <- c(runThetas, runOmegaVector, runSigmaVector)

  # Creating a vector of parameter values. In order to include the exact point 
  # this is done in two steps. One under the value and one over.
  
  # Determine how far along the vector the paramValue should be for param 1 (X)
  frac1 <- round((paramValue1-lims1[1])/(lims1[2]-lims1[1]), 
                 digits = ceiling(log10(resol)))
  
  lowSeq1 <- seq(from = lims1[1], to = paramValue1, 
                 length.out = round(frac1*resol))
  
  uppSeq1 <- seq(from = paramValue1, to = lims1[2], 
                 length.out = 1 + resol - round(frac1*resol))[-1]
  
  paramVals1 <- c(lowSeq1, uppSeq1)
  
  # Same for parameter 2 (Y)
  frac2 <- round((paramValue2-lims2[1])/(lims2[2]-lims2[1]), 
                 digits = ceiling(log10(resol)))
  
  lowSeq2 <- seq(from = lims2[1], to = paramValue2, 
                 length.out = round(frac2*resol))
  
  uppSeq2 <- seq(from = paramValue2, to = lims2[2], 
                 length.out = 1 + resol - round(frac2*resol))[-1]
  
  paramVals2 <- c(lowSeq2, uppSeq2)
  
  # I need a "model" column with a sequence of numbers numbers. 
  # PsN will add the first row so I start from 2.
  modelCol <- c(2:(resol^2+1))
  
  # Fixed parameters are set to the same value for every row.
  fixedParamCols <- data.frame(sapply(fixedParams, function(x){
    
    rep(x, resol^2)
    
  }))
  
  # I create a data frame with all combinations of the paramVals1 and 2 
  paramValsDF <- expand.grid(paramVals1, paramVals2)
  
  # Put the columns in a list and cbind them together
  colList <- list(modelCol, paramValsDF, fixedParamCols)
  
  rawresInput <- do.call("cbind", colList)
  
  # I add a placeholder first row that PsN will overwrite
  firstRow <- rep(1, length(rawresInput))
  
  rawresInput <- rbind(firstRow, rawresInput)
  
  names(rawresInput) <- c("model", paramName1, paramName2, names(fixedParamCols))
  
  fileName <- paste("rawresInput", paramName1, paramName2, resol^2, 
                    "values", format(Sys.time(), "%y%m%d_%H%M%S"), ".csv", sep="_")
  
  write.csv(rawresInput, fileName, row.names=FALSE)
  
  # I return both the fileName of the CSV and the value vectors.
  return(list(fileName, paramVals1, paramVals2))
}














# 
# 
# 
# NMParam <- setClass(
#   "NMParam",
#   representation(
#     name = "character",
#     value = "numeric",
#     lowerBound = "numeric",
#     upBound = "numeric",
#     fixed = "logical", 
#     type = "character"
#   )
# )
# 
# lala <- NMParam(name = "lala",
#                 value = 1,
#                 fixed = FALSE,
#                 type = "theta")  
# class(lala)
# 
# lala@name <- "param1"