

createRawresInput <- function(paramValue1, paramValue2, paramName1, paramName2,
                               lims1, lims2, resol, fixedParams){
  
  # Throw an error if the param value is not within the limits
  if(paramValue1 <= lims1[1] || paramValue1 >= lims1[2]){
    
    print("Given parameter 1 value is not within the given limits")
    
    return(NULL)
  }

  if(paramValue2 <= lims2[1] || paramValue2 >= lims2[2]){
    
    print("Given parameter 2 value is not within the given limits")
    
    return(NULL)
  }
  
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