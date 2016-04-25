#' createRawresInput
#' 
#' Creates a csv file with formatting compatible with raw results files from PsN.
#' 
#' @param modFilePath Model file to use. The function assumes that there is an ext file with the same base file name.
#' @param paramsToCompare A vector of two parameter names following the NONMEM ext file standard names. Default is c("THETA1", "THETA2").
#' @param lims1 A vector of two values, lower and upper limit for the first paramsToCompare. Default is 1 percent below and above the original value
#' @param lims2 A vector of two values, lower and upper limit for the second paramsToCompare. Default is 1 percent below and above the original value
#' @param absolute Whether or not to take the absolute value of all parameter values before constructing the input file. Default is FALSE.
#' @param resol Resolution on each axis. Default is 10 and will return 10^2 = 100 sets of parameter values.
#' 
#' 
#' @export

createRawresInput <- function(modFilePath, paramsToCompare = c("THETA1", "THETA2"), 
                              lims1 = c(0.99*as.numeric(paramVector[paramsToCompare[1]]),
                                        1.01*as.numeric(paramVector[paramsToCompare[1]])), 
                              lims2 = c(0.99*as.numeric(paramVector[paramsToCompare[2]]),
                                        1.01*as.numeric(paramVector[paramsToCompare[2]])), 
                              absolute = TRUE, resol = 10){
  
  
  # This is dependent on there not being a FILE option set on the $EST. Not great but fine for now.
  modFileNameNoExt <- sub("\\.[[:alnum:]]+$", "", basename(as.character(modFilePath)))
  extFileName <- paste0(modFileNameNoExt, ".ext")
  
  if(!file.exists(extFileName)){
    
    print("No .ext file found for this model. Please run it first")
    
    break()
  }
  
  extFileDFList <- parseExtFile(extFileName)
  
  # Get the last table in the ext file
  extFileDF <- extFileDFList[[length(extFileDFList)]]
  
  # Pick out the final parameter values row
  paramVectorRow <- subset(extFileDF, ITERATION == -1e+9)
  
  # Pick out the OBJ values and package it with the original values of paramsToCompare
  origParamsAndOFV <- cbind(paramVectorRow[paramsToCompare], 
                            paramVectorRow[length(paramVectorRow)])
  
  # Ignoring the first and last column (Iteration and OBJ)
  paramVectorFull <- paramVectorRow[2:(length(paramVectorRow)-1)]
  
  # Take absolute if option is set
  if(absolute){
    paramVectorFull <- abs(paramVectorFull)
    lims1 <- sort(abs(lims1))
    lims2 <- sort(abs(lims2))
  }
  
  # Getting the columns for the different parameter types so that I can reorder and remove unnecessary ones
  thetaCols <- grep("THETA", names(paramVectorFull))
  omegaCols <- grep("OMEGA", names(paramVectorFull))
  sigmaCols <- grep("SIGMA", names(paramVectorFull))
  
  # Reordering to fit PsN standard with SIGMA last.
  paramVectorFull <- paramVectorFull[c(thetaCols, omegaCols, sigmaCols)]
  
  ### Getting rid of zero value off-diagonal elements. This is required for PsN rawres_input
  # First some more or less dodgy regex
  indices <- gsub("SIGMA", "", names(paramVectorFull))
  indices <- gsub("OMEGA", "", indices) 
  indices <- gsub("\\(", "", indices)
  indices <- gsub("\\)", "", indices)
  # Spliting the two numbers 
  indicesList <- strsplit(indices, ",")
  
  
  # Checking if they are off-diagonal sigmas/omegas 
  paramVectorOffDiags <- unlist(sapply(indicesList, function(x){
    # If the indices do not match and the value isn't NA (catches THETAs) then it is an 
    # off/diagonal sigma or omega and I set TRUE
    y <- ifelse(x[1] != x[2] && !is.na(x[2]), TRUE, FALSE)
    return(y)
    
  }), recursive = FALSE)
  
  # Put the names removed above back
  
  
  # Check if they are also zero. I seem to be messing up the names here. I'm reducing 
  # this to a vector so that naming in the next step is taken from paramVectorFull
  paramVectorOffDiagZeroes <- c(paramVectorOffDiags & paramVectorFull == 0)
  
  # Finally we have the PsN compatible
  paramVector <- paramVectorFull[!paramVectorOffDiagZeroes]
  
  
  
  # Picking out the relevant parameters
  paramValue1 <- paramVector[paramsToCompare[1]]
  paramValue2 <- paramVector[paramsToCompare[2]]
  paramValsList <- list()
  
  # Check if the original data is within the limits and create paramValsList accordingly
  
  if(paramValue1[[1]] > lims1[1] && paramValue1[[1]] < lims1[2]){
    
    # Determine how far along the vector the paramValue should be for param 1 (X)
    frac1 <- round((paramValue1[[1]]-lims1[1])/(lims1[2]-lims1[1]), 
                   digits = ceiling(log10(resol)))
    
    lowSeq1 <- seq(from = lims1[1], to = paramValue1[[1]], 
                   length.out = round(frac1*resol))
    
    uppSeq1 <- seq(from = paramValue1[[1]], to = lims1[2], 
                   length.out = 1 + resol - round(frac1*resol))[-1]
    
    paramValsList[[paramsToCompare[1]]] <- c(lowSeq1, uppSeq1)
    
  }else{
    
    paramValsList[[paramsToCompare[1]]] <- seq(from = lims1[1], 
                                               to = lims1[2],
                                               length.out = resol)
    
  }
  
  # Same for parameter 2 (Y)
  
  if(paramValue2[[1]] > lims2[1] && paramValue2[[1]] < lims2[2]){
    
    frac2 <- round((paramValue2[[1]]-lims2[1])/(lims2[2]-lims2[1]), 
                   digits = ceiling(log10(resol)))
    
    lowSeq2 <- seq(from = lims2[1], to = paramValue2[[1]], 
                   length.out = round(frac2*resol))
    
    uppSeq2 <- seq(from = paramValue2[[1]], to = lims2[2], 
                   length.out = 1 + resol - round(frac2*resol))[-1]
    
    paramValsList[[paramsToCompare[2]]] <- c(lowSeq2, uppSeq2)
    
  }else{
    
    paramValsList[[paramsToCompare[2]]] <- seq(from = lims2[1], 
                                               to = lims2[2],
                                               length.out = resol)
    
  }
  
  
  # I need a "model" column with a sequence of numbers numbers. 
  # PsN will use the first row so I start from 2.
  modelCol <- c(2:(resol^2+1))
  
  # Fixed parameters are set to the same value for every row.
  paramCols <- data.frame(sapply(paramVector, function(x){
    
    rep(x, resol^2)
    
  }), check.names = FALSE)
  
  # I create a data frame with all combinations of the paramVals1 and 2 
  paramValsDF <- do.call(expand.grid, paramValsList)
  
  # I replace the two columns for the parameters I want to compare with the 
  paramCols[paramsToCompare] <- paramValsDF
  
  # I add a placeholder first row that PsN will overwrite
  firstRow <- rep(1, length(paramCols)+1)
  
  rawresInput <- cbind(modelCol, paramCols)
  rawresInput <- rbind(firstRow, rawresInput)
  
  names(rawresInput) <- c("model", names(paramVector))
  
  fileName <- paste0("rawresInput_", paste(gsub("[[:punct:]]", "", paramsToCompare), collapse = "_"), 
                     "_", resol^2, "values_", format(Sys.time(), "%y%m%d_%H%M%S"), ".csv")
  
  write.csv(rawresInput, fileName, row.names=FALSE)
  
  # Check to make sure the output file exists
  if (!file.exists(fileName)) {
    stop("Failed to create rawres input file; expected file \"", 
         fileName, "\" does not exist.")
  }
  
  # I return both the fileName of the CSV and the value vectors.
  return(list(fileName, paramValsList[[paramsToCompare[1]]], 
              paramValsList[[paramsToCompare[2]]], origParamsAndOFV))
}