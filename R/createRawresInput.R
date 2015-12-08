#' createRawresInput
#' 
#' Creates a csv file with formatting compatible with raw results files from PsN.
#' 
#' @param modFilePath Model file to use. The function assumes that there is an ext file with the same base file name.
#' @param paramsToCompare A vector of two parameter names following the NONMEM ext file standard names. Default is c("THETA1", "THETA2").
#' @param lims1 A vector of two values, lower and upper limit for the first paramsToCompare. Default is 1% below and above the original value
#' @param lims2 A vector of two values, lower and upper limit for the second paramsToCompare. Default is 1% below and above the original value
#' @param resol Resolution on each axis. Default is 50 and will return 50^2 = 2500 sets of parameter values.
#' 
#' 
#' @export

createRawresInput <- function(modFilePath, paramsToCompare = c("THETA1", "THETA2"), 
                              lims1 = c(0.99*as.numeric(paramVector[paramsToCompare[1]]),
                                        1.01*as.numeric(paramVector[paramsToCompare[1]])), 
                              lims2 = c(0.99*as.numeric(paramVector[paramsToCompare[2]]),
                                        1.01*as.numeric(paramVector[paramsToCompare[2]])), 
                              resol = 50){
  
  
  # This is dependent on there not being a FILE option set on the $EST. Not great but fine for now.
  modFileNameNoExt <- sub("\\.[[:alnum:]]+$", "", basename(as.character(modFilePath)))
  extFileName <- paste0(modFileNameNoExt, ".ext")
  extFileDF <- parseExtFile(extFileName)
  
  # Pick out the final parameter values row
  paramVectorRow <- subset(extFileDF, ITERATION == -1e+9)
  
  # Ignoring the first and last column (Iteration and OBJ)
  paramVector <- paramVectorRow[2:(length(paramVectorRow)-1)]
  
  # Reordering to fit PsN standard with SIGMA last.
  paramVector <- paramVector[c(1:(sigmaCols-1), (sigmaCols[length(sigmaCols)]+1):length(paramVector), sigmaCols)]

  # Picking out the relevant parameters
  paramValue1 <- paramVector[paramsToCompare[1]]
  paramValue2 <- paramVector[paramsToCompare[2]]
  
  # Determine how far along the vector the paramValue should be for param 1 (X)
  frac1 <- round((paramValue1[[1]]-lims1[1])/(lims1[2]-lims1[1]), 
                 digits = ceiling(log10(resol)))
  
  lowSeq1 <- seq(from = lims1[1], to = paramValue1[[1]], 
                 length.out = round(frac1*resol))
  
  uppSeq1 <- seq(from = paramValue1[[1]], to = lims1[2], 
                 length.out = 1 + resol - round(frac1*resol))[-1]
  
  paramVals1 <- c(lowSeq1, uppSeq1)
  
  # Same for parameter 2 (Y)
  frac2 <- round((paramValue2[[1]]-lims2[1])/(lims2[2]-lims2[1]), 
                 digits = ceiling(log10(resol)))
  
  lowSeq2 <- seq(from = lims2[1], to = paramValue2[[1]], 
                 length.out = round(frac2*resol))
  
  uppSeq2 <- seq(from = paramValue2[[1]], to = lims2[2], 
                 length.out = 1 + resol - round(frac2*resol))[-1]
  
  paramVals2 <- c(lowSeq2, uppSeq2)
  
  # I need a "model" column with a sequence of numbers numbers. 
  # PsN will use the first row so I start from 2.
  modelCol <- c(2:(resol^2+1))
  
  # Fixed parameters are set to the same value for every row.
  paramCols <- data.frame(sapply(paramVector, function(x){
    
    rep(x, resol^2)
    
  }), check.names = FALSE)
  
  # I create a data frame with all combinations of the paramVals1 and 2 
  paramValsDF <- expand.grid(paramVals1, paramVals2)
  
  # I replace the two columns for the parameters I want to compare with the 
  paramCols[paramsToCompare] <- paramValsDF
  
  # I add a placeholder first row that PsN will overwrite
  firstRow <- rep(1, length(paramCols))
  
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
  return(list(fileName, paramVals1, paramVals2))
}