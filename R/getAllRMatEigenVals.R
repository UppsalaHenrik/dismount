
getAllRMatEigenVals <- function(filePath = "."){
  
  # List all the rmt files in the location
  rMatFiles <- list.files(filePath, pattern = ".rmt$")
  
  maxEigenCols <- 0
  
  # Apply parseNMMat over all the rmts and get the eigen values.
  eigenValRows <- lapply(rMatFiles, function(x){
    
    mat <- parseNMMat(x)
    
    # TODO: parse the EXT file and determine what parameters are 
    # fixed from the value of first and last iteration
    extFileName <- gsub("\\.rmt", "\\.ext", x)
    
    # Parse EXT file
    extFile <- parseExtFile(extFileName)
    
    # Get the first and last iterations
    firstIter <- subset(extFile[[1]], ITERATION == 0)
    lastIter <- subset(extFile[[1]], ITERATION == -1.0e+09)
    # Compare and get the names for fixed/unused parameters
    fixParams <- names(firstIter)[firstIter == lastIter]
    
    # Get the OFV just for fun
    finalOfv <- lastIter["OBJ"]
    
    # Remove them before decomposition
    fixParamRowsCols <- match(fixParams,names(mat[,1]))
    
    nonFixMat <- mat[-fixParamRowsCols,-fixParamRowsCols]
  
    eigenVals <- eigen(nonFixMat, only.values = TRUE)$values

    # Need to check how close to zero they are...
    zeroEigenVals <- any(abs(eigenVals) <= 10^-10)
    negEigenVals <- any(eigenVals < -10^-10)
    
    # Put them in a vector and return. This makes the numbers characters instead, not great.
    return(data.frame(x, zeroEigenVals, negEigenVals, finalOfv, rbind(eigenVals)))
  })
  
  # Bind the rows together, this means only models 
  eigenValDF <- rbind.fill(eigenValRows)
  
  return(eigenValDF)
  
}

#eigenValsDF <- getAllRMatEigenVals()
#write.csv(eigenValsDF, "eigenValsDF3.csv")
