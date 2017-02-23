#' @export

createPercentageTable <- function(wd = getwd(), compParaRetries = FALSE, 
                                  checkMinSuccess = FALSE){
  
  userWd <- getwd()
  
  setwd(wd)
  
  # For each model I combine the ofvs from the different settings into a data frame
  
  # First the initial parallel retries
  paraRetriesCsvFileName <- "paraRetriesOfvs.csv"
  paraRetriesCsv <- read.csv(paraRetriesCsvFileName, header = TRUE)
  paraRetriesOfvs <- paraRetriesCsv$paraRetriesOfv
  paraRetriesRetry <- paraRetriesCsv$retry
  
  # Check for compParaRetriesOfvs.csv and force compParaRetries to FALSE if not found
  compParaRetriesCsvFileName <- "compParaRetriesOfvs.csv"
  if(!file.exists(compParaRetriesCsvFileName)){
    print(paste0("Could not find ", compParaRetriesCsvFileName, 
                 ". Proceeding without compParaRetries."))
    compParaRetries <- FALSE
  }
  
  # Hardcoding a max length for now. Could parse command or count
  maxLength <- 1000
  length(paraRetriesOfvs) <- maxLength
  length(paraRetriesRetry) <- maxLength
  
  if(compParaRetries){
    compParaRetriesCsv <- read.csv(compParaRetriesCsvFileName, header = TRUE)
    compParaRetriesOfvs <- compParaRetriesCsv$paraRetriesCompOfv
    length(compParaRetriesOfvs) <- maxLength
  }else{
    compParaRetriesOfvs <- rep(NA, maxLength)
  }
  
  # Inefficient rawres file finding...
  nmDismountRawresPaths <-  list.files(path = "nmDismountRuns", 
                                       pattern = "raw_results_.+csv$", 
                                       recursive = TRUE, full.names = TRUE)
  
  # Parsing out the OFVs from the each rawres file 
  nmDismountOfvsList <- lapply(nmDismountRawresPaths, function(x){
    
    # Get the relevant OFVs and minimization statuses
    rawresSubset <- parseRawres(x, cols = c("minimization_successful", "ofv"))
    ofvVec <- rawresSubset["ofv"][[1]]
    
    # Set OFV to NA for unsuccessful estimations if asked for
    if(checkMinSuccess){
      minSuccess <- rawresSubset["minimization_successful"][[1]]
      ofvVec[!as.logical(minSuccess)] <- NA
    }
    ofvs <- data.frame(ofvVec)
    # Name the vector after the settings (splitting folder name should work but 
    # assumes known file structure)
    settingsUsed <- unlist(strsplit(x, "/"))[2]
    names(ofvs) <- settingsUsed
    
    return(ofvs)
  })
  
  nmDismountOfvs <- do.call("cbind", nmDismountOfvsList)
  allOfvs <- cbind(paraRetriesRetry, paraRetriesOfvs, nmDismountOfvs, compParaRetriesOfvs)
  write.csv(allOfvs, "allOfvs.csv", row.names = FALSE)
  
  minOfv <- min(allOfvs[2:length(allOfvs)], na.rm = TRUE)
  limOfv <- minOfv + 1
  
  acceptOfvs <- apply(allOfvs[2:length(allOfvs)], c(1, 2), function(x){
    
    if(is.na(x)){
      return(NA)
    }
    
    if(x <= limOfv){
      return(1)
    }else{
      return(0)
    }
  })
  
  
  percentageTable <- apply(acceptOfvs, 2, function(x){
    
    failedPercentage <- percent(sum(is.na(x))/length(x))
    acceptPercentage <- percent(sum(x == 1, na.rm = TRUE)/length(x))
    unacceptPercentage <- percent(sum(x == 0, na.rm = TRUE)/length(x))
    return(c(acceptPercentage, unacceptPercentage, failedPercentage))
  })
  
  row.names(percentageTable) <- c("Lowest OFV", "Higher OFV", "Failed")
  
  write.csv(percentageTable, paste0("percentageTable", basename(wd), ".csv"))
  
  setwd(userWd)
  
  return(percentageTable)
}
