#' @export

createPercentageTable <- function(wd = getwd()){
  
  userWd <- getwd()
  
  setwd(wd)
  
  # For each model I combine the ofvs from the different settings into a data frame
  
  # First the initial parallel retries
  paraRetriesCsvFileName <- "paraRetriesOfvs.csv"
  paraRetriesCsv <- read.csv(paraRetriesOfvsFileName, header = TRUE)
  paraRetriesOfvs <- paraRetriesCsv$paraRetriesOfv
  paraRetriesRetry <- paraRetriesCsv$retry
  
  compParaRetriesCsvFileName <- "compParaRetriesOfvs.csv"
  compParaRetriesCsv <- read.csv(compParaRetriesOfvsFileName, header = TRUE)
  compParaRetriesOfvs <- compParaRetriesCsv$paraRetriesCompOfv
  compParaRetriesRetry <- compParaRetriesCsv$retry
  
  
  # Hardcoding a max length for now. Could parse command or count
  maxLength <- 1000
  length(paraRetriesOfvs) <- maxLength
  length(paraRetriesRetry) <- maxLength
  length(compParaRetriesOfvs) <- maxLength
  length(compParaRetriesRetry) <- maxLength
  
  # Inefficient rawres file finding...
  nmDismountRawresPaths <-  list.files(path = "nmDismountRuns", 
                                       pattern = "raw_results_.+csv$", 
                                       recursive = TRUE, full.names = TRUE)
  
  # Parsing out the OFVs from the each rawres file 
  nmDismountOfvsList <- lapply(nmDismountRawresPaths, function(x){
    
    # Get the relevant OFVs
    ofvs <- parseRawres(x, cols = "ofv")
    
    # Name the vector after the settings (splitting folder name should work but 
    # assumes known file structure)
    settingsUsed <- unlist(strsplit(x, "/"))[2]
    names(ofvs) <- settingsUsed
    
    return(ofvs)
  })
  
  nmDismountOfvs <- do.call("cbind", nmDismountOfvsList)
  allOfvs <- cbind(paraRetriesRetries, paraRetriesOfvs, nmDismountOfvs, compParaRetriesOfvs)
  
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
  
  write.csv(percentageTable, "percentageTable.csv", row.names = FALSE)
  
  setwd(userWd)
  
  return(percentageTable)
}
# mod <- "Jönsson"
# 
# plotDf <- data.frame()
# 
# for(i in 3:ncol(acceptOfvs)){
#   
#   plotDf <- rbind(plotDf, cbind(rep(mod, nrow(acceptOfvs)), 
#                                 rep(names(acceptOfvs)[[i]], 
#                                     nrow(acceptOfvs)),
#                                 acceptOfvs[[i]]))
# }

# plotDf$acceptOfv <- factor(plotDf$acceptOfv, levels = c(1, 0))

# names(plotDf) <- c("model", "run", "acceptOfv")

# qplot(run, data = plotDf, geom = 'bar', 
#       fill = acceptOfv, facets = .~ model, xlab = "Setting", ylab = NULL) +
#   scale_fill_manual(values = c("darkgreen", "darkred")) +
#   scale_y_continuous(labels = scales::percent) +
#   #scale_x_discrete(limits = c('2 Comp', 'Pheno', 'Jönsson', 'Bergmann', 'Wahlby')) +
#   geom_text(stat='count',aes(label=..count..),vjust=+1) +
#   guides(fill=FALSE)



### Make a nice table

# One row per model. Data workup same as above
# Headers: Model saddle1 saddle2 saddle3 saddle1_hess1 ....
# skriv ut till en fil också...




