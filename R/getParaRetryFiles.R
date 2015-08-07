

getParaRetryFiles <- function(path, fileExt, retryNumsToParse = "all"){
  
  # In case file extension is given with dot I remove it
  fileExt <- gsub("\\.", "", fileExt)
  
  if(retryNumsToParse == "all"){
    
    # Grab all files that follow the pattern retry<number>.<fileExtension>
    filesToParse <- list.files(path, pattern = paste0("retry[0-9]+\\.", fileExt))
  }else{
    
    # Otherwise retryNumsToParse is assumed to be a numerical vector and we get only those numbers
    filesToParseList <- sapply(retryNumsToParse, function(x){
      
      fileToParse <- list.files(path, pattern = paste0("retry", x, "\\.", fileExt))
      
      return(fileToParse)
    })
    
    filesToParse <- unlist(filesToParseList)
  }
  
  return(filesToParse)
}