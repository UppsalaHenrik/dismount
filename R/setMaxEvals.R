
setMaxEvals <- function(controlFile, newMaxEvals){
  
  # Find any rows with a MAXEVAL setting 
  rowNum <- grep("MAX[A-Z]*?=", controlFile)
  
  # If there is no current setting, just return the original file.
  if(length(rowNum) == 0){
    
    print("Found no current MAXEVAL setting. Will return control file unchanged.")
    
    return(controlFile)
  }
  
  row <- controlFile[rowNum]
  
  splitRow <- unlist(strsplit(row, " "))
  
  splitRow[grep("MAX[A-Z]*?=", splitRow)] <- paste0("MAXEVAL=", newMaxEvals)
   
  # Put the file to be edited in the new object 
  newControlFile <- controlFile
  
  # Replace the row in question with the edited one
  newControlFile[rowNum] <- paste(splitRow, collapse = " ")
  
  # Return the new 
  return(newControlFile)
}