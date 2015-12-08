#' parseExtFile
#' 
#' Parses an ext file and return a list of data frame. Each data frame contains the outputs from one estimation step.
#'  
#' @param extFilePath File Path to the ext file to be parsed.
#' 
#' @export


parseExtFile <- function(extFilePath){
  
  # Check to make sure the referenced file exists
  if (!file.exists(extFilePath)) {
    stop("Failed to parse ext file; expected input file \"", 
         extFilePath, "\" does not exist.")
  }
  
  # Parsing the .ext using readLines because of different types of pesky whitespace.
  rawExtFile <- readLines(extFilePath)
  
  # Pick out the separate tables. I include the last row in the vector to know where to stop.
  tableRows <- grep("TABLE", rawExtFile)
  tableAndLastRows <- c(tableRows, length(rawExtFile))
  noTables <- length(tableRows)
  
  # Extracting the table number and method name as names for the tables. Regex is not perfect here, 
  # but does work in all the examples I've looked at.
  tableNames <- gsub("\\(.+$", "", rawExtFile[tableRows])
  tableNames <- gsub("\\s\\s+", "", tableNames)
  tableNames <- gsub("\\s+$", "", tableNames)
  
  # Create a list of dataframes with each 
  tableList <- lapply(1:noTables, function(x){
    
    # Read in the numeric portion of this table, ignoring the title and header rows 
    table <- rawExtFile[(tableAndLastRows[x]+2):(tableAndLastRows[x+1]-1)]
    
    # Extract the header 
    header <- unlist(strsplit(gsub("^\\s+", "", rawExtFile[(tableAndLastRows[x]+1)]), "\\s+"))
    
    # Remove leading whitespace
    table <- gsub("^\\s+", "", table)
    
    # Separate the values (still as char strings)
    tableCharDF <- data.frame(do.call("rbind", strsplit(table, "\\s+")),
                              stringsAsFactors = FALSE)
    
    # Set the header
    names(tableCharDF) <- header
    
    # Convert to numerics
    tableDF <- as.data.frame(sapply(tableCharDF, as.numeric))
    
    return(tableDF)
  })
  
  # Set names of the separate tables
  names(tableList) <- tableNames
  
  # Return the ext file contents as a dataframe
  return(tableList)
}