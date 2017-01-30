#' Get SLURM queue for active user
#'
#' Gets the slurm queue for the user that is currently logged in.
#' No options available. Returns a character string with the rows
#' of the queue, including the header.
#'
#' @param userName The user to get the queue for. Default is 
#'        the current user
#' @param extraSqueueOptions Additional options to add to the 
#'        squeue command. Default is empty string.
#'
#' getUserSlurmQ()
#'
#' @export
#'

getUserSlurmQ <- function(userName = "current", extraSqueueOptions = ""){
  
  # Get user name
  if(userName == "current"){
    userName <- system('echo "$USER"', intern=TRUE)
  }
  
  # Get the slurm queue for the active user
  slurmQStringsAndHeader <- system(paste("squeue", "-u", userName, 
                                         extraSqueueOptions), intern=TRUE)
  
  # Remove leading whitespace
  slurmQStringsAndHeader <- gsub("^\\s+", "", slurmQStringsAndHeader)
  
  # If there are no jobs in the queue, stop
  if(length(slurmQStringsAndHeader) <= 1){
    
    stop("No jobs found in queue for user ", userName)
    
  }
  
  # Separate the non-header rows
  slurmQStrings <- slurmQStringsAndHeader[-1]
  
  # Extract the header
  header <- unlist(strsplit(slurmQStringsAndHeader[1], "\\s+"))
  
  # Separate the values (still as char strings)
  slurmQCharDF <- data.frame(do.call("rbind", strsplit(slurmQStrings, ".\\s+")),stringsAsFactors = FALSE)
  
  # Set the header
  names(slurmQCharDF) <- header
  
  # Convert to numerics
  slurmQ <- slurmQCharDF
  slurmQ["JOBID"] <- as.data.frame(sapply(slurmQCharDF["JOBID"], as.numeric))
  
  return(slurmQ)
}