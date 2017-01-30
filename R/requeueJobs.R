#' @export

requeueJobs <- function(userName = "current", jobType = "SUSPENDED"){
  
  # Get the relevant job ids. This isn't perfect... unlist should go
  extraOptionsString <- paste0('-t ', jobType, ' -o "%.10i"')
  jobIdsList <- getUserSlurmQ(userName, extraSqueueOptions = extraOptionsString)
  jobIds <- unlist(jobIdsList)
  
  # If there were no job ids, stop
  if(length(jobIds) == 0){
    stop("No suspended jobs in queue")
  }
  
  # Run scontrol requeue on each suspended job
  sapply(jobIds, function(x){
    
    print(paste("Requeing job number", x))
    
    # Build requeue command
    cmd <- paste("scontrol requeue", x)
    
    # Execute command
    system(cmd , wait = FALSE, intern = FALSE)
    
  })
  
  return(jobIds)
  
  
}