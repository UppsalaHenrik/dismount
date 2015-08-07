
# A function that waits for the SLURM queue to be smaller than a 
# target number of runs. Used either for 

waitForSlurmQ <- function(targetLength=0, secsToWait=30, maxWaits=20){
  
  keepWaiting <- TRUE
  
  i <- 1
  
  while(keepWaiting & i <= maxWaits){
    
    # Get the slurm queue and check its length
    slurmQ <- getUserSlurmQ()
    qLength <- length(slurmQ)
    
    # if it has no jobs in it end the while loop
    if(qLength - 1 <= targetLength){
      keepWaiting <- FALSE
      qTargetMessage <- paste0("Queue has ", qLength - 1, " jobs (<=", targetLength, ") - ending wait.")
      print(qTargetMessage)
      break
    }
    
    # Print a message about the wait
    qWaitMessage <- paste("Waiting for", secsToWait, "seconds (nr", 
                          i, "out of max", maxWaits, "waits) - ", 
                          qLength-1, "jobs in queue")
    print(qWaitMessage)
    
    # Wait for specified number of seconds 
    Sys.sleep(secsToWait)
    
    # Grow i 
    i <- i + 1
    
    if(i > maxWaits){
      qMaxWaitsMessage <- paste("Wait limit reached. Waited for", secsToWait*i, "seconds" )
      print(qMaxWaitsMessage)
    }
  }  
  
}
