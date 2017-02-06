#' Wait for the SLURM queue to empty
#'
#' A function that waits for the SLURM queue to be smaller than a
#' target number of runs. Typically used to wait for a run that your
#' code has kicked off before parsing results. Will print messages
#' with how many runs it waits for and for how long. The wait will
#' time out after ( secsToWait * maxWaits ) seconds.
#'
#' @param targetLength How many runs to leave in the queue.Default is 0.
#' @param secsToWait How many seconds to wait between parsing the queue. Default is 30.
#' @param maxWaits Maximum number of times the wait should happen before timing out. Default is 20.
#' waitForSlurmQ()
#'
#' @export
#'
#' @author Henrik Bjugård Nyberg - henrik.b.nyberg@@farmbio.uu.se

waitForSlurmQ <- function(targetLength = 0, secsToWait = 30, maxWaits = 20){

  # Set initial state
  keepWaiting <- TRUE
  i <- 1

  # While loop until the targetLength or the maxWaits is reached
  while(keepWaiting & i <= maxWaits){

    # Get the slurm queue and check its length
    slurmQ <- getUserSlurmQ()
    qLength <- nrow(slurmQ)

    # if it has no jobs in it end the while loop
    if(qLength - 1 <= targetLength){
      keepWaiting <- FALSE
      qTargetMessage <- paste0("Queue has ", qLength, 
                               " jobs (<=", targetLength, 
                               ") - ending wait.")
      print(qTargetMessage)
      break
    }

    # Print a message about the wait
    qWaitMessage <- paste("Waiting for", secsToWait, "seconds (nr",
                          i, "out of max", maxWaits, "waits) - ",
                          qLength, "jobs in queue")
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
