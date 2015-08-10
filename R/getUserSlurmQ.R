#' Get SLURM queue for active user
#'
#' Gets the slurm queue for the user that is currently logged in.
#' No options available. Returns a character string with the rows
#' of the queue, including the header.
#'getUserSlurmQ()

getUserSlurmQ <- function(){

  # Get user name
  userName <- system('echo "$USER"', intern=TRUE)

  # Get the slurm queue for the active user
  slurmQ <- system(paste("squeue", "-u", userName), intern=TRUE)

  return(slurmQ)
}

