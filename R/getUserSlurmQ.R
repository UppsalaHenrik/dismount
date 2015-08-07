
getUserSlurmQ <- function(){
  
  # Get user name
  userName <- system('echo "$USER"', intern=TRUE)
  
  # Get the slurm queue for the active user
  slurmQ <- system(paste("squeue", "-u", userName), intern=TRUE)
  
  return(slurmQ)
}

