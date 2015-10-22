#' Run dismount (isestimable)
#'
#' Runs dismount (aka isestimable). Currently limited to slurm queue submission.
#' This function is in need of some TLC.
#'
#'
#' @param modelFileName The name of the model file. No default
#' @param wait Wraps the wait option in system(), which specifies whether to
#' wait for the system call to complete before continuing in R (TRUE) or not
#' wait after submission (FALSE). Default is FALSE
#' @param logging Whether to log the command line outputs to a text file or not. Default is FALSE.
#'
#' runDismount()
#'
#' @author Henrik Bjugård Nyberg - henrik.b.nyberg@@farmbio.uu.se



# I should rewrite this better... para retries run function would be the example to look at

runDismount <- function(modelFileName, wait = FALSE, logging = FALSE, runOnSlurm = FALSE){

  # Wait for the SLURM queue to have less than 100 runs in it
  waitForSlurmQ(targetLength=100, secsToWait=5, maxWaits=12)


  if(logging){
    logOpt <- paste0(" > ", dir, "_log.txt 2>&1")
  }else{
    logOpt <- ""
  }
  
  if(runOnSlurm){
    runOnSlurmOpt <- paste0(" -run_on_slurm")
  }else{
    runOnSlurmOpt <- ""
  }

  modelFileNameNoExt <- sub("\\.[[:alnum:]]+$", "", basename(as.character(modelFileName)))

  dir <- paste("dismount", modelFileNameNoExt, sep = "_")

  cmd <- paste0("srun perl /blue/home/USER/bjuny231/PrecondProject/_HackedPsN7/PsN4_4_ver_YA/bin/isestimable ",
                basename(as.character(modelFileName)), " -dir=", dir, runOnSlurmOpt, 
                logOpt)

  # Print the command to command line
  print(cmd)

  # Run the command
  system(cmd, intern = FALSE, wait = wait)

  # Sleep half a second to not overload the system.
  Sys.sleep(3)
  
  return(dir)
}
