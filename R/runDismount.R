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
#'
#'
#' runDismount()



# I should rewrite this better... para retries run function would be the example to look at

runDismount <- function(modelFileName, wait = FALSE){

  # Wait for the SLURM queue to have less than 100 runs in it
  waitForSlurmQ(targetLength=100, secsToWait=5, maxWaits=12)



  modelFileNameNoExt <- sub("\\.[[:alnum:]]+$", "", basename(as.character(modelFileName)))

  dir <- paste("dismount", modelFileNameNoExt, format(Sys.time(), "%y%m%d_%H%M%S"), sep = "_")

  cmd <- paste0("srun perl /blue/home/USER/yasao745/PsN4_4_ver_YA/isestimable ",
                basename(as.character(modelFileName)), " -dir=", dir, " -run_on_slurm > ", dir, "/",
                dir, "_log.txt 2>&1")

  # Print the command to command line
  print(cmd)

  # Run the command
  system(cmd, intern = FALSE, wait = wait)

  return(dir)
}
