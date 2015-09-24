#' Run precond
#'
#'
#' @param modelFileName The name of the model file. No default.
#' @param pertSize Size of perturbation. No default.
#'
#'




runPrecond <- function(modelFileName, pertSize, precondScriptPath,
                       runNum, illCondFileName, pertSeed){

  modelFileNameNoExt <- sub("\\.[[:alnum:]]+$", "", basename(as.character(modelFileName)))

  # Wait for the SLURM queue to have less than 100 runs in it
  waitForSlurmQ(targetLength=100, secsToWait=5, maxWaits=12)

  # Create a dir name to use
  dirName <- paste0("./illCondRuns/", gsub(".csv$", "", basename(illCondFileName)))

  # Create the command
  cmd <- paste0("srun perl ", precondScriptPath, " ", modelFileName, " -dir=", dirName,
                " -pre=", illCondFileName, " -cholesky -pertSize=", pertSize,
                " -clean=2 -seedForPert=", pertSeed)

  print(cmd)

  # Run the command
  system(cmd, intern=FALSE, wait=FALSE)

  return(dirName)
}






