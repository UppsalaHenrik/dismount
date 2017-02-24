#' @export


workflowNmDismountRuns <- function(retryModFilePaths, dirName, saddleReset, 
                                   saddleHess, nm_version = "7_40_g51_alpha14",
                                   slurm_partition = "standard"){
  
  # Save current working directory, create a subdirectory, copy the model files 
  # to it and set it as wd
  cwd <- getwd()
  
  # Set the right saddle options
  setModList <- sapply(retryModFilePaths, function(x){
    setSaddleReset(x, saddleReset, printMessage = FALSE)
    setSaddleHess(x, saddleHess, printMessage = FALSE)
  })
  
  modFiles <- as.vector(setModList)
  modFileString <- paste(modFiles, collapse = " ")
  
  # Run dismount on the models
  cmd <- paste0("srun execute ", modFileString, " -dir=", dirName, 
                " -nm_version=", nm_version, " -threads=100", 
                " -slurm_partition=", slurm_partition)
  print(paste("running command ", cmd))
  
  system(cmd)

  # I've had issues with the runs not starting before I start the waitForSlurmQ 
  # below, so here is a little initial wait
  Sys.sleep(10)
  
  # Wait for the queue 
  waitForSlurmQ(targetLength = 1, maxWaits = 20)

  # Find and parse the rawres file
  nmDismountRawresPath <- findRawres(dirName)
  nmDismountRawres <- parseRawres(nmDismountRawresPath)
  
  # Add the vector of model file names. This assumes that the models are handled in order by PsN.
  # I really hope that's the case
  nmDismountRawres <- cbind(modFileName = modFiles, nmDismountRawres)
  
  
  nmDismountOFVs <- cbind(dirName = dirName, nmDismountOFV = nmDismountRawres$ofv)
  
  return(nmDismountOFVs)
}