#' Run precond
#'
#'
#' @param modelFileName The name of the model file. No default.
#' @param pertSize Size of perturbation. No default.
#'
#'

# "/blue/home/USER/bjuny231/PrecondProject/_hackedPsN6/PsN4_4/bin/precond_numStab"


runPrecond <- function(modelFileName, pertSize = 0, precondScriptPath = "precond",
                       preCondMatFileName = NULL, pertSeed = 123456, always = TRUE){

  modelFileNameNoExt <- sub("\\.[[:alnum:]]+$", "", basename(as.character(modelFileName)))

  # Wait for the SLURM queue to have less than 100 runs in it
  waitForSlurmQ(targetLength=100, secsToWait=5, maxWaits=12)

  # Create a dir name to use
  dirName <- paste0(modelFileNameNoExt, 
                    gsub(".csv$", "", ifelse(length(preCondMatFileName) == 0,
                                             "",
                                             paste0("_", basename(preCondMatFileName)))))

  if(length(preCondMatFileName) == 0){
    
    preCondMatOpt <- "" 
    
  }else{
    
    preCondMatOpt <- paste0(" -pre=", preCondMatFileName)
    
  }
  
  if(always){
    alwaysOpt <- " -always"
  }else{
    alwaysOpt <- ""
  }
  
  
  # Create the command
  cmd <- paste0(precondScriptPath, " ", modelFileName, " -dir=", dirName,
                preCondMatOpt, " -cholesky -pertSize=", pertSize,
                " -clean=2 -seedForPert=", pertSeed, alwaysOpt)

  print(cmd)

  # Run the command
  system(cmd, intern=FALSE, wait=FALSE)
  
  Sys.sleep(0.1)

  return(dirName)
}






