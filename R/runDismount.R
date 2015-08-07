
# I should rewrite this better... para retries run function would be the example to look at

runDismount <- function(modelFileName, wait = FALSE){
  
  modelFileNameNoExt <- sub("\\.[[:alnum:]]+$", "", basename(as.character(modelFileName)))
  
  dir <- paste("dismount", modelFileNameNoExt, format(Sys.time(), "%y%m%d_%H%M%S"), sep = "_")
  
  cmd <- paste0("srun perl /blue/home/USER/yasao745/PsN4_4_ver_YA/isestimable ", 
                modelFileName, " -dir=", dir, " -run_on_slurm > ", dir, "/", 
                dir, "_log.txt 2>&1")
  
  # Print the command to command line
  print(cmd)
  
  # Run the command
  system(cmd, intern = FALSE, wait = wait)
  
  return(dir)
}