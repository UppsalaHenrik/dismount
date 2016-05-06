#' Run parallel retries
#'
#' This is a wrapper function for parallel_retries in PsN
#' @param modFileName The model to run parallel retries on. Only one permitted.
#' @param paraRetriesCmd Command or path used to run parallel retries. Default is "parallel_retries" which assumes the PsN directory is on the PATH.
#' @param dir The directory to run in. Defaults to "para_retries_<date>_<time>".
#' @param clean The PsN clean-up level. Exactly wrapping the PsN option. Defaults to 2.
#' @param threads The number of threads to use. Exactly wrapping the PsN option. Defaults to 100.
#' @param min_retries The number of retries to run. Exactly wrapping the PsN option. Defaults to 1
#' 
#' runParaRetries()
#'
#' @export
#'

runParaRetries <- function(modFileName, paraRetriesCmd = "parallel_retries", 
                           dir = paste0("para_retries_", format(Sys.time(), "%y%m%d_%H%M%S")),
                           clean = 2, threads = 100, min_retries = 1, degree = 0.1,
                           slurm_partition = "standard", rawres_input = "", seed = format(Sys.time(), "%Y%m%d"),
                           nm_output = NULL, wait = TRUE){

  # Test that degree is not outside its bounds
  if(degree <= 0 || degree >= 1){
    print("Degree setting must be between 0 and 1")
    return(NULL)

  # If it is ok, set the degree option
  }else{
    perturbation <- paste0(" -degree=", degree)
  }

  # Set nm_output if there is any
  if(length(nm_output) > 0){
    extraNMOutputs <- paste0(" -nm_output=", paste(nm_output, collapse = ","))
  }else{
    extraNMOutputs <- ""
  }

  partition <- paste0(" -slurm_partition=", slurm_partition)

  # If there is a rawres input file supplied, and that file exists, use it and disregard min_retries.
  # If there isn't one, use the min_retries setting
  ifelse(file.exists(rawres_input),
         retriesOrInput <- paste0(" -rawres_input=", rawres_input),
         retriesOrInput <- paste0(" -min_retries=", min_retries))

  # Create the command
  cmd <- paste0(paraRetriesCmd, " ", modFileName, " -dir=", dir, 
                retriesOrInput, " -clean=", clean,
                perturbation, " -threads=", threads, " -seed=", 
                seed, partition, extraNMOutputs)

  # Print the command to command line
  print(cmd)

  # Run the command
  system(cmd, intern = FALSE, wait = wait)

  return(dir)
}
