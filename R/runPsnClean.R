#' Run psn_clean
#' 
#' Runs the PsN cleanup script. Currently only the level option is mapped and wrapped.
#' 
#' @param level How much to clean up. Higher setting removes more. 2 - 4 is available with 3 being the default.
#' 
#' @export


runPsnClean <- function(path = ".", level = 3){
  
  cmd <- paste0("psn_clean ", path, " -level=", level)
  system(cmd)
  
}