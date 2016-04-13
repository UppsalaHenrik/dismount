#' 
#' 
#' @export


runPsnClean <- function(path = ".", level = 3){
  
  cmd <- paste0("psn_clean ", path, " -level=", level)
  system(cmd)
  
}