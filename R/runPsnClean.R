#' Run psn_clean
#' 
#' Runs the PsN cleanup script. Currently only the level option is mapped and wrapped.
#' 
#' @param level How much to clean up. Higher setting removes more. 2 - 4 is available with 3 being the default.
#' @param interact If TRUE the user is prompted for deletion or every file in an interactive session. Default is FALSE.
#' @param addOpt String of additional options to include on command line. Must follow PsN conventions and include space in between options. Default is an empty string.
#' 
#' runPsnClean()
#' 
#' 
#' @export


runPsnClean <- function(path = ".", level = 3, interact = FALSE, addOpt = ""){
  
  # Determine what string to use for the interactive option
  
  interactOpt <- ifelse(interact, 
                        " -interactive ", 
                        " -no-interactive ")
  
  # Paste together the command and run it
  
  cmd <- paste0("psn_clean ", path, " -level=", level, interactOpt, addOpt)
  system(cmd)
  
}