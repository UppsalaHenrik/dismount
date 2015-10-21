#' Categorize runs in a raw results file based on minimization and cov step success
#'
#'
#' @param rawres A raw results data frame following the PsN standard. 
#'               Most importantly containing minimization_successful
#'               and covariance_step_successful columns.
#' 
#' 
#' @author Henrik Bjugård Nyberg - henrik.b.nyberg@@farmbio.uu.se
#'



assignExecutionGroups <- function(rawres){
  
  
  rawres$group <- ifelse(rawres$minimization_successful == 1 & rawres$covariance_step_successful == 1, 1,
                                ifelse(rawres$minimization_successful == 1 & rawres$covariance_step_successful == 0, 2,
                                       ifelse(rawres$minimization_successful == 0 & rawres$covariance_step_successful == 1, 3,
                                              ifelse(rawres$minimization_successful == 0 & rawres$covariance_step_successful == 0, 4,
                                                     5))))
  
  return(rawres)
} 