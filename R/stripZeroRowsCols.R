#' stripZeroRowsCols
#' 
#' removes zero rows and columns from a matrix return a matrix. 
#'  
#' @param inputMatrix Input matrix.
#' 
#' @export

stripZeroRowsCols <- function(inputMatrix){
  
  # Make sure the matrix is symmetric
  if(ncol(inputMatrix) != nrow(inputMatrix)){
    print("Matrix sent to stripZeroRowsCols is not symmetric")
    return(NULL)
  }
  
  # Pick out the row numbers for any zero rows
  zeroRows <- which(apply(inputMatrix, 1, sum) == 0)
  
  # Check if any zero rows were found and remove them, if not return the input matrix
  if(length(zeroRows)==0){
    print("No zero rows to strip out")
    return(inputMatrix)
  }else{
    noZeroRowsColsMatrix <- inputMatrix[-c(zeroRows),
                                        -c(zeroRows)]
    return(list(noZeroRowsColsMatrix, zeroRows))
  }  
}