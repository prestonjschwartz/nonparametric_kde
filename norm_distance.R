#' Determine the euclidean norm of the data. 
#'
#' Determines the euclidean norm of the data. 
#'
#' @param x the data for which the distance is to be calculated. 
#'
#' @return the euclidean distance of the data used as the input. 

normVec = function(x) {
  return (sqrt(rowSums(x^2)))
}