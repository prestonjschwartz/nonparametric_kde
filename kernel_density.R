#' Gaussian Kernel Output
#'
#' The function computes kernel density estimates using the gaussian kernel and a
#' bandwith value determined by k-fold cross validation error minimizaton. 
#'
#' @param DF matrix containing covariate data.
#' @param x the data from which the estimates are to be computed. 
#' @param k the number of folds on which the errors are to be cross-validated. 
#'
#' @return estimates of the kernel density using gaussian. 

density_output = function(DF, k, x) {
  return(density(x, DF, kernel.bandwith(DF, k)))
}