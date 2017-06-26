#' Nadaraya Watson Regression 
#'
#' Nonparametric regession using Nadaraya Watson kernel density estimation. The function fits the 
#' Nadaraya Watson estimator to the data using the gaussian kernel and a bandiwth determined by 
#' cross validation. 
#'
#' @param data_point multivariate data to be passed through the function, i.e. the independent object
#' to be used to estimate the response variable.
#' @param DF matrix containing covariate data.
#' @param k the number of folds on which the errors are to be cross-validated
#'
#' @return classify the response variable, given covariate information. 

nw.estimator = function(data_point, DF, k) {
  x_i = DF[,-ncol(DF)]
  
  distances = c()
    for (i in 1:nrow(DF)) {
      distances = append(distances, normVec((x_i[i,] - data_point)) / (kernel.bandwith(DF, k))) 
    }
    kerns = as.vector(unlist(lapply(distances, dnorm)))
    
  return((1/(nrow(DF)*(h*(ncol(DF)-1))))*sum(kerns))
} 