#' Determine optimal bandiwth value
#'
#' Searches across a set of possible h values to determine the optimal. Search
#' function selects the bandwith associated to minimum error. 
#'
#' @param DF matrix containing covariate data.
#' @param k value specifying the number of folds for cross-validation. 
#'
#' @return optimal bandiwth for the kernel density. 

kernel.bandwith = function(DF, k) { 
  h_values = matrix(1:1000, nrow = 1000, ncol = 2, byrow = FALSE)
  h_values[,1] = h_values[,1] / 10000
    
    for(i in 1:nrow(h_values)) { 
      h_values[i,2] = kfold.bandwith(k, DF, h_values[i,1])
    }
        
  return(na.omit(h_values)[which.min(h_values[, 2]),1])
}