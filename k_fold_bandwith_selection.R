#' Evaluate fold error for a set of bandwiths. 
#'
#' Evaluates fold error for a set of bandwiths. Function is passed a set of posssible
#' h values and determines the mean cross-validated error term for the bandwith. 
#'
#' @param input_matrix matrix containing covariate data.
#' @param k value specifying the number of folds for cross-validation.
#' @param h a vector of bandwith values to search across. 
#'
#' @return mean fold error for each bandwith in the set h. 

kfold.bandwith = function(k, input_matrix, h) {
     
    norms = as.matrix(dist(input_matrix))
     norms = dnorm(norms/h) 
        index = sample(nrow(input_matrix))
        split = split(index, cut(seq(length(index)), k))
  
  set_of_r_b = 0
  for (i in 1:k) {
      a = 1:nrow(input_matrix)
      b = setdiff(a, split[[i]])
          fold_error = 0
          Y_hat_minus_i = 0
    
    denominator = sum(norms[split[[i]],b])
    Y_hat_minus_i = as.matrix(norms[split[[i]],b]/denominator) %*% as.vector(input_matrix[b, 1])
        fold_error = sum((Y_hat_minus_i - input_matrix[split[[i]], 1])^2)
          r_hat_b = (fold_error/length(split[[i]]))
            set_of_r_b = set_of_r_b + r_hat_b
    }
  return (mean(set_of_r_b))
}