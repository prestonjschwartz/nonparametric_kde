L_fold = function(number_of_folds, input_matrix, h) {
  indicies = sample(nrow(DF))
      norms = as.matrix(dist(DF[,-1]))
      norms = dnorm(norms/h)
        split_indicies = split(indicies, cut(seq(length(indicies)), number_of_folds))
    set_of_R_b = c()
    for (i in 1:number_of_folds) {
      Y_hat_minus_i = 0
      y_i = as.matrix(input_matrix[split_indicies[[i]],10])
          a = 1:nrow(input_matrix)
          b = setdiff(a, split_indicies[[i]])
            ones_in_fold = intersect(b, ones_indicies)
            fold_error = 0
              num_of_ones = length(ones_in_fold)/length(b)
            p_hat_h = (1/length(b)) * rowSums(norms[split_indicies[[i]],b])
          p_hat_k = (1/length(ones_in_fold)) * rowSums(norms[split_indicies[[i]], as.vector(unlist(ones_in_fold))])
          to_compare = ((p_hat_k/p_hat_h) * num_of_ones)
        Yn_b = as.matrix(lapply(to_compare, compare))
      error = c()
    
    for (i in 1:length(Yn_b)) {
      error = append(error, (unlist(Yn_b)[i]-y_i[i]))
    }
        fold_error = sum(error^2)
        R_hat_b = (fold_error/nrow(y_i))
      set_of_R_b = append(set_of_R_b, R_hat_b)
    }
  return (mean(set_of_R_b))
}