naive.bayes = function(data_point, input_matrix, h) {
    x_i = input_matrix[,-ncol(input_matrix)]
      distances = c()
        for (i in 1:nrow(input_matrix)){
          distances = append(distances, normVec((x_i[i,] - data_point))/h ) 
        }
      kerns = as.vector(unlist(lapply(distances, dnorm)))
      num_of_ones = length(ones_indicies)/nrow(input_matrix)
          p_hat_h = (1/nrow(input_matrix)) * sum(kerns)
          p_hat_k = (1/length(ones_indicies)) * sum(kerns[ones_indicies])
      to_compare = ((p_hat_k/p_hat_h) * num_of_ones)
      print(to_compare)
      if (to_compare > .5) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
