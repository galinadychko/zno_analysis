weighted_MSE <- function(Y_true, Y_predicted, A_coeff){
  if (!(is.vector(Y_true, mode="numeric") & is.vector(Y_predicted, mode="numeric") & 
      is.vector(A_coeff, mode="numeric"))) 
  {stop("Not correct input type")}
  if (any(c(length(Y_true), length(Y_predicted), length(A_coeff)) 
          != rep(length(Y_predicted), 3)))
  {stop("Not correct imput dimensions")}
  MSE <- (Y_true - Y_predicted)^2
  return(as.vector(A_coeff %*% MSE)/length(Y_true))
} 
