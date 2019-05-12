weighted_MSE <- function(Y_true, Y_predicted, A_coeff){
  if (!(is.vector(Y_true, mode = "numeric") &
        is.matrix(Y_predicted) &
        is.matrix(A_coeff)))
  {stop("Not correct input type")}
  if (any(c(length(Y_true), nrow(Y_predicted), nrow(A_coeff))
          != rep(length(Y_true), 3)))
  {stop("Not correct input dimensions")}
  MSE <- (Y_true - Y_predicted)^2
  return((colSums(A_coeff * MSE))/length(Y_true))
}
