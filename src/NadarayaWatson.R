# --------------------------------------------------------
# Epanechnikov kernell
# --------------------------------------------------------


Epanechnikov <- function(x){
  if (is.matrix(x) != TRUE & is.vector(x,  mode = "numeric") != TRUE)
    {stop("Not correct input type")}
  result <- 3/4*(1 - x^2)*(abs(x) <= 1)
  return(result)
}


# --------------------------------------------------------
# Nadaraya-Watson for component
# --------------------------------------------------------

nw_any_components <- function(x, X_train, Y_train, h, A){
  if (is.vector(X_train, mode = "numeric") != TRUE | 
      is.vector(Y_train, mode = "numeric") != TRUE | 
      is.vector(x, mode = "numeric") != TRUE) 
    {stop("Not correct input type")}
  if (is.vector(A, mode = "numeric") == TRUE) {A <- as.matrix(A, ncol = 1)}
  if (length(x) > 1) {stop("Not correct input type")}
  
  n_colms <- ncol(A)
  n_rows <- nrow(A)
  h_matrix <- 1/matrix(t(h), nrow = n_rows, ncol = n_colms, byrow = TRUE)
  denominator <- colSums(matrix(A * Epanechnikov(h_matrix*(x - X_train)), ncol = n_colms))
  if (sum(denominator) == 0) {stop("Devision by zero")}
  numerator <- colSums(matrix(A * Y_train * Epanechnikov(h_matrix*(x - X_train)), ncol = n_colms))
  return(numerator/denominator)
}
