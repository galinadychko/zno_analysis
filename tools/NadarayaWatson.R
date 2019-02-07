# --------------------------------------------------------
# Epanechnikov kernell
# --------------------------------------------------------


Epanechnikov <- function(x){
  if (is.matrix(x) == TRUE) {stop("Not correct input type")}
  result <- 3/4*(1 - x^2)*(abs(x) <= 1)
  return(result)
}


# --------------------------------------------------------
# Nadaraya-Watson for component
# --------------------------------------------------------

nw_one_component <- function(x, X_train, Y_train, h, A){
  X_train <- as.numeric(X_train)
  Y_train <- as.numeric(Y_train)
  A <- as.numeric(A)
  
  numerator <- sum(A * Y_train * Epanechnikov((x - X_train)/h))
  denominator <- sum(A * Epanechnikov((x - X_train)/h))
  return(numerator/denominator)
}


nw_all_components <- function(x, X_train, Y_train, h, A){
  M <- ncol(A)
  result <- sapply(1:M, 
                   function(i, x, X_train, Y_train, h, A){
                     return(nw_one_component(x, X_train, Y_train, h, A[, i]))
                   }, x = x, X_train = X_train, Y_train = Y_train, h = h, A = A )
  return(result)
}
