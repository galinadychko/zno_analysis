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

nw_any_components <- function(x, X_train, Y_train, h, A){
  if (is.numeric(X_train) != TRUE | is.numeric(Y_train) != TRUE | 
      is.numeric(x) != TRUE) {stop("Not correct input type")}
  if (is.numeric(A) == TRUE) {A <- as.matrix(A, ncol = 1)}
  n_colms <- ncol(A)
  numerator <- colSums(matrix(A * Y_train * Epanechnikov((x - X_train)/h), ncol = n_colms))
  denominator <- colSums(matrix(A * Epanechnikov((x - X_train)/h), ncol = n_colms))
  
  if (sum(denominator) == 0) {stop("Devision by zero")}
  
  return(numerator/denominator)
}

# 
# nw_all_components <- function(x, X_train, Y_train, h, A){
#   if (is.matrix(A) != TRUE) {stop("Not correct input type")}
#   M <- ncol(A)
#   result <- sapply(1:M, 
#                    function(i, x, X_train, Y_train, h, A){
#                      return(nw_one_component(x, X_train, Y_train, h, A[, i]))
#                    }, x = x, X_train = X_train, Y_train = Y_train, h = h, A = A)
#   return(result)
# }
