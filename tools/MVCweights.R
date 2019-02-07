# -------------------
# Gramm matrix 
# -------------------

GramN_new <- function(weights_matrix){
  nrows <- nrow(weights_matrix)
  prod_ <- t(weights_matrix) %*% weights_matrix
  return((prod_)/nrows)
}


# -------------------
# a - weighted coeff
# -------------------

minor <- function(A, i, j){
  return(as.matrix(A[-i, -j]))
}


all_minors <- function(matrix_){
  nrows <- nrow(matrix_)
  ncols <- ncol(matrix_)
  m <- matrix(nrow = ncols, ncol = ncols)
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      print(minor(matrix_, i, j))
      m[i, j] <- det(minor(matrix_, i, j))
    }
  }
  return(m)
}
