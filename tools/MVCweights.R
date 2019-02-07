# -------------------
# Gramm matrix 
# -------------------

GramN_new <- function(weights_matrix){
  nrows <- nrow(weights_matrix)
  prod_ <- t(weights_matrix) %*% weights_matrix
  return((prod_)/nrows)
}
