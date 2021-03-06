# -------------------
# Gramm matrix 
# -------------------

Gramm_matrix <- function(weights_matrix){
  flag <- try(is.matrix(weights_matrix))
  if (flag == FALSE) {stop("Not appropriate input format")}
  if (ncol(weights_matrix) < 2) {stop("Not correct mixture")}
  nrows <- nrow(weights_matrix)
  prod_ <- t(weights_matrix) %*% weights_matrix
  return((prod_)/nrows)
}


# -------------------
# a - weighted coeff
# -------------------

minor <- function(A, i, j){
  flag <- try(is.matrix(A))
  if (flag == FALSE) {stop("Not appropriate input format")}
  if (ncol(A) < 2 | nrow(A) < 2) {stop("Not correct dimension of input matrix")}
  if (i <= 0 | j <= 0) {stop("Not correct i,j")}
  return(det(as.matrix(A[-i, -j])))
}


all_matrix_minors <- function(matrix_){
  flag <- try(is.matrix(matrix_))
  if (flag == FALSE) {stop("Not appropriate input format")}
  nrows <- nrow(matrix_)
  ncols <- ncol(matrix_)
  m <- matrix(nrow = ncols, ncol = ncols)
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      m[i, j] <- minor(matrix_, i, j)
    }
  }
  return(m)
}


minus_one <- function(m_components){
  if (m_components == 1) {stop("Not correct number of components")}
  m <- matrix(-1, m_components, m_components)
  power_matrix <- sapply(1:m_components, 
                         function(x){
                           return(rep(x, m_components) + (1:m_components))
                         })
  return(m^power_matrix)
}


acoeff <- function(weights_matrix){
  G <- Gramm_matrix(weights_matrix)
  detG <- det(G)
  if (detG == 0) {stop("Devision by zero")} 
  minus_ones <- minus_one(ncol(G))
  all_min <- all_matrix_minors(G)
  prod <- weights_matrix %*% (t(all_min) * minus_ones)
  return(prod / detG)
}
