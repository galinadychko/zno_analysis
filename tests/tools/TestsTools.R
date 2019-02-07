#--------------------------------
# primitive weights coefficients
#--------------------------------

wcoeff_two_components <- function(n){
  if (n == 0) {stop("Not valid number of observations")}
  v <- 1:n
  return(cbind(v/n, 1 - v/n))
}

wcoeff_three_components <- function(n){
  if (n == 0) {stop("Not valid number of observations")}
  v <- 1:n
  return(cbind(v/n, (1 - v/n)/2, (1 - v/n)/2))
}
