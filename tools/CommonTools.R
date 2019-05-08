"%&%" <- function(x, y){paste(x, y, sep = "")}

split_k_parts <- function(k, nrows){
  if (nrows < k) {stop("Number of observations is smaller than number of folds")}
  a <- 1:nrows
  kk <- nrows %/% k
  m <- nrows %% k
  l <- list()
  for(i in 0:(k-1)){
    l[[i+1]] <- a[(i * kk + min(i, m) + 1):((i+1) * kk + min(i + 1, m))]
  }
  return(l)
}
split_k_parts(k = 11, nrows = 100)
