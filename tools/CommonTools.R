"%&%" <- function(x, y){paste(x, y, sep = "")}


split_k_parts <- function(k, nrows, random_seed){
  if (nrows < k) {stop("Number of observations is smaller than number of folds")}
  set.seed(random_seed)
  sample_size <- ceiling(nrows/k)
  id_list <- split(1:(k*sample_size), rep(1:k, each = sample_size))
  id_list[[k]] <- id_list[[k]][id_list[[k]] <= nrows]
  return(id_list)
}