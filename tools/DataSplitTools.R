source("tools/GeneralizedNadarayaWatson.R")


train_test_split <- function(df, ratio=0.80){
  nrows <- nrow(df)
  nsample <- round(nrows * ratio)
  id <- sample(nrows, nsample, replace = FALSE)
  return(list(train = df[id, ], test = df[!(1:nrows %in% id), ]))
}

cross_validation_split <- function(df, k=5){
  nrows <- nrow(df)
  df2 <- df[sample(nrows, nrows, replace = FALSE),]
  
  sample_size <- ceiling(nrows/k)
  id_list <- split(1:(k*sample_size), rep(1:k, each = sample_size))
  id_list[[k]] <- id_list[[k]][id_list[[k]] <= nrows]
  return(lapply(id_list, function(x){df2[x, ]}))
}

cross_validation <- function(cv_split, func, parameters){
  k <- length(cv_split)
  cv_names <- names(cv_split)
  res <- lapply(1:k, 
                function(x){
                  test <- cv_split[[x]]
                  train <- do.call(rbind, cv_split[!x %in% cv_names])
                  Y_res <- lapply(test["x"], 
                                  function(x){
                                    return(nadaraya_watson(x, train["x"], train["y"], parameters["h"], parameters["A"]))
                                  })
                })
}