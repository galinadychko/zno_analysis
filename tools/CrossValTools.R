source("tools/GeneralizedNadarayaWatson.R")
source("tools/metrics.R")
source("tools/CommonTools.R")


matrix_mean_std <- function(M){
  m <- colMeans(abs(M))
  std <- apply(M, 2, sd)
  return(t(data.frame("mean" = m, "std" = std)))
}


GNWcv_computation <- function(cv_split, X_colname, Y_colname, W_colname, h){
  k <- length(cv_split)
  cv_names <- names(cv_split)
  res <- lapply(1:2, 
                function(x, X_colname, Y_colname, W_colname, h){
                  print(x)
                  test <- cv_split[[x]]
                  X_test <- as.vector(test[, X_colname])
                  Y_test <- as.vector(test[, Y_colname])
                  W_test <- as.matrix(test[, W_colname])
                  remove(test)
                  
                  train <- do.call(rbind, cv_split[cv_names[-match(1, cv_names)]])
                  X_train <- as.vector(train[, X_colname])
                  Y_train <- as.vector(train[, Y_colname])
                  W_train <- as.matrix(train[, W_colname])
                  remove(train)
                  
                  inst <- GeneralisedNadarayaWatson$new()
                  inst$run_cluster()
                  inst$train(X_train, Y_train, W_train)
                  prediction <- inst$predict_in_parallel(X_test, W_test, h)
                  inst$stop_cluster()
                  
                  metrics_value <- weighted_MSE(Y_true = Y_test,
                                                Y_predicted = prediction$prediction,
                                                A_coeff = prediction$A_test)
                  
                  return(metrics_value)
                }, X_colname = X_colname, Y_colname = Y_colname, W_colname = W_colname, h = h)
  computation <- do.call(rbind, res)
  return(computation)
}


GNWcv_across_h <- function(h_range, cv_df_split, X_colname, Y_colname, W_colname){
  l <- lapply(h_range,
              function(h, cv_df_split, X_colname, Y_colname, W_colname){
                print("h = " %&% as.character(h))
                cv_computation <- GNWcv_computation(cv_df_split,
                                                      X_colname, Y_colname,
                                                      W_colname, h)
                cv_m_std <- matrix_mean_std(cv_computation)
                rownames(cv_m_std) <- c("mean_" %&% as.character(h), "std")
                return(cv_m_std)
              },
              cv_df_split = cv_df_split,
              X_colname = X_colname, Y_colname = Y_colname,
              W_colname = W_colname)
  names(l) <- as.character(h_range)
  res <- do.call(rbind, l)
  return(res)
}

indx_sorted_mean <- function(M, alpha=0.75){
  indx <- apply(M[grep("std", rownames(M)), ], 2,
                function(x, alpha){
                  id <- sort(x, index.return = TRUE)$ix[1:round(alpha * length(x))]
                  id <- 2*id - 1
                  return(id)
                }, alpha = alpha)
  return(indx)
  }


select_h <- function(M_indx, M){
  cut_mean <- sapply(1:ncol(M_indx), 
                     function(i, ind, df){
                       smallest_mean <- df[ind[, i], i][1]
                       name <- names(smallest_mean)
                       h_char <- substring(name, 6, nchar(name))
                       return(as.numeric(h_char))
                     }, 
                     ind = M_indx, df = M)
  return(cut_mean)
}


optimal_h <- function(M, alpha=0.75){
  indx_smean <- indx_sorted_mean(M, alpha)
  opt_h <- select_h(indx_smean, M)
  return(opt_h)
}