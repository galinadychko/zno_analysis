library(R6)
library(foreach)
library(doParallel)

max_threads <- detectCores() - 1

source("tools/NadarayaWatson.R")
source("tools/MVCweights.R")
source("tools/CommonTools.R")


GeneralisedNadarayaWatson <- R6Class("GeneralisedNadarayaWatson", 
                                     public = list(
                                       X_train = NULL,
                                       Y_train = NULL,
                                       A = NULL,
                                       max_threads = max_threads,
                                       initialize = function(){
                                         self$X_train <- NULL
                                         self$Y_train <- NULL
                                         self$A <- NULL
                                         self$max_threads <- max_threads
                                       },
                                       train = function(X_train, Y_train, w_coeff){
                                         if (!is.vector(X_train) | 
                                             !is.vector(Y_train) | 
                                             !is.matrix(w_coeff)) 
                                         {stop("Not correct class atributes")}
                                         
                                         if (any(c(length(X_train), length(Y_train), nrow(w_coeff)) != 
                                                 rep(length(X_train), 3)))
                                         {stop("Not correct input dimensions")}
                                         self$X_train <- X_train
                                         self$Y_train <- Y_train
                                         self$A <- acoeff(w_coeff)
                                       }, 
                                       predict = function(X_test, h){
                                         if (is.numeric(X_test) != TRUE | is.numeric(h) != TRUE)
                                         {stop("Not correct class atributes")}
                                         if (is.null(self$X_train) | 
                                             is.null(self$Y_train) |
                                             is.null(self$A))
                                         {stop("The model was not trained correctly")}
                                         if (!is.null(self$A)) 
                                         {if (any(is.na(self$A))) {stop("The model coefficients are not numbers")}}
                                         results <- sapply(X_test, 
                                                           function(x, h){
                                                             res <- nw_any_components(x, self$X_train, self$Y_train, h, self$A)
                                                             return(res)
                                                           }, h = h)
                                         return(t(results))
                                       },
                                       predict_in_parallel = function(X_test, h){
                                         n_rows <- length(X_test)
                                         list_of_parts <- split_k_parts(k = self$max_threads, nrows = n_rows)
                                         res <- foreach(each_part = list_of_parts, 
                                                        .combine = list, 
                                                        .multicombine = TRUE,
                                                        .export = "self") %dopar% {
                                                          self$predict(X_test[as.vector(each_part)], h)
                                                          }
                                          return(do.call(rbind, res))
                                        },
                                       stop_cluster = function(){
                                         stopImplicitCluster()
                                       },
                                       run_cluster = function(){
                                         registerDoParallel(self$max_threads)
                                       }
                                     )
)
