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
                                       train = function(X_train, Y_train, W_train){
                                         if (!is.vector(X_train, mode = "numeric") | 
                                             !is.vector(Y_train, mode = "numeric") | 
                                             !is.matrix(W_train))
                                           {stop("Not correct class attributes")}
                                         
                                         if (any(c(length(X_train), length(Y_train), nrow(W_train)) != 
                                                 rep(length(X_train), 3)))
                                           {stop("Not correct input dimensions")}
                                         self$X_train <- X_train
                                         self$Y_train <- Y_train
                                         self$A <- acoeff(W_train)
                                       }, 
                                       predict = function(X_test, W_test, h){
                                         if (is.numeric(X_test) != TRUE | 
                                             is.numeric(h) != TRUE | 
                                             is.matrix(W_test) != TRUE)
                                           {stop("Not correct class attributes")}
                                         if (is.null(self$X_train) | 
                                             is.null(self$Y_train) |
                                             is.null(self$A))
                                           {stop("The model was not trained correctly")}
                                         if (!is.null(self$A))
                                           {if (any(is.na(self$A))) {stop("The model coefficients are not numbers")}}
                                         if (length(X_test) != nrow(W_test))
                                           {stop("Not correct input dimensions")}
                                         results <- sapply(X_test, 
                                                           function(x, h){
                                                             res <- nw_any_components(x, self$X_train, self$Y_train, h, self$A)
                                                             return(res)
                                                           }, h = h)
                                         return(list("prediction" = t(results), 
                                                     "A_test" = tryCatch(acoeff(W_test),
                                                                         error = function(e){
                                                                           return(paste0("acoeff caused the error while predict: '", e, "'"))
                                                                           })))
                                       },
                                       predict_in_parallel = function(X_test, W_test, h){
                                         if (is.numeric(X_test) != TRUE | 
                                             is.numeric(h) != TRUE |
                                             is.matrix(W_test) != TRUE)
                                           {stop("Not correct class attributes")}
                                         if (is.null(self$X_train) | 
                                             is.null(self$Y_train) |
                                             is.null(self$A))
                                           {stop("The model was not trained correctly")}
                                         if (!is.null(self$A))
                                           {if (any(is.na(self$A))) {stop("The model coefficients are not numbers")}}
                                         if (length(X_test) != nrow(W_test))
                                           {stop("Not correct input dimensions")}
                                         n_rows <- length(X_test)
                                         list_of_parts <- split_k_parts(k = self$max_threads, nrows = n_rows)
                                         res <- foreach(each_part = list_of_parts,
                                                        .combine = list,
                                                        .multicombine = TRUE,
                                                        .export = c("self", "W_test", "h")) %dopar% {
                                                          pr <- self$predict(X_test[each_part],
                                                                             matrix(W_test[each_part, ], nrow = length(each_part)), h)
                                                          pr$prediction
                                                          }
                                          return(list("prediction" = do.call(rbind, res), 
                                                      "A_test" = acoeff(W_test)))
                                        },
                                       stop_cluster = function(){
                                         stopImplicitCluster()
                                       },
                                       run_cluster = function(max_threads = self$max_threads){
                                         self$max_threads = max_threads
                                         registerDoParallel(self$max_threads)
                                       }
                                     )
)
