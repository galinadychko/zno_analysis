library(R6)
library(foreach)
library(doParallel)

max_threads <- detectCores() - 1

source("src/NadarayaWatson.R")
source("src/MVCweights.R")
source("src/tools/CommonTools.R")


GeneralisedNadarayaWatson <- R6Class("GeneralisedNadarayaWatson", 
                                     public = list(
                                       X_train = NULL,
                                       Y_train = NULL,
                                       A = NULL,
                                       h = NULL,
                                       max_threads = max_threads,
                                       random_seed = NULL,
                                       initialize = function(){
                                         self$X_train <- NULL
                                         self$Y_train <- NULL
                                         self$A <- NULL
                                         self$h <- NULL
                                         self$max_threads <- max_threads
                                         self$random_seed <- 42
                                       },
                                       train = function(X_train, Y_train, W_train, h){
                                         if (!is.vector(X_train, mode = "numeric") | 
                                             !is.vector(Y_train, mode = "numeric") | 
                                             !is.vector(h, mode = "numeric") | 
                                             !is.matrix(W_train))
                                           {stop("Not correct class attributes")}
                                         
                                         if (any(c(length(X_train), length(Y_train), nrow(W_train)) != 
                                                 rep(length(X_train), 3)))
                                           {stop("Not correct input dimensions")}
                                         self$X_train <- X_train
                                         self$Y_train <- Y_train
                                         self$A <- acoeff(W_train)
                                         self$h <- h
                                       }, 
                                       predict = function(X_test, W_test, comp_number=NULL){  
                                         if (is.numeric(X_test) != TRUE |
                                             is.matrix(W_test) != TRUE)
                                           {stop("Not correct class attributes")}
                                         if (is.null(self$X_train) | 
                                             is.null(self$Y_train) |
                                             is.null(self$A) |
                                             is.null(self$h))
                                           {stop("The model was not trained correctly")}
                                         if (!is.null(self$A))
                                           {if (any(is.na(self$A))) {stop("The model coefficients are not numbers")}}
                                         if ((length(X_test) != nrow(W_test)))
                                           {stop("Not correct input dimensions")}
                                         if (is.null(comp_number) == TRUE)
                                           {ncomp <- 1:ncol(W_test)}
                                         else if (comp_number > ncol(W_test)) {stop("Not correct component number")}
                                         else {ncomp <- comp_number}
                                         results <- sapply(X_test, 
                                                           function(x, h){
                                                             res <- nw_any_components(x, self$X_train, self$Y_train, h, self$A[, ncomp])
                                                             return(res)
                                                           }, h = self$h)
                                         return(list("prediction" = t(results), 
                                                     "A_test" = tryCatch(acoeff(W_test),
                                                                         error = function(e){
                                                                           return(paste0("Acoeff caused the error while predict: '", e, "'"))
                                                                           })))
                                       },
                                       predict_in_parallel = function(X_test, W_test, comp_number=NULL){
                                         if (is.numeric(X_test) != TRUE | 
                                             is.matrix(W_test) != TRUE)
                                           {stop("Not correct class attributes")}
                                         if (is.null(self$X_train) | 
                                             is.null(self$Y_train) |
                                             is.null(self$h) |
                                             is.null(self$A))
                                           {stop("The model was not trained correctly")}
                                         if (!is.null(self$A))
                                           {if (any(is.na(self$A))) {stop("The model coefficients are not numbers")}}
                                         if (length(X_test) != nrow(W_test))
                                           {stop("Not correct input dimensions")}
                                         if (is.null(comp_number) == TRUE)
                                           {ncomp <- 1:ncol(W_test)}
                                         else if (comp_number > ncol(W_test)) {stop("Not correct component number")}
                                         else {ncomp <- comp_number}
                                         n_rows <- length(X_test)
                                         set.seed(self$random_seed)
                                         list_of_parts <- split_k_parts(k = self$max_threads, nrows = n_rows)
                                         res <- foreach(each_part = list_of_parts,
                                                        .combine = list,
                                                        .multicombine = TRUE,
                                                        .export = c("X_test", "W_test", "ncomp", "self")
                                                        ) %dopar% {
                                                          M <- ncol(W_test)
                                                          X <- X_test[each_part]
                                                          W <- matrix(W_test[each_part, ], 
                                                                      nrow = length(each_part), ncol = M,
                                                                       dimnames = list(names(X), 1:M))
                                                          pr <- self$predict(X_test=X, W_test=W, comp_number=ncomp)
                                                          pr$prediction
                                                        }
                                         prediction <- tryCatch({pred <- do.call(cbind, res)},
                                                                error = function(cond){pred <- do.call(rbind, res)})
                                          return(list("prediction" = prediction,
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
