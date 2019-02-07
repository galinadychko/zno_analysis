library(R6)
source("tools/NadarayaWatson.R")
source("tools/MVCweights.R")


GeneralisedNadarayaWatson <- R6Class("GeneralisedNadarayaWatson", 
                                     public = list(
                                       X_train = NULL,
                                       Y_train = NULL,
                                       A = NULL,
                                       initialize = function(){
                                         self$X_train <- NULL
                                         self$Y_train <- NULL
                                         self$A <- NULL
                                       },
                                       train = function(X_train, Y_train, w_coeff){
                                         self$X_train <- X_train
                                         self$Y_train <- Y_train
                                         self$A <- acoeff(w_coeff)
                                       }, 
                                       predict = function(X_test, h){
                                         results <- lapply(X_test, 
                                                           function(x, h){
                                                             res <- nw_all_components(x, self$X_train, self$Y_train, h, self$A)
                                                             return(res)
                                                           }, h = h)
                                         results <- do.call(rbind, results)
                                         return(results)
                                       }
                                     )
)