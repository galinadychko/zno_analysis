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
                                         if (is.numeric(X_train) != TRUE | 
                                             is.numeric(Y_train) != TRUE | 
                                             is.matrix(w_coeff) != TRUE) 
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
                                       }
                                     )
)
