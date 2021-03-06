library(R6)
library(foreach)
library(doParallel)

source("src/NadarayaWatson.R")
source("src/MVCweights.R")
source("src/tools/CommonTools.R")


GeneralisedLinearRegression <- R6Class("GeneralisedLinearRegression", 
                                       public = list(
                                         X_train = NULL,
                                         Y_train = NULL,
                                         A = NULL,
                                         b = NULL,
                                         initialize = function(){
                                           self$X_train <- NULL
                                           self$Y_train <- NULL
                                           self$A <- NULL
                                           self$b <- NULL
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
                                           A <- as.matrix(self$A)
                                           b_coeff <- apply(A, 2,
                                                            function(x){
                                                              n <- length(self$X_train)
                                                              X <- as.matrix(self$X_train); Y <- as.matrix(self$Y_train)
                                                              X <- cbind(rep(1, n), X)
                                                              A <- rbind(x, x)
                                                              b <- (((solve((t(X) * A) %*% X)) %*% t(X)) * A) %*% Y
                                                              names(b) <- c("b0", "b1")
                                                              return(b)
                                                            })
                                           self$b <- b_coeff
                                         }, 
                                         predict = function(X_test, W_test, comp_number=NULL){
                                           if (is.numeric(X_test) != TRUE | 
                                               is.matrix(W_test) != TRUE)
                                           {stop("Not correct class attributes")}
                                           if (is.null(self$X_train) | 
                                               is.null(self$Y_train) |
                                               is.null(self$b) |
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
                                           X <- rep(1, length(X_test))
                                           X <- cbind(X, X_test)
                                           results <- X %*% self$b[, ncomp]
                                           return(list("prediction" = results, 
                                                       "A_test" = tryCatch(acoeff(W_test),
                                                                           error = function(e){
                                                                             return(paste0("Acoeff caused the error while predict: '", e, "'"))
                                                                           })))
                                         }
                                       )
)
