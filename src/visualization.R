source("src/metrics.R")
source("src/tools/CommonTools.R")
source("src/GeneralizedModels/GeneralizedNadarayaWatson.R")
source("src/GeneralizedModels/GeneralizedLinearRegression.R")
source("src/GeneralizedModels/GeneralizedLocalLinearRegression.R")


Generalized_models_comparing <- function(x, x_train, y_train, w_train, x_test, y_test, w_test, h, h_llinear, i){
  sx_test <- sort(x_test)
  ox_test <- order(x_test)
  
  gnw <- GeneralisedNadarayaWatson$new()
  gnw$run_cluster()
  gnw$train(x_train, y_train, w_train, h[[x]][i])
  gnw_prediction <- gnw$predict_in_parallel(x_test, w_test, x)
  gnw$stop_cluster()
  gnw_wmse <- round(weighted_MSE(y_test, matrix(gnw_prediction$prediction), matrix(gnw_prediction$A_test[, x])), 2)
  gnw_stats <- characterStatistics(gnw_wmse, min(gnw_prediction$prediction), max(gnw_prediction$prediction))
  
  glm <- GeneralisedLinearRegression$new()
  glm$train(x_train, y_train, w_train)
  glm_prediction <- glm$predict(x_test, w_test, x)
  glm_wmse <- round(weighted_MSE(y_test, matrix(glm_prediction$prediction), matrix(glm_prediction$A_test[, x])), 2)
  glm_stats <- characterStatistics(glm_wmse, min(glm_prediction$prediction), max(glm_prediction$prediction))
  
  gllm <- GeneralisedLocalLinearRegression$new()
  gllm$run_cluster()
  gllm$train(x_train, y_train, w_train, h_llinear[[x]][i])
  gllm_prediction <- gllm$predict_in_parallel(x_test, w_test, x)
  gllm$stop_cluster
  gllm_wmse <- round(weighted_MSE(y_test, matrix(gllm_prediction$prediction), matrix(gllm_prediction$A_test[, x])), 2)
  gllm_stats <- characterStatistics(gllm_wmse, min(gllm_prediction$prediction), max(gllm_prediction$prediction))

  p <- plot_ly(x = sx_test, y=gnw_prediction$prediction[ox_test],
               mode = "lines", type = "scatter",
               line = list(color = 'rgb(205, 12, 24)', opacity=0.7, width = 4),
               name = "<b>GNW " %&% "(h=" %&% as.character(h[[x]][i]) %&% ")</b>\n" %&% gnw_stats) %>%
    add_trace(y = glm_prediction$prediction[ox_test],
              mode = "lines", type = "scatter", 
              line = list(color = 'rgb(22, 96, 167)', opacity=0.7, width = 4),
              name =  "<b>GLM</b> \n" %&% glm_stats) %>%
    add_trace(y = gllm_prediction$prediction[ox_test],
              mode = "lines", type = "scatter", 
              line = list(color = 'rgb(50,205,50)', opacity=0.7, width = 4),
              name =  "<b>GLLM</b> \n" %&% "(h=" %&% as.character(h_llinear[[x]][i]) %&% ")</b>\n" %&% gllm_stats) %>%
    layout(title = "<b>EIT models component #" %&% as.character(x) %&% "</b>\n(test data)")
  return(p)
}


dash_type <<- c("longdash", "dash", "dot")


GNW_comparing <- function(x, x_train, y_train, w_train, x_test, y_test, w_test, h, i, p){
  sx_test <- sort(x_test)
  ox_test <- order(x_test)
  gnw <- GeneralisedNadarayaWatson$new()
  gnw$run_cluster()
  gnw$train(x_train, y_train, w_train, h[[x]][i])
  gnw_prediction <- gnw$predict_in_parallel(x_test, w_test, x)
  gnw$stop_cluster()
  # gnw_wmse <- round(weighted_MSE(y_test, matrix(gnw_prediction$prediction), matrix(gnw_prediction$A_test[1:1000, x])), 2)
  # gnw_stats <- characterStatistics(gnw_wmse, min(gnw_prediction$prediction), max(gnw_prediction$prediction))
  
  if(is.null(p)){return(plot_ly(x = sx_test, y = gnw_prediction$prediction[ox_test],
                          name = 'GNW #1', type = 'scatter', mode = 'lines',
                          width = 800, height = 600))}
  else{
    p <- p %>% add_trace(y = gnw_prediction$prediction[ox_test], mode = 'lines',
                         line=list(dash=dash_type[x]),
                         name="GNW #" %&% as.character(x))
    return(p)
    }
}


GLLM_comparing <- function(x, x_train, y_train, w_train, x_test, y_test, w_test, h_llinear, i, p){
  sx_test <- sort(x_test)
  ox_test <- order(x_test)
  gllm <- GeneralisedLocalLinearRegression$new()
  gllm$run_cluster()
  gllm$train(x_train, y_train, w_train, h_llinear[[x]][i])
  gllm_prediction <- gllm$predict_in_parallel(x_test, w_test, x)
  gllm$stop_cluster()
  # gllm_wmse <- round(weighted_MSE(y_test, matrix(gllm_prediction$prediction), matrix(gllm_prediction$A_test[, x])), 2)
  # gllm_stats <- characterStatistics(gllm_wmse, min(gllm_prediction$prediction), max(gllm_prediction$prediction))
  
  if(is.null(p)){return(plot_ly(x = sx_test, y = gllm_prediction$prediction[ox_test],
                                name = 'GLLM #1', type = 'scatter', mode = 'lines',
                                width = 800, height = 600))}
  else{
    p <- p %>% add_trace(y = gllm_prediction$prediction[ox_test], mode = 'lines',
                         line=list(dash=dash_type[x]),
                         name="GLLM #" %&% as.character(x))
    return(p)
  }
}

GLM_comparing <- function(x, x_train, y_train, w_train, x_test, y_test, w_test, p){
  sx_test <- sort(x_test)
  ox_test <- order(x_test)
  glm <- GeneralisedLinearRegression$new()
  glm$train(x_train, y_train, w_train)
  glm_prediction <- glm$predict(x_test, w_test, x)
  # glm_wmse <- round(weighted_MSE(y_test, matrix(glm_prediction$prediction), matrix(glm_prediction$A_test[1:1000, x])), 2)
  # glm_stats <- characterStatistics(gnw_wmse, min(glm_prediction$prediction), max(glm_prediction$prediction))
  
  if(is.null(p)){return(plot_ly(x = sx_test, y = glm_prediction$prediction[ox_test],
                                name = 'GLM #1', type = 'scatter', mode = 'lines',
                                width = 800, height = 600))}
  else{
    p <- p %>% add_trace(y = glm_prediction$prediction[ox_test], mode = 'lines',
                         line=list(dash=dash_type[x]),
                         name="GLM #" %&% as.character(x))
    return(p)
  }
}


recursive.plot <- function(yy){
  if(yy==0){invisible()}
  if(yy==1){return(data_set %>% plot_ly(x = sort(data_set[, "x"]), y = data_set[order(data_set$x), "comp1"],
                                        name = 'Comp #1', type = 'scatter', mode = 'lines',
                                        width = 800, height = 600))}
  else{return(recursive.plot(yy-1) %>% add_trace(y = data_set[order(data_set$x), (yy+1)], name="Comp #" %&% as.character(yy), mode = 'lines'))}
}


characterStatistics <- function(WMSE_valuee, min_value, max_value){
  return("WMSE: " %&% as.character(round(WMSE_valuee, 2)) %&% "\n" %&%
         "min: " %&% as.character(round(min_value, 2)) %&% "\n" %&%
         "max: " %&% as.character(round(max_value, 2)))
}
