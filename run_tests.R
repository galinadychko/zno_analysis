library(testthat) 

source("tools/MVCweights.R")
source("tools/NadarayaWatson.R")

test_results <- test_dir("tests", reporter = "summary")
