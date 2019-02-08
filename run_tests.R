library(testthat) 

source("tools/MVCweights.R")
source("tools/NadarayaWatson.R")
source("tools/GeneralizedNadarayaWatson.R")

test_results <- test_dir("tests", reporter = "summary")
