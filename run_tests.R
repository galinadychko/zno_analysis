library(testthat) 

source("tools/MVCweights.R")
source("tools/NadarayaWatson.R")
source("tools/GeneralizedNadarayaWatson.R")
source("tools/DataSplitTools.R")
source("tools/CommonTools.R")
source("tools/metrics.R")

test_results <- test_dir("tests", reporter = "summary")
