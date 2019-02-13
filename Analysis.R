source("tools/DataSplitTools.R")
source("tools/GeneralizedNadarayaWatson.R")


df <- read.csv2(file = "/home/administrator/workplace/univ/ZNOandVoating/input.csv", 
                header = FALSE, sep = ";", dec = ",")
names(df) <- c("ukr","math", "pro-ukr", "radical", "oposition", "small", "not_voted")
head(df)

cv_split <- cross_validation_split(df)

X <- as.vector(cv_split[[1]][, "math"])
Y <- as.vector(cv_split[[1]][, "ukr"])
W <- as.matrix(cv_split[[1]][, -c(1, 2)])

Xtest <- as.vector(cv_split[[2]][, "math"])

gnw <- GeneralisedNadarayaWatson$new()
gnw$train(X, Y, W)
gnw$run_cluster()

system.time({
  pr1 <- gnw$predict_in_parallel(Xtest, 1)
})

gnw$stop_cluster()

# system.time({
#   pr2 <- gnw$predict(Xtest, 1)
# })
# 

rownames(pr1) <- rownames(cv_split[[2]])
head(pr1)

W2 <- cv_split[[2]][, -c(1, 2)]
head(W2)

W2 <- as.data.frame(W2)

W2[, "max_value"] <- apply(W2, 1, max)

pro_ukr <- cv_split[[2]][W2[, "pro-ukr"] == W2["max_value"], ]
head(pro_ukr)


plot(as.vector(pro_ukr[, "math"]), as.vector(pro_ukr[, "ukr"]))
points(pro_ukr[, "math"], pr1[rownames(pro_ukr), 1], col = "red")

points(Xtest, as.vector(pr1[, 1]), col = "red")


plot(Xtest, as.vector(cv_split[[2]][, "ukr"]))
points(Xtest, as.vector(pr1[, 1]), col = "red")

points(Xtest, as.vector(pr1[, 2]), col = "green")

points(Xtest, as.vector(pr1[, 3]), col = "blue")

points(Xtest, as.vector(pr1[, 4]), col = "pink")

points(Xtest, as.vector(pr1[, 5]), col = "yellow")

all_plots <- list()
annotations_list <- list()

for (name in names(W[, -ncol(W)])) {
  print(name)
  d <- df[W[, name] == W["max_value"], ]
  print(nrow(d))
  # all_plots[[name]] <- plot_ly(x = d[, "math"], y = d[, "ukr"], mode = "markers", type = "scatter", name=name) 
  # annotations_list <- append(annotations_list, list(x = 0.2 , y = 1.05, text = name, showarrow = F, xref='paper', yref='paper'))
}
# plotly::subplot(all_plots[["pro_ukr"]], all_plots[["not_voted"]]) %>% 
#   layout(annotations = list(
#     list(x = 0.2 , y = 1.05, text = name, showarrow = F, xref='paper', yref='paper'),
#     list(x = 0.8 , y = 1.05, text = name, showarrow = F, xref='paper', yref='paper'))
#   )
# 


library(WVPlots)
ScatterHist(diamonds, "price", "carat")