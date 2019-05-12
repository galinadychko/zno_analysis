source("src/tools/CommonTools.R")


train_test_split <- function(df, ratio=0.80, random_seed=42){
  set.seed(random_seed)
  if (!is.matrix(df) & !is.data.frame(df) & !is.table(df)) {stop("Not correct input type")}
  if (nrow(df) < 2) {stop("Not correct input dimension")}
  nrows <- nrow(df)
  nsample <- round(nrows * ratio)
  id <- sample(nrows, nsample, replace = FALSE)
  return(list(train = df[id, ], test = df[!(1:nrows %in% id), ]))
}


cross_validation_split <- function(df, k=5){
  if (!is.matrix(df) & !is.data.frame(df) & !is.table(df)) {stop("Not correct input type")}
  if (nrow(df) < k) {stop("Not correct input dimension")}
  nrows <- nrow(df)
  df2 <- as.matrix(df[sample(nrows, nrows, replace = FALSE),], ncol = ncol(df))
  id_list <- split_k_parts(k, nrows)
  return(lapply(id_list, function(x){df2[x, ]}))
}
