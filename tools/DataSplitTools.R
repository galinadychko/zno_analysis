
train_test_split <- function(df, ratio=0.80){
  nrows <- nrow(df)
  nsample <- round(nrows * ratio)
  id <- sample(nrows, nsample, replace = FALSE)
  return(list(train = df[id, ], test = df[!(1:nrows %in% id), ]))
}
