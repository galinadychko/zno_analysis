## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library("cdata")

d <- wrapr::build_frame(
  "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species" |
  5.1           , 3.5          , 1.4           , 0.2          , "setosa"  |
  4.9           , 3            , 1.4           , 0.2          , "setosa"  )
d$id <- seq_len(nrow(d))

knitr::kable(d)

## ------------------------------------------------------------------------
expect <- wrapr::build_frame(
  "id", "Species", "Part" , "Measure", "Value" |
  1L  , "setosa" , "Sepal", "Length" , 5.1     |
  1L  , "setosa" , "Sepal", "Width"  , 3.5     |
  1L  , "setosa" , "Petal", "Length" , 1.4     |
  1L  , "setosa" , "Petal", "Width"  , 0.2     |
  2L  , "setosa" , "Sepal", "Length" , 4.9     |
  2L  , "setosa" , "Sepal", "Width"  , 3       |
  2L  , "setosa" , "Petal", "Length" , 1.4     |
  2L  , "setosa" , "Petal", "Width"  , 0.2     )

knitr::kable(expect)

## ------------------------------------------------------------------------
control_table <- wrapr::qchar_frame(
  "Part",  "Measure", "Value"         |
  "Sepal", "Length" ,  "Sepal.Length" |
  "Sepal", "Width"  ,  "Sepal.Width"  |
  "Petal", "Length" ,  "Petal.Length" |
  "Petal", "Width"  ,  "Petal.Width"  )

## ------------------------------------------------------------------------
res <- rowrecs_to_blocks(
  d,
  control_table,
  controlTableKeys = c("Part", "Measure"),
  columnsToCopy = c("id", "Species"))

knitr::kable(res)

## ------------------------------------------------------------------------
back <- blocks_to_rowrecs(
  res,
  keyColumns = c("id", "Species"),
  control_table,
  controlTableKeys = c("Part", "Measure"))

knitr::kable(back)

