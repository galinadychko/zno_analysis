## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------

d <- wrapr::build_frame(
   "model_id"  , "measure", "value" |
     1         , "AUC"    , 0.7     |
     1         , "R2"     , 0.4     |
     2         , "AUC"    , 0.8     |
     2         , "R2"     , 0.5     )

knitr::kable(d)

## ------------------------------------------------------------------------
library("cdata")

transform <- rowrecs_to_blocks_spec(
  wrapr::qchar_frame(
    "measure", "value" |
    "AUC"    , AUC     |
    "R2"     , R2      ),
  recordKeys = "model_id")

print(transform)

## ------------------------------------------------------------------------
knitr::kable(d)

d2 <- d %//% t(transform)

knitr::kable(d2)

## ------------------------------------------------------------------------
knitr::kable(d2)

d3 <- d2 %**% transform

knitr::kable(d3)

## ------------------------------------------------------------------------
knitr::kable(d)

# identity
d4 <- d %//% t(transform) %**% transform

knitr::kable(d4)

## ------------------------------------------------------------------------
# reverse or adjoint/transpose operation specification
t_record_spec <- t(transform)

d %.>% 
  t_record_spec %.>%
  knitr::kable(.)

# using dot-pipe's bquote style .() execute immediate notation
d %.>% 
  .(t(transform)) %.>%
  knitr::kable(.)

# identity
d %.>% 
  .(t(transform)) %.>% 
  transform %.>%
  knitr::kable(.)

## ------------------------------------------------------------------------
have_db <- requireNamespace("DBI", quietly = TRUE) &&
   requireNamespace("RSQLite", quietly = TRUE)

## ----eval=have_db--------------------------------------------------------
raw_connection <- DBI::dbConnect(RSQLite::SQLite(), 
                                 ":memory:")
RSQLite::initExtension(raw_connection)
db <- rquery::rquery_db_info(
  connection = raw_connection,
  is_dbi = TRUE,
  connection_options = rquery::rq_connection_tests(raw_connection))

d_td <- rquery::rq_copy_to(db, "d", d)

## ----eva=have_db---------------------------------------------------------
ops <- d_td %//% t(transform)
cat(format(ops))

rquery::execute(db, ops) %.>%
  knitr::kable(.)

d_td %.>% 
  .(t(transform)) %.>%
  rquery::execute(db, .) %.>%
  knitr::kable(.)

## ----eval=have_db--------------------------------------------------------
DBI::dbDisconnect(raw_connection)

