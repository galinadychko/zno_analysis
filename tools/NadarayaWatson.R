# --------------------------------------------------------
# Epanechnikov kernell
# --------------------------------------------------------


Epanechnikov <- function(x){
  result <- 3/4*(1 - x^2)*(abs(x) <= 1)
  return(result)
}
