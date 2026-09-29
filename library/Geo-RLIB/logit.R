logit <- function(x) {
  y <- log(x / (1-x))
  return(y)
}

inv_logit <- function(x) {
  y <- 1 / (1 + exp(-x))
  return(y)
}
