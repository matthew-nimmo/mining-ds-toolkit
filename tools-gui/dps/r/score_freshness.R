score_freshness <- function(x) {
  if (!is(x, "score")) {
    return(NULL)
  }
  
  # Not implemented.
  z <- rep(NA, length(x))
  x$score[["freshness"]] <- z
  
  return(x)
}