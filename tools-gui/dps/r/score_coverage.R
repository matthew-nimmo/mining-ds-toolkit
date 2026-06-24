score_coverage <- function(x) {
  if (!is(x, "score")) {
    return(NULL)
  }
  
  # Not implemented.
  z <- rep(NA, length(x))
  x$score[["coverage"]] <- z
  
  return(x)
}