score_accuracy <- function(x) {
  if (!is(x, "score")) {
    return(NULL)
  }

  # Not implemented.
  z <- rep(NA, length(x))
  x$score[["accuracy"]] <- z

  return(x)
}