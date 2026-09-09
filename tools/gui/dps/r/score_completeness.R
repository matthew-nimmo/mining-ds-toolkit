score_completeness <- function(x) {
  if (!is(x, "score")) {
    return(NULL)
  }
  y <- lapply(x$profile, "[[", "fields")

  # Missing values
  z <- sapply(y, \(u) {
    if (is.null(u)) {
      return(0)
    }
    if (length(u) == 0) {
      return(0)
    }
    z <- sapply(u, \(x) {
      pb <- x$N_missing
      if (is.null(pb) || length(pb) == 0) {
        return(0)
      }
      n <- x$N
      if (is.null(n) || length(n) == 0) {
        return(0)
      }
      prev <- pb / n
      conf <- 1 - prev
      return(prev * conf)
    })
    z <- mean(z, na.rm=TRUE)
    return(z)
  })

  x$score[["completeness"]] <- 1 - z

  return(x)
}