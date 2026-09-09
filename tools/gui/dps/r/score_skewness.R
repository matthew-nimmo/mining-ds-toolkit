score_skewness <- function(x) {
  if (!is(x, "score")) {
    return(NULL)
  }
  y <- lapply(x$profile, "[[", "fields")

  # Skewness & Kurtosis
  z1 <- sapply(y, \(u) {
    if (is.null(u)) {
      return(0)
    }
    if (length(u) == 0) {
      return(0)
    }
    z <- sapply(u, \(x) {
      pb <- x$skewness
      if (is.null(pb) || length(pb) == 0) {
        return(0)
      }
      z1 <- ifelse(abs(pb) > 1, 1, 0)

      pb <- x$kurtosis
      if (is.null(pb) || length(pb) == 0) {
        return(0)
      }
      z2 <- ifelse(abs(pb) > 3, 1, 0)

      return(z1 * z2)
    })
    z <- mean(z, na.rm=TRUE)
    return(z)
  })

  x$score[["skewness"]] <- 1 - z1
  
  return(x)
}