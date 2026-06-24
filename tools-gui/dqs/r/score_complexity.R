score_complexity <- function(x) {
  if (!is(x, "score")) {
    return(NULL)
  }
  y <- lapply(x$profile, "[[", "table")

  # Number of columns * rows
  z <- sapply(y, \(u) {
    z1 <- min(u$ncol, 10) / 10
    z2 <- min(u$nrow, 40) / 40
    z <- z1 * z2
    return(z)
  })

  x$score[["complexity"]] <- z

  return(x)
}