detect_rownames <- function(x) {
  stopifnot(is.data.frame(x))

  p <- apply(x, 2, \(y) {
    n <- length(y)
    i <- grepl("^$", y[1])
    m <- duplicated(y) | duplicated(rev(y))
    i <- c(i, !m)
    sum(i) / (n+1)
  })

  i <- which.max(p)
  if (p[i] < 0.8) {
    return(NULL)
  }

  z <- as.character(x[, i])
  attr(z, "prob") <- p[i]
  attr(z, "rowid") <- as.numeric(i)

  return(z)
}