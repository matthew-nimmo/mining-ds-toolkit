detect_limit <- function(x) {
  stopifnot(is.data.frame(x))

  p <- apply(x, 1, \(y) {
    n <- length(y)
    p1 <- grepl("detect", tolower(y[1]))
    m <- sapply(y[-1], readr::guess_parser) == "double"
    p2 <- sum(m) / n
    p1 * p2
  })
  i <- which.max(p)
  if (p[i] < 0.8) {
    return(NULL)
  }

  z <- as.character(x[i, ])
  if (grepl("detect", tolower(z[1]))) {
    z[1] <- NA
  }
  attr(z, "prob") <- p[i]
  attr(z, "rowid") <- as.numeric(i)

  return(z)
}