detect_colnames <- function(x) {
  stopifnot(is.data.frame(x))

  p <- apply(x, 1, \(y) {
    n <- length(y)
    p1 <- sum(grepl("^$", y)) / n
    i <- duplicated(y) | duplicated(rev(y))
    p2 <- sum(i) / n
    p3 <- sum(grepl("^[[:alpha:][:punct:]]", y)) / n
    (1 - p1) * (1 - p2) * p3
  })

  i <- which.max(p)
  if (p[i] < 0.1) {
    return(NULL)
  }

  z <- as.character(x[i, ])
  blanks <- z == ""
  blanks <- ifelse(is.na(blanks), FALSE, blanks)
  z0 <- paste0("V", 1:length(z))
  z[blanks] <- z0[blanks]
  attr(z, "prob") <- p[i]
  attr(z, "rowid") <- as.numeric(i)

  return(z)
}