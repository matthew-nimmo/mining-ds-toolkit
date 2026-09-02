theilsen <- function(x, y) {
  xx <- outer(x, x, "-")
  yy <- outer(y, y, "-")
  z <- yy / xx
  s  <- z[lower.tri(z)]
  slope <- median(s, na.rm=TRUE)
  intercept <- median(y, na.rm=TRUE) - slope*median(x, na.rm=TRUE)

  return(list(intercept, slope))
}