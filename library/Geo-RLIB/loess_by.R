loess_by <- function(x, y, by, span=1, degree=1) {
  for (i in unique(by)) {
    j <- by == i
    if (sum(j) > 3) {
      z <- suppressWarnings(loess.smooth(x[j], y[j], span=span, degree=degree))
      lines(z, col="black", lwd=1, lty="dashed")
      text(tail(z$x,1), tail(z$y,1), labels=i, cex=0.5, pos=4, offset=0.2, xpd=TRUE)
    }
  }
}
