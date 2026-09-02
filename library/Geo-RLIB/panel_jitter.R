panel_jitter <- function(x, y, ...)
{
  points(jitter(x), jitter(y), pch=20, col="black", cex=1.2)
  try(lines(loess.smooth(x, y, span=1), col="red", lwd=1, lty="dashed"), silent=TRUE)
}
