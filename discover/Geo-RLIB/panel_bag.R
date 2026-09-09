panel_bag <- function(x, y, ...)
{
  i <- complete.cases(cbind(x, y))
  x <- x[i]
  y <- y[i]

  z <- densCols(x, y, colramp=colorRampPalette(c("black", "white")))
  cols <- viridis(256)

  df <- data.frame(x, y)
  df$dens <- col2rgb(z)[1,] + 1L
  df$col <- cols[df$dens]
  df$pch <- 20

  bg <- try(bagplot(cbind(x, y), na.rm=TRUE, create.plot=FALSE), silent=TRUE)
  if (class(bg)=="try-error") {
    bg <- bagplot(cbind(x, y), na.rm=TRUE, create.plot=FALSE, approx.limit=0.9*length(x))
  }
  polygon(bg$hull.loop, border=NA, col="gray90")
  polygon(bg$hull.bag, border=NA, col="gray80")
  out <- (x %in% bg$pxy.outlier[,1]) & (y %in% bg$pxy.outlier[,2])
  if (any(out)) {
    df$pch[out] <- 3
    df$col[out] <- "black"
  }

  o <- order(df$pch, -df$dens, decreasing=TRUE)
  df <- df[o,]

  points(y~x, data=df, pch=pch, col=col, cex=1.2)
  #lines(loess.smooth(x, y, span=1), col="black", lwd=1, lty="dashed")
}
