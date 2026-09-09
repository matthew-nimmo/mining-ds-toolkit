panel_points <- function(x, y, ...)
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

  o <- order(df$pch, -df$dens, decreasing=TRUE)
  df <- df[o,]

  points(y~x, data=df, pch=pch, col=col, cex=1.2)
}
