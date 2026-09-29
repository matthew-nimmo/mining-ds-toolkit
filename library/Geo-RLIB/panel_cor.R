panel_cor <- function(x, y, digits=2, prefix="", cex.cor=1.6, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr=c(0, 1, 0, 1))
  r <- cor(x, y, use="complete.obs")
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  text(0.5, 0.5, txt, cex=1.3)
}
