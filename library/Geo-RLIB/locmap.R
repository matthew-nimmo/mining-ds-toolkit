locmap <- function(x,y,u,main=paste("Location Map of",ToLabel(z)),xlab=ToLabel(x),ylab=ToLabel(y),col="black")
{
  g <- function() {plotaxis(1); plotaxis(2)}

  zrange <- range(u, na.rm=T)
  r <- (zrange[2]-zrange[1])/nclass.Sturges(u)
  breaks <- floor((z-zrange[1])/r)
  breaks <- breaks + 1; breaks[is.na(breaks)] <- 1

  n <- length(unique(breaks))
  c <- colorRampPalette(c("blue","green","yellow","red","purple"))(n)

  plot(x,y,main=main,xlab=xlab,ylab=ylab,pch=20,col=c[unclass(breaks)],panel.first=g(),asp=1)

  t <- formatC(r * sort(unique(breaks-1)),digits=2, format="f")
  legend("topright",legend=t,col=c,pch=20,cex=par("cex")*0.7)
}
