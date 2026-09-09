histscatter <- function(x,y,log=FALSE,xlab=ToLabel(x),ylab=ToLabel(y),main=NULL,sub=NULL,cols=heat.colors)
{
  op <- par()
  on.exit(op)

  xhist <- hist(log(x),breaks="Scott",plot=FALSE)
  yhist <- hist(log(y),breaks="Scott",plot=FALSE)

  top <- max(c(xhist$counts,yhist$counts))
  nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE),c(5,1),c(1,5),TRUE)
  layout.show(nf)

  par(mar=c(5,4,1,1))
  g <- function() {plotaxis(1,"log"); plotaxis(2,"log")}
  plot(x,y,xlab=xlab,ylab=ylab,panel.first=g(),log="xy",axes=F,frame.plot=T,pch=21,bg="blue")
  par(mar=c(0,4,1,1))
  barplot(xhist$counts,axes=FALSE,ylim=c(0,top),space=0)
  par(mar=c(5,0,1,1))
  barplot(yhist$counts,axes=FALSE,xlim=c(0,top),space=0,horiz=TRUE)

  #title(main=main,xlab=xlab,ylab="Frequency")
  #mtext(sub,side=3,line=0.5,font=par("font.sub"),cex=par("cex.sub"),adj=0.5)
}
