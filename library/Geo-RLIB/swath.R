swath <- function(mdx, mdy, dhx, dhy, mdw=NULL, dhw=NULL, spacing=25, main="Swath plot", xlab="Section (m)", ylab="Grade", png_file=NULL)
{
	halfspacing <- spacing/2
	mdx <- as.integer((mdx+halfspacing)/spacing)*spacing
	dhx <- as.integer((dhx+halfspacing)/spacing)*spacing

	if (!is.null(dhw))
	{
		k <- as.data.frame(cbind(dhy, dhw))
		u <- split(k, dhx)
		y1 <- sapply(u, function(v) wmean(v[[1]], v[[2]], na.rm=TRUE))
		h <- sapply(u, function(v) length(v[,1]))
	}
	else
	{
		u <- split(dhy, dhx)
		y1 <- sapply(u, function(v) mean(v, na.rm=TRUE))
		h <- sapply(u, function(v) length(v))
	}
	x1 <- as.integer(names(y1))

	if (!is.null(mdw))
	{
		k <- as.data.frame(cbind(mdy, mdw))
		u <- split(k, mdx)
		y2 <- sapply(u, function(v) wmean(v[[1]], v[[2]], na.rm=TRUE))
	}
	else
	{
		u <- split(mdy, mdx)
		y2 <- sapply(u, function(v) mean(v, na.rm=TRUE))
	}
	x2 <- as.integer(names(y2))

	limx <- c(min(x1,x2,na.rm=TRUE),max(x1,x2,na.rm=TRUE))
	limy <- c(min(y1,y2,na.rm=TRUE),max(y1,y2,na.rm=TRUE))

	if (!is.null(png_file))
		png(png_file)

	op <- par()
	nf <- layout(matrix(c(1,2),2,1,byrow=TRUE), c(4,4), c(3,1), TRUE)

	par(mar=c(-0.1,4,4,2)+0.1)

	plot(x1,y1,type="l",col="blue",ylab=ylab,xlim=limx,ylim=limy,xaxt='n',main=ylab)
	mtext(main,side=3,line=0.5,cex=0.8)
	lines(x2,y2,type="l",col="red")
	legend("topright",c("Drillholes","Model"),lty=c(1,1),col=c("blue","red"),cex=0.8)
	grid(col="grey",lty=3)

	par(mar=c(5,4,-0.1,2)+0.1)
	plot(x1,h,ylab="Samples",xlim=limx,type='h',xlab=xlab,lwd=2,col="blue")
	grid(col="grey",lty=3)

	if (!is.null(png_file))
		dev.off()
}
