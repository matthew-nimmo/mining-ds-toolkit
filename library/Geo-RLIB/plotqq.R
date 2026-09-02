plotqq <- function(x,y,main="QQ-Plot",sub=NULL,xlab=ToLabel(x),ylab=ToLabel(y),lim,plot.stats=T,cex=0.6,pch=19)
{
	single <- FALSE
	x <- na.exclude(x)

	if (is.null(y))
	{
		single <- TRUE
		y <- x
		k <- qqnorm(y,plot.it=F)
	}
	else
	{
		y <- na.exclude(y)
		k <- qqplot(x,y,plot.it=F)
	}

	if(missing(lim))
	{
		lim <- c(min(k$x,k$y), max(k$x,k$y))
	}

	g <- function() {plotaxis(1); plotaxis(2)}

	plot(k,xlim=lim,ylim=lim,main=main,xlab=xlab,ylab=ylab,panel.first=g(),col="lightgray",type="l",lwd=0.4,pty='s')
	points(k,cex=cex,pch=pch)

	if(plot.stats)
	{
		u <- par("usr")
		cex <- 0.8*par("cex")
		rx <- u[2]-u[1]
		ry <- u[2]-u[1]
		spc <- 1.5*strheight("W",cex=cex)

		if (single)
		{
			qqline(x,col="blue")
		}
		else
		{
			eq <- glm(k$y ~ k$x)

			abline(a=0,b=1,col='blue',pch=19,cex=0.2)
			abline(a=eq$coefficients[1],b=eq$coefficients[2],col='red',lty='dashed')

			#points(quantile(x,c(.25,.75)),quantile(y,c(.25,.75)),pch=16,col=2,cex=1)

			# Plot y stats
			text(u[1]+0.05*rx,u[1]+0.95*ry,ylab,adj=0,cex=cex)
			lab <- c("n:","mean:","median:","sd:")
			stx <- c(length(y),mean(y),median(y),sd(y))
			stx <- formatC(stx,digits=2,format="f")
			loc <- u[3] + (0.95*ry - 1:4*spc)
			text(u[1]+0.05*rx,loc,lab,adj=0,cex=cex); text(u[1]+0.25*rx,loc,stx,adj=1,cex=cex)
		}

		# Plot x stats
		text(u[1]+0.75*rx,u[3]+0.18*ry,xlab,adj=0,cex=cex)
		lab <- c("n:","mean:","median","sd")
		stx <- c(length(x),mean(x),median(x),sd(x))
		stx <- formatC(stx,digits=2,format="f")
		loc <- u[3] + (0.18*ry - 1:4*spc)
		text(u[1]+0.75*rx,loc,lab,adj=0,cex=cex); text(u[1]+0.95*rx,loc,stx,adj=1,cex=cex)
	}

	mtext(sub,side=3,line=0.5,font=par("font.sub"),cex=par("cex.sub"),adj=0.5)
}
