histplot <- function(x,xlab=ToLabel(x),main=NULL,sub=NULL,col=NULL,col.grid='lightgray',border=par("fg"),breaks="Scott",xlim=NULL,log=NULL)
{
	u <- x[!is.na(x)]
	y <- u

	if(is.null(log))
	{
		if(abs(skew(y)) > 2)
		{
			log <- T
		}
		else
			log <- F
	}

	if(is.null(xlim))
		xlim <- range(u)

	if(log)
	{
		y <- log(y[y>0])
		xlim[xlim<=0] <- 0.001
		xlim <- log(xlim)
	}

	h <- hist(y,plot=F,breaks=breaks)
	n <- length(h$breaks)

	# Plot histogram

	plot.new()
	plot.window(xlim=xlim,ylim=c(0,max(h$counts)))
	box()

	rect(xleft=h$breaks[1:(n-1)],ybottom=0,xright=h$breaks[2:n],ytop=h$counts,col=col,border=border)

	if(log)
	{
		usr <- exp(par("usr"))
		us <- c(floor(usr[1]),ceiling(usr[2]))
		s <- (us[1]-3):(us[2]+1)
		atTicks1 <- unlist(lapply(s,FUN=function(x) log(10^x)))
		atLabels <- atTicks1[seq(1,length(atTicks1),by=2)]
		axis(1,labels=exp(atTicks1),at=atTicks1,tick=F)
		atTicks2 <- unlist(lapply(s,FUN=function(x) log(1:9*10^x)))
		axis(1,labels=F,at=atTicks2[atTicks2>=min(atTicks1) & atTicks2<=max(atTicks1)])
		xlab <- paste(xlab,"(log scale)",sep=" ")
	}
	else
		axis(1)
	axis(2)

	grid(NA,NULL,col=col.grid)

	title(main=main,xlab=xlab,ylab="Frequency")
	mtext(sub,side=3,line=0.5,font=par("font.sub"),cex=par("cex.sub"),adj=0.5)

	# Plot boxplot under histogram

	plot.window(xlim=xlim,ylim=c(0,1))
	boxplot(y,add=T,outline=F,range=1,horizontal=T,axes=F,at=-0.02,boxwex=0.03,staplewex=1,col="ivory3")

	# Plot stats

	plot.window(xlim=c(0,1),ylim=c(0,1),log="",asp=NA)
	l <- list("Samples","Missing"," ","Min","Mean","Median","Max","Variance","Std Dev","CV")
	cols <- c(rep("black",4),"blue",rep("black",times=4),"red")
	usr <- par("usr")

	sv <- vector(length=10)
	sv[1] <- length(x)
	sv[2] <- length(x[is.na(x)])
	sv[3] <- " "
	sv[4] <- min(u)
	sv[5] <- round(mean(u),3)
	sv[6] <- round(median(u),3)
	sv[7] <- max(u)
	sv[8] <- round(var(u),3)
	sv[9] <- round(sd(u),3)
	sv[10] <- round(sd(u)/mean(u),3)

	v <- 1.2 * strheight('W')
	vv <- 1.2 * strwidth("variance")
	t1 <- xlim[1] + 0.5 * (xlim[2] - xlim[1])
	t2 <- h$breaks[h$counts==max(h$counts,na.rm=T)]
	if(median(y) > t1 & any(t2 > t1))
		p <- vv
	else
		p <- 1 - max(strwidth(sv))

	text(p,seq(1,1-(9*v),by=-v),sv,adj=0,cex=0.8*par('cex'),col=cols)
	p <- p - vv
	text(p,seq(1,1-(9*v),by=-v),l,adj=0,cex=0.8*par('cex'),col=cols)
}
