cdf <- function(x,weight=NULL,log=TRUE,prob=TRUE)
  UseMethod('cdf',x)

cdf.default <- function(x, weight=NULL, log=TRUE, prob=TRUE)
{
	if (!is.numeric(x))
		stop(paste(sQuote("x"))," Needs to be numeric")

	i <- !is.na(x)
	x <- x[i]
	n <- length(x)

	if (n <= 1)
		return(NA)

	if ((max(x,na.rm=T) - min(x,na.rm=T)) == 0)
		return(NA)

	if (min(x,na.rm=TRUE)<=0 & log==TRUE)
		log <- FALSE

	if (is.null(weight))
	{
		x <- sort(x)

		if(prob)
			y <- (1:n)/(n+1)
		else
			y <- (1:n)/n
	}
	else
	{
		w <- weight[i]
		ox <- order(x)
		x <- x[ox]
		w <- w[ox]
		y <- cumsum(w) / sum(w)
	}

	fn <- approxfun(x,y,method="constant",yleft=0,yright=1,f=0,ties="ordered")

	class(fn) <- c("cdf","stepfun",class(fn))
	attr(fn,"log") <- log
	attr(fn,"prob") <- prob
	attr(fn,"weighted") <- !is.null(weight)
	attr(fn,"call") <- sys.call()

	fn
}


# Format of formula: domain + wt ~ assay1 + assay2 + ... + assayn
cdf.formula <- function(x, data, weight=NULL, log=TRUE, prob=TRUE)
{
	if (!is.data.frame(data))
		stop(paste(sQuote("data"))," Needs to be a data frame")

	vars <- get_all_vars(x, data)
	f <- vars[[2]]
	y <- split(vars[[1]],f)

	if(!is.null(weight))
	{
		w <- split(weight,f)
		value <- sapply(1:length(y),function(i) cdf(y[[i]],weight=w[[i]],log=log,prob=prob))
	}
	else
		value <- sapply(y,function(x) cdf(x,log=log,prob=prob))

	#value <- value[!is.na(value)]
	names(value) <- names(y)
	class(value) <- c("cdf.list",class(value))
	attr(value,"formula") <- x

	value
}

points.cdf <- function(object, show.all=T, pch=20, horizontal=T, ...)
{
	xx <- knots(object)

	if(show.all)
		yy <- eval(expression(y),envir=environment(object))
	else
		yy <- object(xx)

	if (attr(object,"prob")==TRUE)
		yy <- qnorm(yy)

	if(horizontal)
		points(xx,yy,pch=pch,...)
	else
		points(yy,xx,pch=pch,...)
}

lines.cdf <- function(object, show.all=T, horizontal=T, ...)
{
	xx <- knots(object)

	if(show.all)
		yy <- eval(expression(y),envir=environment(object))
	else
		yy <- object(xx)

	if (attr(object,"prob")==TRUE)
		yy <- qnorm(yy)

	if(horizontal)
		lines(xx,yy,...)
	else
		lines(yy,xx,...)
}

plot.cdf <- function(x, xlab="x", pch=20, cex=1, xlim=NULL, ylim=NULL, show.all=T, horizontal=T, plot.line=F, ...)
{
	if (suppressWarnings(is.na(x)))
		return(NULL)

	xx <- knots(x)

	if (is.null(xlim))
		xlim <- range(xx,na.rm=T)
	if(xlim[1]<=0) xlim[1]=0.001

	if(show.all)
		yy <- eval(expression(y),envir=environment(x))
	else
		yy <- x(xx)
	if (attr(x,"prob")==TRUE)
	{
		yy <- qnorm(yy)
		ytype <- "prob"
		ylab="Cumulative Probability"
		if (is.null(ylim))
			ylim <- range(yy[is.finite(yy)])
		if(any(!is.finite(ylim)))
			ylim <- qnorm(c(0.0001,0.9999))
	}
	else
	{
		ytype <- NA
		ylab <- "Cummulative Frequency"
		ylim <- c(0.0,1.0)
	}

	if (attr(x,"weighted")==TRUE)
		ylab <- paste(ylab,"(weighted)")

	if (attr(x,"log")==TRUE)
	{
		logx <- "x"
		logy <- "y"
		xtype <- "log"
		xlab <- paste(xlab,"(log scale)",sep=" ")
	}
	else
	{
		logx <- ""
		logy <- ""
		xtype <- NA
	}

	if(horizontal)
	{
		g <- function() {plotaxis(1,xtype); plotaxis(2,ytype)}
		plot(xx,yy,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,log=logx,panel.first=g(),frame.plot=T,axes=F,pch=pch,cex=cex,...)
	}
	else
	{
		g <- function() {plotaxis(1,ytype); plotaxis(2,xtype)}
		plot(yy,xx,xlim=ylim,ylim=xlim,xlab=ylab,ylab=xlab,log=logy,panel.first=g(),frame.plot=T,axes=F,pch=pch,cex=cex,...)
	}
}

plot.cdf.list <- function(x, col=NULL, show.all=T, legend=T, cex=1, horizontal=T, plot.line=F, ...)
{
	if (all(is.na(x)))
		return(NULL)

	u <- x[!is.na(x)]

	class(u) <- c("cdf.list",class(u))
	attr(u,"formula") <- attr(x,"formula")
	x <- u

	mx <- lapply(x,function(y) max(knots(y)))
	i <- sort(as.numeric(mx),decreasing=T,index.return=T)$ix

	if (is.null(col))
		col <- rainbow(length(i))

	f <- attr(x, 'formula')
	xlab <- all.vars(f)[1]
	legtitle <- attr(terms(f),'term.labels')

	lx <- min(as.numeric(lapply(1:length(x),function(i) min(knots(x[[i]])))))
	hx <- max(as.numeric(lapply(1:length(x),function(i) max(knots(x[[i]])))))
	rx <- c(lx, hx)

	ly <- min(as.numeric(lapply(1:length(x),function(i) min(qnorm(eval(expression(y),envir=environment(x[[i]])))))))
	hy <- max(as.numeric(lapply(1:length(x),function(i) max(qnorm(eval(expression(y),envir=environment(x[[i]])))))))
	ry <- c(ly, hy)

	first <- 1
	for (j in 1:length(i))
	{
		if(first==1)
		{
			if (plot.line)
			{
				plot(x[[i[j]]],xlab=xlab,col=col[[i[[1]]]],cex=cex,xlim=rx,ylim=ry,show.all=show.all,horizontal=horizontal,type="l",...)
				points(x[[i[j]]],col=col[[i[[1]]]],show.all=show.all,cex=cex,horizontal=horizontal,...)
			}
			else
				plot(x[[i[j]]],xlab=xlab,col=col[[i[[1]]]],cex=cex,xlim=rx,ylim=ry,show.all=show.all,horizontal=horizontal,...)
			first <- 0
		}
		else
		{
			if (plot.line)
				lines(x[[i[j]]],col=col[[i[[j]]]],show.all=show.all,cex=0.8*cex,horizontal=horizontal,...)
			points(x[[i[j]]],col=col[[i[[j]]]],show.all=show.all,cex=cex,horizontal=horizontal,...)
		}
	}

	if(legend)
		legend("topleft",names(x),title=legtitle,col=col,pch=20,bty="o",bg="white",cex=0.8*par("cex.axis"))
}
