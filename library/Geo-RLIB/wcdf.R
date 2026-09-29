wcdf <- function(x, weight=NULL)
  UseMethod('wcdf', x)

wcdf.default <- function(x, weight=NULL)
{
	if (!is.numeric(x))
		stop(paste(sQuote("x"))," Needs to be numeric")

	if (is.null(weight))
	{
		x <- na.exclude(x)
		n <- length(x)

		if (n < 1)
			stop("'x' must have 1 or more non-missing values")

		x <- sort(x)
		y <- (1:n)/n
	}
	else
	{
		i <- !is.na(x)
		x <- x[i]
		w <- weight[i]
		ox <- order(x)
		x <- x[ox]
		w <- w[ox]
		y <- cumsum(w) / sum(w)
	}

	fn <- approxfun(x,y,method="constant",yleft=0,yright=1,f=0,ties="ordered")
	class(fn) <- c("wcdf","stepfun",class(fn))
	attr(fn,"weighted") <- !is.null(weight)
	attr(fn,"call") <- sys.call()

	fn
}

wcdf.formula <- function(x, data, weight=NULL)
{
	if (!is.data.frame(data))
		stop(paste(sQuote("data"))," Needs to be a data frame")

	vars <- get_all_vars(x, data)
	f <- vars[[2]]
	y <- split(vars[[1]],f)

	if(!is.null(weight))
	{
		w <- split(weight,f)
		value <- lapply(1:length(y),function(i) wcdf(y[[i]],weight=w[[i]]))
	}
	else
		value <- lapply(y,function(x) wcdf(x))

	value <- value[!is.na(value)]
	names(value) <- names(y)
	class(value) <- c("wcdf",class(value))
	attr(value,"formula") <- x

	value
}

points.wcdf <- function(object, show.all=T, pch=20, horizontal=T, log=TRUE, prob=TRUE, ...)
{
	xx <- knots(object)

	if(show.all)
		yy <- eval(expression(y),envir=environment(object))
	else
		yy <- object(xx)

	if (prob==TRUE)
		yy <- qnorm(yy)

	if(horizontal)
		points(xx,yy,pch=pch,...)
	else
		points(yy,xx,pch=pch,...)
}

plot.wcdf <- function(x, xlab="x", pch=20, cex=1, xlim=NULL, ylim=NULL, show.all=T, horizontal=T, log=TRUE, prob=TRUE, ...)
{
	plotcdf <- function(u)
	{
		xx <- knots(u)

		if (is.null(xlim))
			xlim <- range(xx,na.rm=T)
		if(xlim[1]<=0) xlim[1]=0.001

		if(show.all)
			yy <- eval(expression(y),envir=environment(x))
		else
			yy <- x(xx)

		if (prob)
		{
			yy <- qnorm(yy)
			ytype <- "prob"
			ylab="Cummulative Probability"
			ylim <- range(yy[is.finite(yy)])
			if(any(!is.finite(ylim)))
				ylim <- qnorm(0.001,0.999)
		}
		else
		{
			ytype <- NULL
			ylab <- "Cummulative Frequency"
			ylim <- c(0.0,1.0)
		}

		if (attr(u,"weighted")==TRUE)
			ylab <- paste(ylab,"(weighted)")

		if (log)
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
			xtype <- NULL
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

	if (is(x, "list"))
	{
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

		plot(x[[i[1]]],xlab=xlab,col=col[[i[[1]]]],cex=cex,xlim=rx,show.all=show.all,horizontal=horizontal,...)
		for (j in 2:length(i))
			points(x[[i[j]]],col=col[[i[[j]]]],show.all=show.all,cex=cex,horizontal=horizontal,...)

		legend("topleft",names(x),title=legtitle,col=col,pch=20,bty="o",bg="white",cex=par("cex.axis"))
	}
	else
	{
		plotcdf(x)
	}
}
