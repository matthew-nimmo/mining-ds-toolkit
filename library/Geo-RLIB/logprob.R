#' log-probability plot.
#'
#' Generates a log-probability plot of numeric variable.
#'
#' For simple logprob plots, \code{\link{logprob.default}} will be used. However, there are
#' \code{logprob} methods for formula and list R objects.
#' @param x numeric vector.
#' @param ... other parameters.
#'
#' @return None
#'
#' @keywords logprob
#' @export
logprob <- function(x, ...) UseMethod('logprob')

#' log-probability plot.
#'
#' Generates a log-probability plot of numeric variable.
#'
#' @param x numeric vector.
#' @param horizontal a logical value indicating if the plot is rotated.
#' @param add a logical indicating if the plot should be added to the current logprob plot.
#' @param main a main title for the plot.
#' @param sub a sub title for the plot.
#' @param xlab a label for the x axis, defaults to description of x.
#' @param xlim the x limits (x1, x2) of the plot.
#' @param col the colour of the series.
#' @param pch plotting symbol.
#' @param cex symbol expansion.
#' @param type of the plot. see \code{\link{plot.default}}.
#' @param ... other parameters.
#'
#' @return None
#'
#' @keywords logprob.default
#' @export
logprob.default <- function(x, horizontal=TRUE, add=FALSE, main=NULL, sub=NULL, xlab=NULL,
                            xlim=NULL, col="black", pch=20, cex=1, type="p", ...)
{
	if (!is.numeric(x))
		stop(paste(sQuote("x"))," Needs to be numeric")

	if (is.null(xlab))
		xlab <- ToLabel(deparse(substitute(x)))

	i <- !is.na(x)
	x <- sort(x[i])
	n <- length(x)

	if (n <= 1)
		return(NA)

	if ((max(x, na.rm=TRUE) - min(x, na.rm=TRUE)) == 0)
		return(NA)

	if (is.null(xlim))
		xlim <- range(x, na.rm=TRUE)

	y <- qnorm((1:n) / (n+1))
	ylab <- "Cumulative Probability"
	ylim <- range(y[is.finite(y)])
	if (any(!is.finite(ylim)))
		ylim <- qnorm(0.001, 0.999)

	if (is.null(main))
		main <- "Log probability plot"

	o <- par("mar")
	if (main == "") {
	  mar <- o
	  mar[3] <- 1.1
	  par(mar=mar)
	}

	x.log <- ifelse(xlim[1] <= 0, FALSE, TRUE)
	if (x.log)
		xlab <- paste(xlab, "\n(log scale)", sep=" ")

	if (horizontal)
	{
		if (!add)
		{
			plot(x, y, type="n", main=main, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim,
			     log=ifelse(x.log, "x", ""), frame.plot=FALSE, axes=FALSE)

		  usr <- floor(par("usr")[1]):ceiling(par("usr")[2])
      atx <- 10^usr
		  labels <- sapply(usr, function(i) as.expression(bquote(10^ .(i))))
		  if (length(labels) < 4)
		  {
		    labels <- axTicks(1)
		    atx <- labels
		  }
		  axis(1, at=atx, labels=labels)

		  labels <- c(0.01, 0.1, 0.5, 0.9, 0.99)
		  aty <- qnorm(labels)
      abline(h=aty, col="grey")
      mtext(labels, side=2, line=1, at=aty, cex=par("cex.axis")*par("cex"), las=1)
		}
		if (type=="l" | type=="b")
			lines(x, y, col=col, ...)
		if (type=="p" | type=="b")
			points(x, y, pch=pch, cex=cex, col=col, ...)
	}
	else
	{
		if (!add)
		{
			plot(y, x, type="n", main=main, xlab=ylab, ylab=xlab, xlim=ylim, ylim=xlim,
			     log=ifelse(x.log, "y", ""), frame.plot=FALSE, axes=FALSE)

		  aty <- round(axTicks(1), 0)
		  labels <- sapply(log10(aty), function(i) as.expression(bquote(10^ .(i))))
		  if (length(labels) < 2)
		  {
		    #atx <- axTicks(2)
		    labels <- atx
		  }
		  axis(2, at=aty, labels=labels)

		  labels <- c(0.01, 0.1, 0.5, 0.9, 0.99)
		  atx <- qnorm(labels)
		  #axis(1, at=aty, labels=labels)
		  abline(h=aty, col="grey")
		  mtext(labels, side=1, line=1, at=aty, cex=par("cex.axis")*par("cex"), las=1)
			#box()
		}
		if (type=="l" | type=="b")
			lines(y, x, col=col, ...)
		if (type=="p" | type=="b")
			points(y, x, pch=pch, cex=cex, col=col, ...)
	}
	par(o)

	return(invisible())
}

#' log-probability plot.
#'
#' Generates a log-probability plot of numeric variable.
#'
#' @param formula a formula, such as y ~ grp, where y is a numeric vector of data values to be
#' split into groups according to the grouping variable \code{grp} (usually a factor).
#' @param data a data.frame from which the variables in \code{formula} should be taken.
#' @param xlab a label for the x axis, defaults to description of x.
#' @param col the colour of the series.
#' @param plot.legend plot the legend. Defaults to TRUE.
#' @param ... other parameters.
#'
#' @return None
#'
#' @keywords logprob.formula
#' @export
logprob.formula <- function(formula, data=NULL, xlab=NULL, col=NULL, plot.legend=TRUE, title=NULL, ...)
{
	mf <- model.frame(formula=formula, data=data)
  xlabels <- attr(terms(mf), "term.labels")
	ylabels <- names(mf)[attr(terms(mf), "response")]
	for (i in xlabels)
	{
		if (length(ylabels) == 0)
		{
			if (is.null(col))
				col <- "black"
			logprob(mf[,i], xlab=i, col=col, ...)
		}
		else
		{
			for (j in ylabels)
			{
				x <- mf[,i]
				y <- mf[,j]
				if (!is.factor(x))
					x <- factor(x)

				s <- split(y, x, drop=TRUE)
				xlim <- range(y[y>0], na.rm=TRUE)

				labs <- levels(x)
				olabs <- vapply(s, length, FUN.VALUE=0)
				ord <- order(olabs, decreasing=TRUE)
				olabs <- names(olabs[ord])

				n <- length(labs)
				if (is.null(col)) {
					col <- 1:n
				}
				if (length(col) == 1) {
				  col <- rep(col, n)
				}
				ocol <- col[ord]

  			#o <- par("mar")
  			#mar <- o
  			#mar[4] <- 8
  			#par(mar=mar)

				indx <- 1
				first <- TRUE
				for (k in olabs)
				{
					logprob(s[[k]], add=!first, xlim=xlim, xlab=ifelse(is.null(xlab), j, xlab), col=ocol[indx], ...)
					first <- FALSE
					indx <- indx + 1
				}

				#legend("topleft", labs, title=xlabels, inset=c(1, 0), xpd=TRUE,
				#       col=col, pch=20, bty="n", bg="white", cex=0.8*par("cex.axis"))
				if (plot.legend) {
				  title <- ifelse(is.null(title), xlabels, title)
				  legend("topleft", labs, title=title, col=col, pch=20, bty="o", bg="white", cex=0.7*par("cex.axis"))
				}
				#par(o)
			}
		}
	}

	return(invisible())
}

#' log-probability plot.
#'
#' Generates a log-probability plot of numeric variable.
#'
#' @param list a list of variable names to plot.
#' @param data a data.frame from which the variables in \code{list} should be taken.
#' @param col the colour (can be a vector of colours) of the series.
#' @param ... other parameters.
#'
#' @return None
#'
#' @keywords logprob.list
#' @export
logprob.list <- function(list, data, col=NULL, ...)
{
	list <- list[list %in% names(data)]
	for (i in list)
	{
		if (is.null(col))
			col <- "black"
		logprob(data[,i], xlab=i, col=col, ...)
	}

	return(invisible())
}

ToLabel <- function(x)
{
  if (missing(x))
    return("")

  lab <- deparse(substitute(x))

  start <- regexpr("\\$", lab)[1] + 1
  if(start==0)
    start=1

  end <- regexpr("\\[", lab)[1] - 1
  if(end<0)
    end <- nchar(lab)

  s <- gsub(".\\$","",deparse(substitute(x)))
  s
}
