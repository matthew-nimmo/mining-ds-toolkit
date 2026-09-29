wboxplot <- function(x, ...) UseMethod("wboxplot")

wboxplot.default <- function(groups, wts, ..., col='white', at=NULL, plot=TRUE)
{
	bx.stats <- function (x, w, coef=1.5, do.conf=TRUE, do.out=TRUE) 
	{
		if (coef < 0)
			stop("'coef' must not be negative")
		nna <- !is.na(x)
		n <- sum(nna)
		stats <- wquantile(x, w, probs=c(0,0.25,0.5,0.75,1), na.rm=T)
		iqr <- diff(stats[c(2, 4)])
		if (coef == 0)
			do.out <- FALSE
		else
		{
			out <- if (!is.na(iqr))
			{
				x < (stats[2L] - coef*iqr) | x > (stats[4L] + coef*iqr)
			}
			else
				!is.finite(x)
			if (any(out[nna], na.rm=TRUE))
				stats[c(1, 5)] <- range(x[!out], na.rm=TRUE)
		}
		conf <- if (do.conf) 
			stats[3L] + c(-1.58, 1.58) * iqr/sqrt(n)
		list(stats=stats, n=n, conf=conf, out=if (do.out) x[out & nna] else numeric())
	}

	n <- length(groups)
	names <- names(groups)

	for(i in 1:n)
	{
		nn <- length(groups[[i]])
		if(missing(wts))
			w <- rep(1, nn)
		else
			w <- unclass(wts[[i]])
		if(all(is.na(w)))
			w <- rep(1, nn)
		else
			w[is.na(w)] <- mean(w, na.rm=T)
		groups[i] <- list(bx.stats(unclass(groups[[i]]),w))
	}

    stats <- matrix(0, nr=5, nc=n)
    conf  <- matrix(0, nr=2, nc=n)
    ng <- out <- group <- numeric(0)
    ct <- 1

    for(i in groups)
	{
		stats[,ct] <- i$stats
		conf [,ct] <- i$conf
		ng <- c(ng, i$n)
		if((lo <- length(i$out)))
		{
			out	  <- c(out,i$out)
			group <- c(group, rep.int(ct, lo))
		}
		ct <- ct+1
    }

    z <- list(stats=stats, n=ng, conf=conf, out=out, group=group, names=names)
	if(plot==TRUE)
		bxp(z, boxfill=col, at=at, ...)

	invisible(z)
}

wboxplot.formula <- function(formula, wts, data=NULL, ...)
{
	if(missing(formula) || (length(formula) != 3))
		stop("'formula' missing or incorrect")

    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
		m$data <- as.data.frame(data)
    m$... <- NULL
    m$na.action <- na.pass
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())

    wboxplot(split(mf[[1]], mf[[2]]), split(mf[[3]], mf[[2]]), ...)
}
