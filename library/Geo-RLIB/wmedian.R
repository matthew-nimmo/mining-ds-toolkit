wmedian <-
function(x, ...) UseMethod('wmedian')

wmedian.default <-
function(x, w=NULL, na.rm=TRUE, ties=NULL)
{
	if (is.null(w))
		w <- rep(1, length(x))

	if(all(is.na(w)))
		w <- rep(1, length(x))

	# Remove values that are NA's
	if (na.rm == TRUE)
	{
		keep <- !(is.na(x) | is.na(w))
		x <- x[keep]
		w <- w[keep]
	}
	else if (any(is.na(x)))
		return(NA)

	# Assert that the weights are all non-negative
	if (any(w < 0))
		stop("Some of the weights are negative; one can only have positive weights.")

	# Remove values with weight zero. This will:
	# 1) take care of the case when all weights are zero,
	# 2) make sure that possible tied values are next to each others, and
	# 3) it will most likely speed up the sorting
	n <- length(w)
	keep <- (w > 0)
	nkeep <- sum(keep)
	if (nkeep < n)
	{
		x <- x[keep]
		w <- w[keep]
		n <- nkeep
	}

	# Are any weights Inf? Then treat them with equal weight and all others
	# with weight zero.
	wInfs <- is.infinite(w)
	if (any(wInfs))
	{
		x <- x[wInfs]
		n <- length(x)
		w <- rep(1, n)
	}

	# Are there any values left to calculate the weighted median of?
	if (n == 0)
		return(NA)

	# Order the values and order the weights accordingly
	ord <- order(x)
	x <- x[ord]
	w <- w[ord]
	wcum <- cumsum(w)
	wsum <- wcum[n]
	wmid <- wsum / 2

	# Find the position where the sum of the weights of the elements such that
	# x[i] < x[k] is less or equal than half the sum of all weights
	# (these two lines could probably be optimized for speed)
	lows <- (wcum <= wmid)
	k <- sum(lows)

	# Two special cases where all the weight are at the first or the
	# last value:
	if (k == 0) return(x[1])
	if (k == n) return(x[n])

	# At this point we know that:
	# 1) at most half the total weight is in the set x[1:k],
	# 2) that the set x[(k+2):n] contains less than half the total weight
	# The question is whether x[(k+1):n] contains *more* than
	# half the total weight (try x=c(1,2,3), w=c(1,1,1)). If it is then
	# we can be sure that x[k+1] is the weighted median we are looking
	# for, otherwise it is any function of x[k:(k+1)].
	wlow <- wcum[k]; # the weight of x[1:k]
	whigh <- wsum - wlow; # the weight of x[(k+1):n]
	if (whigh > wmid)
		val <- x[k+1]
	if (is.null(ties) || ties == "weighted")
	{
		# Default!
		val <- (wlow*x[k] + whigh*x[k+1]) / wsum
	}
	else if (ties == "max")
	{
		val <- x[k+1]
	}
	else if (ties == "min")
	{
		val <- x[k]
	}
	else if (ties == "mean")
	{
		val <- (x[k]+x[k+1])/2
	}
	else if (ties == "both")
	{
		val <- c(x[k], x[k+1])
	}
	return(val)
}

wmedian.matrix <-
function(x, w=NULL, na.rm=TRUE)
{
	x <- as.matrix(x)
	ncx <- ncol(x)
	r <- matrix(0, nrow=1, ncol=ncx)

	for (i in 1:ncx)
	{
		x2 <- x[,i]
		ok <- !is.na(x2)
		r[i] <- if(any(ok)) wmedian(x2, w=w, na.rm=na.rm) else NA
	}
	colnames(r) <- colnames(x)

	return(r)
}

wmedian.data.frame <-
function(x, ...) wmedian(as.matrix(x), ...)

wmedian.formula <-
function(formula, data=list(), w=NULL, na.rm=TRUE)
{
	mf <- model.frame(formula=formula, data=data)
	xlabels <- attr(terms(mf), "term.labels")
	ylabels <- names(mf)[attr(terms(mf), "response")]

	x <- mf[,xlabels[1]]
	y <- mf[,ylabels[1]]

	if (!is.factor(x))
		x <- factor(x)

	s <- split(y, x, drop=TRUE)
	if (is.null(w))
		r <- sapply(s, function(x) wmedian(x, na.rm=na.rm))
	else
	{
		sw <- split(w, x, drop=TRUE)
		r <- sapply(1:length(s), function(i) wmedian(s[[i]], w=sw[[i]], na.rm=na.rm))
	}

	return(r)
}
