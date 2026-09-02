wsum <-
function(x, ...) UseMethod('wsum')

wsum.default <-
function(x, w=NULL, na.rm=TRUE)
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

	# Are there any values left to calculate the weighted mean of?
	if (n == 0)
		return(NA)

	# calculated weighted sum
	sum(w*x)
}

wsum.matrix <-
function(x, w=NULL, na.rm=TRUE)
{
	x <- as.matrix(x)
	ncx <- ncol(x)
	r <- matrix(0, nrow=1, ncol=ncx)

	for (i in 1:ncx)
	{
		x2 <- x[,i]
		ok <- !is.na(x2)
		r[i] <- if(any(ok)) wsum(x2, w=w, na.rm=na.rm) else NA
	}
	colnames(r) <- colnames(x)

	return(r)
}

wsum.data.frame <-
function(x, ...) wsum(as.matrix(x), ...)

wsum.formula <-
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
		r <- sapply(s, function(x) wsum(x, na.rm=na.rm))
	else
	{
		sw <- split(w, x, drop=TRUE)
		r <- sapply(1:length(s), function(i) wsum(s[[i]], w=sw[[i]], na.rm=na.rm))
	}

	return(r)
}
