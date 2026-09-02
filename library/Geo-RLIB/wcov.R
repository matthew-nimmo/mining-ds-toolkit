wcov <-
function(x, ...) UseMethod('wcov')

wcov.default <-
function(x, y, w=NULL, na.rm=TRUE)
{
	if (is.null(w))
		w <- rep(1, length(x))

	if (all(is.na(w)))
		w <- rep(1, length(x))

	# Remove values that are NA's
	if (na.rm == TRUE)
	{
		keep <- !(is.na(x) | is.na(y) | is.na(w))
		x <- x[keep]
		y <- y[keep]
		w <- w[keep]
	}
	else if (any(is.na(x)) | any(is.na(y)))
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

	# Are there any values left to calculate the weighted covariance?
	if (n == 0)
		return(NA)

	# calculated weighted covariance

	sumw <- sum(w)
	sumw2 <- sum(w^2)
	meanx <- sum(x * w) / sum(w)
	meany <- sum(x * w) / sum(w)

	val <- (sumw/(sumw^2-sumw2))*sum(w*(x-meanx)*(y-meany))
	#val <- sum(w*(x-meanx)*(y-meany)) / sum(w)

	return(val)
}

wcov.matrix <-
function(x, w=NULL)
{
	ncx <- ncol(x)
	r <- matrix(0, nrow=ncx, ncol=ncx)

	for (i in 1:ncx)
	{
		for (j in 1:ncx)
		{
			x2 <- x[,i]
			y2 <- x[,j]
			ok <- complete.cases(x2, y2)
			r[i, j] <- if(any(ok)) wcov(x2, y2, w=w) else NA
		}
	}
	rownames(r) <- colnames(x)
	colnames(r) <- colnames(x)

	return(r)
}

wcov.data.frame <-
function(x, ...) wcov(as.matrix(x), ...)
