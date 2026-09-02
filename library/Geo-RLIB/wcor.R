wcor <-
function(x, ...) UseMethod('wcor')

wcor.default <-
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
		y <- y[keep]
		w <- w[keep]
		n <- nkeep
	}

	# Are any weights Inf? Then treat them with equal weight and all others
	# with weight zero.
	wInfs <- is.infinite(w)
	if (any(wInfs))
	{
		x <- x[wInfs]
		y <- y[wInfs]
		n <- length(x)
		w <- rep(1, n)
	}

	# Are there any values left to calculate the weighted coefficient of variation?
	if (n == 0)
		return(NA)

	# calculated weighted correlation
	covx <- wcov(x, x, w=w, na.rm=TRUE)
	covy <- wcov(y, y, w=w, na.rm=TRUE)
	covxy <- wcov(x, y, w=w, na.rm=TRUE)
	val <- covxy / sqrt(covx * covy)

	return(val)
}

wcor.matrix <-
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
			r[i, j] <- if(any(ok)) wcor(x2, y2, w=w) else NA
		}
	}
	rownames(r) <- colnames(x)
	colnames(r) <- colnames(x)

	return(r)
}

wcor.data.frame <-
function(x, ...) wcor(as.matrix(x), ...)
