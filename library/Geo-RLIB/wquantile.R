wquantile <-
function(x, ...) UseMethod("wquantile")

wquantile.default <-
function(x, w=NULL, probs=seq(0,1,0.25), na.rm=FALSE, names=TRUE)
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
	cumw <- cumsum(w) / sum(w)

	val <- NA
	for(i in 1:length(probs))
	{
		if(probs[i] > 0 & probs[i] < 1)
		{
			k <- x[cumw<=probs[i]]
			val[i] <- ifelse(length(k)>0,max(k),NA)
		}
		else if (probs[i] == 0)
			val[i] <- min(x)
		else if (probs[i] == 1)
			val[i] <- max(x)
	}
	names(val) <- paste(probs*100,'%',sep="")

	return(val)
}

wquantile.matrix <-
function(x, w=NULL, probs=seq(0,1,0.25), na.rm=FALSE, names=TRUE)
{
	x <- as.matrix(x)
	ncx <- ncol(x)
	r <- matrix(0, nrow=1, ncol=ncx)

	for (i in 1:ncx)
	{
		x2 <- x[,i]
		ok <- !is.na(x2)
		r[i] <- if(any(ok)) wquantile(x2, w=w, probs=probs, na.rm=na.rm, names=names) else NA
	}
	colnames(r) <- colnames(x)

	return(r)
}

wquantile.data.frame <-
function(x, ...) wquantile(as.matrix(x), ...)

wquantile.formula <-
function(formula, data=list(), w=NULL, probs=seq(0,1,0.25), na.rm=FALSE, names=TRUE)
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
		r <- sapply(s, function(x) wquantile(x, probs=probs, na.rm=na.rm, names=names))
	else
	{
		sw <- split(w, x, drop=TRUE)
		r <- sapply(1:length(s), function(i) wquantile(s[[i]], w=sw[[i]], probs=probs, na.rm=na.rm, names=names))
	}

	return(r)
}
