wsd <-
function(x, ...) UseMethod('wsd')

wsd.default <-
function(x, w=NULL, na.rm=TRUE)
{
	val <- wvar(x, w, na.rm)
	sqrt(val)
}

wsd.matrix <-
function(x, w=NULL, na.rm=TRUE)
{
	x <- as.matrix(x)
	ncx <- ncol(x)
	r <- matrix(0, nrow=1, ncol=ncx)

	for (i in 1:ncx)
	{
		x2 <- x[,i]
		ok <- !is.na(x2)
		r[i] <- if(any(ok)) wsd(x2, w=w, na.rm=na.rm) else NA
	}
	colnames(r) <- colnames(x)

	return(r)
}

wsd.data.frame <-
function(x, ...) wsd(as.matrix(x), ...)

wsd.formula <-
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
		r <- sapply(s, function(x) wsd(x, na.rm=na.rm))
	else
	{
		sw <- split(w, x, drop=TRUE)
		r <- sapply(1:length(s), function(i) wsd(s[[i]], w=sw[[i]], na.rm=na.rm))
	}

	return(r)
}
