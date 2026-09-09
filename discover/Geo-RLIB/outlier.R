outlier.zscore <- function(x, left=FALSE, right=TRUE, use.median=TRUE, zscore=3.5)
{
	n <- length(x)
	recs <- rep(FALSE, n)

	if (use.median)
	{
		Z <- 0.6745 * (x - median(x, na.rm=TRUE)) / mad(x, na.rm=TRUE)
	}
	else
	{
		#Z <- (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
		Z <- qqnorm(x, plot.it=FALSE)$x
	}
	if (right)
		recs <- Z > zscore
	if (left)
		recs <- recs | (Z < -zscore)

	return((1:n)[recs])
}

outlier.msd <- function(x, left=FALSE, right=TRUE, sd=3.0, use.median=TRUE)
{
	n <- length(x)
	recs <- rep(FALSE, n)

	if (use.median)
	{
		m <- median(x, na.rm=TRUE)
		s <- 1.483*mad(x, na.rm=TRUE)
	}
	else
	{
		m <- mean(x, na.rm=TRUE)
		s <- sd(x, na.rm=TRUE)
	}
	if (right)
		recs <- x > (m+2*s)
	if (left)
		recs <- recs | x < (m-2*s)

	return((1:n)[recs])
}

outlier.boxplot <- function(x, left=FALSE, right=TRUE, iqrm=3)
{
	n <- length(x)
	recs <- rep(FALSE, n)

	Q <- quantile(x, breaks=c(0.25,0.75), na.rm=TRUE)
	iqr <- Q[2] - Q[1]
	if (right)
		recs <- x > (Q[2] + iqrm * iqr)
	if (left)
		recs <- recs | x < (Q[2] - iqrm * iqr)

	return((1:n)[recs])
}

outlier.abox <- function(x, left=FALSE, right=TRUE, iqrm=1.5)
{
	mc <- function(x)
	{
		n <- length(x)
		m <- mean(x, na.rm=TRUE)
		f <- function(i,j)
		{
			#if (x[i] == x[j] == m)
			v <- ((x[j] - m) - (m - x[i])) / (x[j] - x[i])
			return(v)
		}
		u <- mapply(f, 1:n, 1:m)
		return(median(u))
	}

	n <- length(x)
	recs <- rep(FALSE, n)

	Q <- quantile(x, breaks=c(0.25,0.75), na.rm=TRUE)
	iqr <- Q[2] - Q[1]
	k <- mc(x)
	if (k >= 0)
	{
		wisk.lower <- Q[1] - (iqrm*exp(-3.5*k)*iqr)
		wisk.upper <- Q[2] + (iqrm*exp(4.0*k)*iqr)
	}
	else
	{
		wisk.lower <- Q[1] - (iqrm*exp(-4.0*k)*iqr)
		wisk.upper <- Q[2] + (iqrm*exp(3.5*k)*iqr)
	}
	if (right)
		recs <- x > wisk.upper
	if (left)
		recs <- recs | x < wisk.lower

	return((1:n)[recs])
}
