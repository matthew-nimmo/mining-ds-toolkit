coords <- function(obj)
{
	cols <- attr(obj,'coords')
	obj[cols]
}

"coords<-" <- function(x, value)
{
	if(length(value)>3)
		Stop("Coords too long.")

	isin <- value %in% names(x)
	if(any(!isin))
		stop("Columns do not exist.")

	attr(x,'coords') <- value

	if(!is(x,'coords'))
		class(x) <- c("coords",class(x))
	x
}

plot.coords <- function(obj)
{
	xyz <- coords(obj)
	cols <- colnames(xyz)

	plot(xyz[,1],xyz[,2],xlab=cols[1],ylab=cols[2],pch=19,cex=0.5)
}
