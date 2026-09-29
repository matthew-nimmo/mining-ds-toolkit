domain <- function(obj)
{
	cols <- attr(obj,'domain')

	if (is.null(cols))
		cols
	else
		obj[[cols]]
}

"domain<-" <- function(x, value)
{
	isin <- value %in% names(x)
	if(any(!isin))
		stop("Columns do not exist.")

	attr(x,'domain') <- value

	if(!is(x,'domain'))
		class(x) <- c("domain",class(x))
	x
}
