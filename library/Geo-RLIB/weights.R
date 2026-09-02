weights <- function(obj, defaults=TRUE)
{
	cols <- attr(obj,'weights')

	if (!is.null(cols))
	{
		x <- obj[cols]
		xx <- paste("x[[",1:length(x),"]]")
		xx <- paste(xx,collapse="*")
		w <- eval(parse(text=xx))
	}
	else
	{
		if (defaults)
		{
			l <- dim(obj)
			if (is.null(l))
				w <- rep(1, length(obj))
			else
				w <- rep(1, l[[1]])
		}
		else
		{
			w <- NULL
		}
	}

	w
}

"weights<-" <- function(x, value)
{
	isin <- value %in% names(x)
	if(any(!isin))
		stop("Columns do not exist.")

	attr(x,'weights') <- value

	x
}
