sumby <-
function(x, by, w=NULL, FUNC=NULL, csv=NULL)
{
	if (is.null(FUNC))
		FUNC <- function(y) length(y[!is.na(y)])/length(y)*100

	#hasw <- 'w' %in% names(formals(FUNC))

	if (is.null(by))
	{
		m <- sapply(x, FUNC)
	}
	else
	{
		if (is.null(dim(by)))
		{
			n <- deparse(substitute(by))
			by <- as.matrix(by)
			colnames(by) <- n
		}
		m <- list()
		n <- ncol(x)
		for (j in colnames(by))
		{
			s1 <- split(x, by[,j])
			if (is.null(w))
			{
				if (is.null(n))
					m[[j]] <- sapply(s1, function(y) FUNC(y))
				else
					m[[j]] <- t(sapply(names(s1), function(d) sapply(s1[[d]], function(y) FUNC(y))))
			}
			else
			{
				s2 <- split(w, by[,j])
				if (is.null(n))
					m[[j]] <- sapply(s1, function(y) FUNC(y,w=s2[[d]]))
				else
					m[[j]] <- t(sapply(names(s1), function(d) sapply(s1[[d]], function(y) FUNC(y,w=s2[[d]]))))
			}
		}
	}

	if (!is.null(csv))
	{
		csv <- ifelse(grepl('.csv', csv), csv, paste(csv, 'csv', sep='.'))

		for (i in names(m))
			write.csv(m[[i]], sub("%x%", i, csv))
	}

	return(m)
}
