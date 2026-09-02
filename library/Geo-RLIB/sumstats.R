sumstats <-
function(x, ...) UseMethod('sumstats',x)

sumstats.default <-
function(x, w=NULL)
{
	v <- vector(mode="numeric", length=14)

	if (is.null(x) || length(x[!is.na(x)])==0)
	{
		v <- c(0, 0, rep(NA,12))
	}
	else
	{
		v[1] <- length(x[!is.na(x)])
		v[2] <- length(x[is.na(x)])
		v[3] <- min(x, na.rm=TRUE)
		v[4] <- max(x, na.rm=TRUE)
		v[5] <- v[4] - v[3]
		v[6] <- wsum(x, w, na.rm=TRUE)
		v[7] <- wmean(x, w, na.rm=TRUE)
		v[8] <- wvar(x, w, na.rm=TRUE)
		v[9] <- wcv(x, w, na.rm=TRUE)
		q <- wquantile(x, w, probs=c(0.1,0.25,0.5,0.75,0.995), na.rm=TRUE)
		v[10] <- q[1]
		v[11] <- q[2]
		v[12] <- q[3]
		v[13] <- q[4]
		v[14] <- q[5]
	}
	colls <- c('Number','Missing','Min','Max','Range','Total','Mean','Var','COV','10%','25%','50%','75%','99.5%')
	names(v) <- colls
	v[!is.finite(v)] <- NA

	return(v)
}

sumstats.formula <-
function(formula, data=list(), w=NULL, csv=NULL)
{
	mf <- model.frame(formula=formula, data=data)
	xlabels <- attr(terms(mf), "term.labels")
	ylabels <- names(mf)[attr(terms(mf), "response")]

	x <- mf[,xlabels[1]]
	y <- mf[,ylabels[1]]

	if (!is.factor(x))
		x <- factor(x)

	s <- split(y, x)

	if (is.null(w))
		w <- rep(1, length(x))
	w <- split(w, x)

	first <- TRUE
	for (i in names(s))
	{
		v <- sumstats(s[[i]], w=w[[i]])
		if (first)
		{
			val <- v
			first <- FALSE
		}
		else
		{
			val <- rbind(val, v)
		}
	}

	row.names(val) <- NULL
	val <- as.data.frame(val)
	val <- cbind(names(s), val)
	colnames(val)[1] <- xlabels
	val <- cbind(Assay=rep(ylabels, nrow(val)), val)

	if (!is.null(csv))
	{
		csv <- ifelse(grepl('.csv', csv), csv, paste(csv, 'csv', sep='.'))

		write.csv(m, csv, row.names=FALSE)
	}

	return(val)
}

sumstats.matrix <-
function(x, by=NULL, w=NULL, csv=NULL)
{
	vars <- colnames(x)

	if (!is.null(csv))
		csv <- ifelse(grepl('.csv', csv), csv, paste(csv, 'csv', sep='.'))

	if (is.null(by))
	{
		m <- sapply(1:ncol(x), function(i) sumstats(x[,i], w=w))
		m <- t(m)
		m <- cbind(Field=vars, m)
		row.names(m) <- NULL

		if (!is.null(csv))
			write.csv(m, csv, row.names=FALSE)
	}
	else
	{
		x <- cbind(x, by)
		by <- colnames(by)

		for (i in by)
		{
			first <- TRUE
			for (j in vars)
			{
				v <- sumstats(as.formula(paste(j,'~',i)), data=x, w=w)
				if (first)
				{
					m <- v
					first <- FALSE
				}
				else
				{
					m <- rbind(m, v)
				}
			}
			if (!is.null(csv))
				write.csv(m, sub("%x%", i, csv), row.names=FALSE)
		}
	}
}

sumstats.data.frame <-
function(x, by=NULL, w=NULL, csv=NULL)
{
	if (!is.null(by) && is.null(dim(by)))
	{
		lab <- deparse(substitute(by))
		lab <- gsub(".*\\$","", lab)
		by <- as.matrix(by)
		colnames(by) <- lab
	}
	sumstats.matrix(x, by, w, csv)
}
