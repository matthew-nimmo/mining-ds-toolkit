read.datamine <-
function(filename)
{
	readChar <- function(size)
	{
		if(byte.size==1)
		{
			bytes <- readBin(f, raw(), n=size, endian=endian)
			return(rawToChar(bytes))
		}

		bytes <- readBin(f, raw(), n=2*size, endian=endian)

		n <- size / 4
		val <- raw(length=n)
		for(i in 1:n)
		{
			i1 <- (i-1)*4
			i2 <- (i-1)*8
			val[i1+1] <- bytes[i2+1]
			val[i1+2] <- bytes[i2+2]
			val[i1+3] <- bytes[i2+3]
			val[i1+4] <- bytes[i2+4]
		}

		return(rawToChar(val))
	}

	readNum <- function()
	{
		if(byte.size==1)
			val <- readBin(f, numeric(), size=4, endian=endian)
		else
			val <- readBin(f, numeric(), size=8, endian=endian)

		val
	}

	#------
	# read the data binary file

	byte.size <- 1
	byte.order <- 0
	ieee <- if(.Platform$endian == "big") 1 else 0
	endian <- if(ieee == byte.order | byte.order < 0) .Platform$endian else "swap"

	if (!file.exists(filename))
		stop("read.datamine: Could not open input file: ", filename)

	f = file(filename, open="rb")

	#------
	# Read header

	filename <- readChar(8)
	directory <- readChar(8)
	description <- readChar(64)
	owner <- readChar(8)
	ownerPerms <- readNum()
	otherPerms <- readNum()
	modifyDate <- readNum()
	numFields <- readNum()
	numPages <- readNum()
	recsLastPage <- readNum()

	# Check if extended format
	if(modifyDate < 1)
	{
		byte.size <- 2
		seek(f, 0, rw='r')

		filename <- readChar(8)
		directory <- readChar(8)
		description <- readChar(64)
		owner <- readChar(8)
		ownerPerms <- readNum()
		otherPerms <- readNum()
		modifyDate <- readNum()
		numFields <- readNum()
		numPages <- readNum()
		recsLastPage <- readNum()
	}

	#------
	# Read fields

	fieldname <- vector()
	type <- vector()
	logicalRecPos <- vector()
	wordNumber <- vector()
	unit <- vector()
	default <- vector()
	size <- vector()

	vv <- nf <- 0
	pname <- ""
	for (v in 1:numFields)
	{
		xfieldname <- readChar(8)
		xtype <- readChar(4)[1]
		xlogicalRecPos <- readNum()
		xwordNumber <- readNum()
		xunit <- readNum()
		if(xtype == "N")
			xdefault <- readNum()
		else
			xdefault <- readChar(4)

		if(pname == xfieldname)
		{
			size[vv] <- size[vv] + 4
			default[vv] <- paste(default[vv], xdefault, sep="")
		}
		else
		{
			vv <- vv + 1
			fieldname[vv] <- sub('[[:space:]]+$', '', xfieldname)
			type[vv] <- sub('[[:space:]]+$', '', xtype)
			logicalRecPos[vv] <- xlogicalRecPos
			wordNumber[vv] <- xwordNumber
			unit[vv] <- xunit
			default[vv] <- xdefault
			size[vv] <- 4
			pname <- xfieldname
		}

		if(xlogicalRecPos > 0)
			nf <- nf + 1
	}

	#------
	# Skip to end of header page

	x <- byte.size*(1936 - (numFields * 28))
	seek(f, x, origin="current")

	numFields <- length(fieldname)

	#------
	# Create empty data

	if(nf > 0)
		nrp <- as.integer(508 / nf)
	else
		nrp = 0

	nd <- ((numPages - 2) * nrp) + recsLastPage
	nrb <- byte.size*(2048 - (4 * nf * nrp))

	np <- numPages - 1
	if(np < 0)
		np <- 0

	for(v in 1:numFields)
	{
		if(type[v] == "N")
			d <- as.vector(rep(0,nd), mode="numeric")
		else
			d <- as.vector(rep(" ",nd), mode="character")
		if(exists("adata"))
			adata[v] <- d
		else
			adata <- data.frame(d)
	}
	colnames(adata) <- fieldname

	#------
	# Read data

	r <- 1
	for (p in 1:np)
	{
		if(p == np)
			nrp <- recsLastPage

		for(pr in 1:nrp)
		{
			for(v in 1:numFields)
			{
				if(logicalRecPos[v] != 0)
				{
					if(type[v] == "N")
					{
						# check if variable is implicit or not
						adata[r, v] <- readNum()
					}
					else
					{
						# check if variable is implicit or not
						str <- readChar(size[v])
						adata[r, v] <- sub('[[:space:]]+$', '', str)
					}
				}
				else
					adata[r, v] <- default[v]
			}
			r <- r + 1
		}

		x <- readBin(f, raw(), n=nrb, endian=endian)
	}

	close(f)

	return(adata)
}
