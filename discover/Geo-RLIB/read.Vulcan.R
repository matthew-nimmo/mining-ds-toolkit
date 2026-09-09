read.Vulcan <-
function(filename, echo=0, na.strings=c('NA','-','-9.0','-9.00','-9.000','-9.0000'))
{
	# --- Open file

	if (!file.exists(filename))
		stop("read.Vulcan: Could not open input file: ", filename)

	in.file <- file(filename, open="r")

	# --- Skip
	r <- readLines(in.file, n=1)
	while(substr(r,1,12)!="* DEFINITION")
		r <- readLines(in.file, n=1)

	# --- Read header variables
	r <- readLines(in.file, n=1)
	if(substr(r,1,20)=="*   HEADER_VARIABLES")
	{
		hv.num <- as.integer(substr(r,21,nchar(r)))
		hv.names <- vector(mode="character",length=hv.num)
		hv.type <- vector(mode="character",length=hv.num)
		hv.length <- vector(mode="integer",length=hv.num)
		hv.decimal <- vector(mode="integer",length=hv.num)
		for (i in 1:hv.num)
		{
			r <- readLines(in.file, n=1)
			#hv.names[i] <- gsub(" ", "",substr(r,2,22))
			#hv.type[i] <- gsub(" ", "",substr(r,23,24))
			#hv.length[i] <- as.integer(substr(r,25,28))
			#hv.decimal[i] <- as.integer(substr(r,29,32))
			k <- strsplit(gsub("[[:space:]]+"," ",r),split=" ")
			hv.names[i] <- k[[1]][2]
			hv.type[i] <- k[[1]][3]
			hv.length[i] <- as.integer(k[[1]][4])
			hv.decimal[i] <- as.integer(k[[1]][5])
		}
	}

	# --- Read data variables
	r <- readLines(in.file, n=1)
	if(substr(r,1,13)=="*   VARIABLES")
	{
		dv.num <- as.integer(substr(r,14,nchar(r)))
		dv.names <- vector(mode="character",length=dv.num)
		dv.type <- vector(mode="character",length=dv.num)
		dv.length <- vector(mode="integer",length=dv.num)
		dv.decimal <- vector(mode="integer",length=dv.num)
		for (i in 1:dv.num)
		{
			r <- readLines(in.file, n=1)
			#dv.names[i] <- gsub(" ", "",substr(r,2,22))
			#dv.type[i] <- gsub(" ", "",substr(r,23,24))
			#dv.length[i] <- as.integer(substr(r,25,28))
			#dv.decimal[i] <- as.integer(substr(r,29,32))
			k <- strsplit(gsub("[[:space:]]+"," ",r),split=" ")
			dv.names[i] <- k[[1]][2]
			dv.type[i] <- k[[1]][3]
			dv.length[i] <- as.integer(k[[1]][4])
			dv.decimal[i] <- as.integer(k[[1]][5])
		}
	}

	# --- Skip
	r <- readLines(in.file, n=1)
	while(substr(r,1,9)!="* HEADER:")
		r <- readLines(in.file, n=1)

	# --- Read header data
	hv <- vector(mode="list",length=hv.num)
	s <- 10
	for (i in 1:hv.num)
	{
		f <- s + (hv.length[i]-1)
		if(hv.type[i]=="C")
			hv[i] <- gsub(" +$", "",substr(r,s,f))
		else
			hv[i] <- as.double(substr(r,s,f))
		s <- f + 1
	}

	# --- Skip
	r <- readLines(in.file, n=1)
	while(substr(r,1,1)=="*")
		r <- readLines(in.file, n=1)

	# --- Read data
	recs <- 1
	while(length(r)>0) 
	{
		s <- 1
		dv <- vector(mode="list",length=dv.num)
		for (i in 1:dv.num)
		{
			f <- s + (dv.length[i]-1)
			#ss <- gsub(" +$", "", substr(r,s,f))
			ss <- gsub(" ", "", substr(r,s,f))
			if(dv.type[i]=="C")
				dv[i] <- ss
			else
			{
				if(!is.null(na.strings))
				{
					if(ss %in% na.strings)
						dv[i] <- NA
					else
						dv[i] <- as.double(ss)
				}
			}
			s <- f + 1
		}
		dv <- as.data.frame(dv,stringsAsFactors=F)
		names(dv) <- dv.names
		if (recs==1)
			addata <- dv
		else
			addata <- rbind(addata, dv)
		recs <- recs + 1
		r <- readLines(in.file, n=1)
	}

	# --- Close file
	close(in.file)

	addata
}
