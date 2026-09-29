contact <- function(grade, bhid, from, dom, upper=TRUE, both=FALSE, main=NULL, xlab=NULL, lim=10, cint=1)
{
	bhid <- as.character(bhid)
	r1 <- rle(bhid)
	r1$values <- 1:length(r1$lengths)
	bhid <- inverse.rle(r1)

	# flag domains on BHID basis

	dom <- as.integer(dom)
	r2 <- rle(bhid+0.1*dom)
	r2$values <- diff(c(0,r2$values))

	x <- inverse.rle(r2)
	x <- r2$values
	x[x>0.5] <- 2
	x[x>0 & x<0.2] <- 1
	x[x<0] <- 3
	r2$values <- x
	dom <- inverse.rle(r2)

	y <- lapply(split(dom, bhid), function(x) any(x==1))
	keep <- unsplit(y, bhid)

	# Determine average lengths

	d <- bhid+0.1*dom
	x <- unsplit(lapply(split(from, d), max), d)
	y <- unsplit(lapply(split(from, d), min), d)
	z <- x-y

	m1 <- (dom==1) * z
	m1 <- 0.75*ceiling(mean(m1[m1>0 & keep]))
	m2 <- (dom==2) * z
	m2 <- 0.75*ceiling(mean(m2[m2>0 & keep]))
	m3 <- (dom==3) * z
	m3 <- 0.75*ceiling(mean(m3[m3>0 & keep]))
	if(lim>m1) lim <- m1

	# Calculate bins for box plot

	b <- NULL

	# Upper zone
	b1 <- (dom==1) * (cint*floor((y-from)/cint) - cint)
	b1[!keep | b1<(-lim)] <- 0
	b2 <- (dom==2) * (cint*floor((x-from)/cint) + cint)
	b2[!keep | b2>lim] <- 0
	bu <- ceiling(b1+b2)

	# Lower zone
	b1 <- (dom==1) * (cint*floor((x-from)/cint) + cint)
	b1[!keep | b1>lim] <- 0
	b2 <- (dom==3) * (cint*floor((y-from)/cint) - cint)
	b2[!keep | b2<(-lim)] <- 0
	bl <- ceiling(b1+b2)

	if(both)
	{
		b <- -pmin(-bu,bl)
		t <- "contact"
	}
	else
	{
		if(upper)
		{
			b <- bu
			t <- "upper contact"
		}
		else
		{
			b <- bl
			t <- "lower contact"
		}
	}

	b[b==0] <- NA
	cols <- c("grey95", "yellowgreen")[unclass((sort(unique(b[!is.na(b)]))<0)+1)]

	# Generate plot

	u <- boxplot(grade~b,outline=F,col=cols,medlty=0,range=0.5,medpch=20,main=main,ylab="distance from contact (m)",xlab=xlab,horizontal=T)
	lines(u$stats[3,],1:length(u$names))
	#x <- abs(min(unique(b[!is.na(b)])))+0.5
	x <- length(u$names)/2 + 0.5
	abline(h=x,col="blue")
	mtext('contact',side=4,line=0,at=x,adj=0,col='blue',crt=90,cex=0.6)

	xi <- (1:length(u$names))[!is.na(match(u$names,paste('-',cint,sep='')))]
	yi <- (1:length(u$names))[!is.na(match(u$names,as.character(cint)))]
	if(u$stats[3,xi]>u$stats[3,yi])
		x <- (u$stats[2,xi]+u$stats[4,yi])/2
	else
		x <- (u$stats[4,xi]+u$stats[2,yi])/2
	abline(v=x,col="red")
	mtext(round(x,digits=2),side=3,line=0,at=x,adj=0.5,col='red',cex=0.6)
}
