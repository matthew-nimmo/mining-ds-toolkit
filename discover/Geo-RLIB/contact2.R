cnt.stats <- function(d, upper=TRUE, lim=null)
{
	dom.first <- sapply(split(d,d$bhid), function(x) head(x$from[x$dom==TRUE],1))
	dom.last <- sapply(split(d,d$bhid), function(x) tail(x$to[x$dom==TRUE],1))

	r <- rle(as.character(d$bhid))
	r$values <- as.numeric(dom.first)
	depth <- (d$from + d$to)/2
	depth.u <- inverse.rle(r) - depth
	r$values <- as.numeric(dom.last)
	depth.d <- inverse.rle(r) - depth

	# Get distance interval

	if (upper==TRUE)
		d$depth <- ifelse(depth.d<0, NA, depth.u)
	else
		d$depth <- ifelse(depth.u>0, NA, depth.d)
	d$doms <- d$depth
	i <- d$doms<0 & !is.na(d$doms)
	d$doms[i] <- floor(d$doms[i])
	i <- d$doms>0 & !is.na(d$doms)
	d$doms[i] <- ceiling(d$doms[i])

	# Get statistics

	s <- split(d, d$doms)
	first <- TRUE
	for (i in s)
	{
		val <- i$grade[!is.na(i$grade)]
		m <- median(val)
		ci <- sort(val)[qbinom(c(.025,.975), length(val), 0.5)]
		v <- c(ci[1],m,ci[2],mean(i$depth))
		if (first)
			x <- v
		else
			x <- rbind(x, v)
		first <- FALSE
	}
	x <- cbind(as.numeric(names(s)), x)

	# Trim statistics

	if (is.null(lim))
	{
		n <- 1:length(x[,1])
		i1 <- tail(n[x[,1]<0][is.na(x[x[,1]<0,4])],1) + 1
		i2 <- head(n[x[,1]>0][is.na(x[x[,1]>0,4])],1) - 1
		x <- x[i1:i2,]
	}
	else
	{
		x <- x[abs(x[,1])<=lim,]
	}

	return(x)
}

contact <-
function(grade, bhid, from, to, dom, upper=TRUE, main=NULL, sub="Contact analysis", xlab=.tolabel(x), lim=NULL, cint=1, horizontal=FALSE)
{
	bhid <- as.character(bhid)
	d <- data.frame(bhid,from,to,dom,grade)
	i <- order(bhid, from)
	d <- d[i,]

	x <- cnt.stats(d, upper=upper, lim=lim)

	# Plot

	f <- function(x, eq)
	{
		m <- eq$coefficients[2]
		c <- eq$coefficients[1]
		y <- m * x + c
		return(y)
	}
	
	if (horizontal==TRUE)
	{
		plot(-x[,5], x[,3], type='l', ylim=c(min(x[,2]),max(x[,4])), lty="solid", main=main, ylab=xlab, xlab="Distance from contact (m)")
		mtext(sub, side=3, line=0.5, cex=0.8)
		segments(-x[,5], x[,2], -x[,5], x[,4], col="grey")
		epsilon <- 0.2 * diff(par("usr")[1:2]) / length(x[,1])
		segments(-x[,5]-epsilon, x[,2], -x[,5]+epsilon, x[,2], col="grey")
		segments(-x[,5]-epsilon, x[,4], -x[,5]+epsilon, x[,4], col="grey")
		points(-x[,5], x[,3], pch=19, cex=0.8)
		abline(v=0)

		if (upper==TRUE)
			mtext('upper contact', side=1, line=0, at=0, adj=0, col='blue', crt=90, cex=0.6)
		else
			mtext('lower contact', side=1, line=0, at=0, adj=1, col='blue', crt=90, cex=0.6)

		# Plot regression lines

		n <- par("usr")[1:2]

		a <- x[x[,5]>0,3]
		b <- -x[x[,5]>0,5]
		eq <- glm(a ~ b)
		lines(c(n[1],0), f(c(n[1],0),eq), lty="dashed",col="red")

		a <- x[x[,5]<0,3]
		b <- -x[x[,5]<0,5]
		eq <- glm(a ~ b)
		lines(c(0,n[2]), f(c(0,n[2]),eq), lty="dashed",col="red")
	}
	else
	{
		plot(x[,3], x[,5], type='l', xlim=c(min(x[,2]),max(x[,4])), lty="solid", main=main, xlab=xlab, ylab="Distance from contact (m)")
		mtext(sub, side=3, line=0.5, cex=0.8)
		segments(x[,2], x[,5], x[,4], x[,5], col="grey")
		epsilon <- 0.2 * diff(par("usr")[3:4]) / length(x[,1])
		segments(x[,2], x[,5]-epsilon, x[,2], x[,5]+epsilon, col="grey")
		segments(x[,4], x[,5]-epsilon, x[,4], x[,5]+epsilon, col="grey")
		points(x[,3], x[,5], pch=19, cex=0.8)
		abline(h=0)

		if (upper==TRUE)
			mtext('upper contact', side=4, line=0, at=0, adj=1, col='blue', crt=90, cex=0.6)
		else
			mtext('lower contact', side=4, line=0, at=0, adj=0, col='blue', crt=90, cex=0.6)

		# Plot regression lines
		n <- par("usr")[3:4]

		a <- x[x[,5]>0,5]
		b <- x[x[,5]>0,3]
		eq <- glm(b ~ a)
		lines(f(c(0,n[2]),eq), c(0,n[2]), lty="dashed", col="red")

		a <- x[x[,5]<0,5]
		b <- x[x[,5]<0,3]
		eq <- glm(b ~ a)
		lines(f(c(n[1],0),eq), c(n[1],0), lty="dashed", col="red")
	}
}
