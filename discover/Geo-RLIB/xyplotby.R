xyplotby <-
function(x, y, by, xlab=NULL, ylab=NULL, bylab=NULL, col.hi='red', col.lo='black', col.smooth='darkgray', cex=c(0.4, 1), pch=c(20, 20), show.smooth=TRUE, png=NULL)
{
	kk <- sort(unique(by))
	n <- length(kk)
	nr <- floor(sqrt(n))
	nc <- ceiling(n/nr)

	xlim <- range(x, na.rm=TRUE)
	ylim <- range(y, na.rm=TRUE)
	ylim[2] <- ylim[1] + 1.1*(ylim[2]-ylim[1])

	ph <- 1.84+nr*3
	pw <- 1.2+nc*3

	if (length(cex) == 1)
		cex <- c(cex, cex)
	if (length(cex) > 2)
		cex <- cex[1:2]

	if (length(pch) == 1)
		pch <- c(pch, pch)
	if (length(pch) > 2)
		pch <- pch[1:2]

	if (!is.null(png))
		png(png, width=pw, height=ph, unit='in', res=144)

	op <- par(mfrow=c(nr, nc), mar=c(0.2,0.2,0.2,0.2), oma=c(5.1,4.1,9.1,4.1), cex=1)

	cnt.nc <- 1
	cnt.nr <- 1
	for (z in kk)
	{
		if (show.smooth)
		{
			smoothScatter(x[by!=z],y[by!=z], axes=FALSE, colramp=colorRampPalette(c("white", col.smooth)), nrpoints=Inf, col=col.lo, pch=pch[1], cex=cex[1], transformation=function(x) x^0.65, xlim=xlim, ylim=ylim)
			#denscol <- densCols(x, y, colramp=colorRampPalette(c("black", "white")))
			#df.dens <- col2rgb(denscol)[1,] + 1L
			#cols <- colorRampPalette(c("white", col.dark))(255)
			#df.col <- cols[df.dens]
			#i <- order(df.dens)
			#x <- x[i]
			#y <- y[i]
			#plot(x[by!=z], y[by!=z], pch=20, cex=3, col=df.col, axes=FALSE, frame.plot=TRUE, xlim=xlim, ylim=ylim)
			#points(x[by!=z], y[by!=z], pch=20, cex=0.3, col='black')
		}
		else
		{
			plot(x[by!=z], y[by!=z], pch=pch[1], cex=cex[1], col=col.lo, axes=FALSE, frame.plot=TRUE, xlim=xlim, ylim=ylim)
		}
		points(x[by==z], y[by==z], pch=pch[2], cex=cex[2], col=col.hi)
		pars <- par()
		text(mean(pars$usr[1:2]), pars$usr[4], labels=paste(bylab,z,sep='='), pos=1, cex=1, font=pars$font.main)

		if(cnt.nc==1 && !(cnt.nr%%2))
		{
			axis(2)
			mtext(ylab, side=2, line=3, cex=par()$cex.lab)
		}
		if(cnt.nc==nc && (cnt.nr%%2))
		{
			axis(4)
			mtext(ylab, side=4, line=3, cex=par()$cex.lab)
		}
		if(cnt.nr==1 && !(cnt.nc%%2))
		{
			axis(3)
			mtext(xlab, side=3, line=3, cex=par()$cex.lab)
		}
		if(cnt.nr==nr && (cnt.nc%%2))
		{
			axis(1)
			mtext(xlab, side=1, line=3, cex=par()$cex.lab)
		}

		cnt.nc <- cnt.nc+1
		if (cnt.nc>nc)
		{
			cnt.nr <- cnt.nr + 1
			cnt.nc <- 1
		}
	}
	mtext(paste('Scatter plot of',ylab,'verse',xlab), side=3, line=6.5, cex=par('cex.main'), font=par('font.main'), outer=TRUE)
	mtext(main, side=3, line=5.5, cex=1, outer=TRUE)
	par(op)

	if (!is.null(png))
		dev.off()
}
