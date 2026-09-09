barsby <-
function(x, by, FUNC=length, main=NA, sub=NA, xlab=NULL, ylab=NULL, col='darkgrey', png=NULL)
{
	y <- sort(sumby(x, by=by, FUNC=FUNC)[[1]])
	z <- paste(y,' (',signif(100*y/sum(y),3),'%)',sep='')
	n <- length(y)
	xlim <- c(0, 1.1*max(y))
	ylim=c(0, n*1.2)

	op <- par(oma=c(0,0,2,0), mar=c(2,4,2,2), mgp=c(2,0.5,0), no.readonly=TRUE)
	pars <- par()
	th <- max(strheight(names(y), units="in", cex=NULL))
	tlm <- max(strwidth(c(names(y),ylab), units="in", cex=NULL)) + 2*th
	trm <- max(strwidth(z, units="in", cex=NULL))
	tt <- strwidth(main, units="in", cex=pars$cex.main, font=pars$font.main) + 6*strwidth('W', units="in", cex=pars$cex.main, font=pars$font.main)
	mar <- pars$omi + pars$mai
	h <- mar[1] + mar[3] + n*1.2*pars$cin[2]
	dev.off()

	if (!is.null(png))
	{
		if (!grepl('.png', png))
			png <- paste(png, 'png', sep='.')

		png(png, width=max(1.5*h, tt), height=h, units='in', res=144)
		on.exit(dev.off())
	}
	else
		windows(width=max(1.5*h, tt), height=h)

	par(oma=c(0,0,2,0), mar=c(2,4,2,2), mgp=c(2,0.5,0), mai=c(pars$mai[1],tlm,pars$mai[3],trm), xaxs='i', yaxs='i')
	h <- barplot(y, main=NA, horiz=TRUE, las=1, col=col, border=NA, xlab=NA, xlim=xlim, ylim=ylim, axes=FALSE, xpd=FALSE)
	#axis(1, cex.axis=0.8, cex.lab=0.8, lwd=0, lwd.ticks=1)
	text(y,h, labels=z, pos=4, offset=0.2, cex=1, xpd=NA)
	mtext(ylab, side=2, at=1.05*par('usr')[4], line=0.5, las=1, adj=1, padj=0, font=par()$font.main)
	mtext(sub, side=3, line=-0.5, cex=par()$cex.sub, outer=TRUE)
	mtext(paste('Total =',FUNC(x)), side=1, line=0.5, cex=par()$cex.sub)
	title(main=main, outer=TRUE)

	par(op)

	return(y)
}
