.nlaxis <-
function(side, type=NA, grid=TRUE, grid.minor=grid)
{
	is.x <- side%%2 == 1

	if (!is.na(type))
	{
		if (type=="log")
		{
			usr <- par("usr")[if (is.x) 1:2 else 3:4]
			us <- c(floor(usr[1]), ceiling(usr[2]))
			usr <- 10^usr

			s <- us[1]:us[2]

			atTicks <- unlist(lapply(s, FUN=function(x) (1:9*10^x)))
			Labels <- atTicks[seq(1,length(atTicks),by=9)]
			Labels <- Labels[Labels>usr[1] & Labels<usr[2]]
			if (length(Labels)<2)
			{
				Labels <- atTicks[seq(1,length(atTicks))]
				Labels <- Labels[Labels>usr[1] & Labels<usr[2]]
			}
			if (length(Labels)<2)
			{
				atTicks <- seq.int(floor(usr[1]),ceiling(usr[2]),length.out=11)
				Labels <- atTicks
				Labels <- Labels[Labels>=usr[1] & Labels<=usr[2]]
			}
			atLabels <- Labels
			pat <- c(1,rep(0,8))
		}
		else if (type=="prob")
		{
			o <- par(las=2); on.exit(par(o))

			usr <- par("usr")[if (is.x) 1:2 else 3:4]

			atTicks <- qnorm(c(1:9*0.0001,1:9*0.001,1:9*0.01,1:9*0.1,91:99*0.01,991:999*0.001,9991:9999*0.0001))
			Labels <- c(0.001,0.01,0.05,0.1,0.2,0.5,0.8,0.9,0.95,0.99,0.999)
			ql <- qnorm(Labels)
			Labels <- Labels[ql>usr[1] & ql<usr[2]]
			atLabels <- qnorm(Labels)
			pat <- c(1,rep(0,8),1,rep(0,8),1,rep(0,8),1,rep(0,3),1,rep(0,3),1,rep(0,8),1,rep(0,8))
		}

		axis(side, label=FALSE, at=atTicks)

		if (length(Labels)>=2)
			mtext(Labels, side=side, line=1, at=atLabels, cex=par("cex.axis")*par("cex"))

		if (grid==TRUE)
		{
			if (grid.minor==TRUE)
			{
				pat[pat==0] <- 0.85
				pat[pat==1] <- 0.4
			}
			else
			{
				pat <- rep(0.85, length(pat[pat==1]))
				atTicks <- atLabels
			}
			if(is.x)
				abline(v=atTicks, col=gray(pat), lwd=(1-pat), lty=1)
			else
				abline(h=atTicks, col=gray(pat), lwd=(1-pat), lty=1)
		}
	}
	else
	{
		axis(side)
		if (grid==TRUE)
			grid(col=gray(0.7), lwd=0.3, lty=1)
	}
}
