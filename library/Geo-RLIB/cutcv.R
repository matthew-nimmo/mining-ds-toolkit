# Golder Associates
# M Nimmo
# v 1.0

cutcv <- function(x,main=NULL,sub=NULL,lab=ToLabel(x))
{
  op <- par(mar=c(5,4,4,4)+0.1);on.exit(par(op))

  y <- na.exclude(x)
  breaks <- quantile(y,probs=c(seq(0.1,0.9,0.05),seq(0.91,0.995,0.005)),na.rm=T)
  n <- length(breaks)
  m <- vector(length=n)
  cv <- vector(length=n)

  for(i in n:1)
  {
    y[y>breaks[i]] <- breaks[i]
    m[i] <- mean(y, na.rm=T)
    cv[i] <- sd(y, na.rm=T)/m[i]
  }

  plot(breaks,cv,xlab="Breaks",ylab="CV",type="l",col="blue",axes=T,frame.plot=T)
  #axis(1);abline(v=axTicks(1),col="black")
  #axis(2);abline(h=axTicks(2),col="blue",lty="dashed")
  #plotaxis(1,"prob")

  #y <- na.exclude(x)
  #q <- quantile(y, probs=0.975)
  #y[y>q] <- q
  #mq <- sd(y, na.rm=T)/mean(y)
  #lines(c(par("usr")[1],q,q),c(mq,mq,par("usr")[3]),col="green")
  #points(q,mq,col="black",pch=19)
  #mtext("97.5%",side=1,line=0.5,at=q,col="green",cex=0.8)
  #mtext(formatC(mq,digits=2),side=2,line=0.5,at=mq,col="green",cex=0.8)

  #uq <- quantile(x, probs=0.75, na.rm=T)
  #iqr <- uq-quantile(x, probs=0.25, na.rm=T)
  #abline(v=uq+1.5*iqr,col="red")
  #abline(v=uq+3.0*iqr,col="red")
}
