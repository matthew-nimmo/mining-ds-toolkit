# Golder Associates
# M Nimmo
# v 1.0

statsplot <- function(x,main=NULL,sub=NULL,lab=ToLabel(x),filter=NULL)
  UseMethod('statsplot',x)

statsplot.default <- function(x,main=NULL,sub=NULL,lab=ToLabel(x),filter="no filter")
{
  y <- na.exclude(x)
  sig <- max(nchar(x-as.integer(x)))-2
  sig <- ifelse(sig>4,4,sig)

  # set plot margins
  op <- par(mar=c(2,2,4.1,2))
  on.exit(par(op))

  # create new plot
  plot.new()
  plot.window(xlim=c(0,1),ylim=c(0,1),log="",asp=NA)
  text(0,0.9,lab,adj=0,cex=2,col="black",font=2)
  if(!is.null(filter))
    text(0,0.83,filter,adj=0,cex=1,col="darkgreen",font=2)
  height <- (0.95-0.05)/13
  cex <- 1

  # summary statistics
  l <- list("Samples","Missing"," ","Mean","Median","Variance","Std Dev","CV")
  cols <- c(rep("black",3),"blue",rep("black",times=3),"red")
  h <- 0.95-4*height
  pos <- 1.2*strwidth("variance")+0.1
  text(0,seq(h,h-7*height,by=-height),l,adj=0,cex=cex,col=cols)
  sv <- vector(length=8)
  sv[1] <- formatC(length(x)-length(x[is.na(x)]))
  sv[2] <- formatC(length(x[is.na(x)]))
  sv[3] <- " "
  sv[4] <- formatC(mean(y),format="f",digits=4)
  sv[5] <- formatC(median(y),format="f",digits=4)
  sv[6] <- formatC(var(y),format="f",digits=4)
  sv[7] <- formatC(sd(y),format="f",digits=4)
  sv[8] <- formatC(sd(y)/mean(y),format="f",digits=4)
  text(pos,seq(h,h-7*height,by=-height),sv,adj=0,cex=cex,col=cols)

  # quantiles
  l <- c("maximum",0.99,0.975,0.95,0.1*9:1,"minimum")
  text(0.55,seq(0.95,0.95-13*height,by=-height),l,adj=0,cex=cex,col="black")
  q <- quantile(y,probs=c(0.1*1:9,0.95,0.975,0.99))
  qv <- vector(length=14)
  qv[1] <- formatC(suppressWarnings(max(y)),format="f",digits=sig)
  qv[2] <- formatC(q[12],format="f",digits=sig)
  qv[3] <- formatC(q[11],format="f",digits=sig)
  qv[4] <- formatC(q[10],format="f",digits=sig)
  qv[5] <- formatC(q[9],format="f",digits=sig)
  qv[6] <- formatC(q[8],format="f",digits=sig)
  qv[7] <- formatC(q[7],format="f",digits=sig)
  qv[8] <- formatC(q[6],format="f",digits=sig)
  qv[9] <- formatC(q[5],format="f",digits=sig)
  qv[10] <- formatC(q[4],format="f",digits=sig)
  qv[11] <- formatC(q[3],format="f",digits=sig)
  qv[12] <- formatC(q[2],format="f",digits=sig)
  qv[13] <- formatC(q[1],format="f",digits=sig)
  qv[14] <- formatC(suppressWarnings(min(y)),format="f",digits=sig)
  text(0.8,seq(0.95,0.95-13*height,by=-height),qv,adj=0,cex=cex,col="black")

  title(main=main)
  mtext(sub,side=3,line=0.5,font=par("font.sub"),cex=par("cex.sub"),adj=0.5)

  box(which="plot")
}

statsplot.formula <- function(x,data,main=NULL,sub=NULL,lab=ToLabel(x))
{
  if (!is.data.frame(data))
    stop(paste(sQuote("data"))," Needs to be a data frame")

  y <- get_all_vars(x, data)
  n <- names(y)
  y <- split(y[[1]],y[[2]])

  internal.f <- function(a,b)
  {
    statsplot(a,main,sub,lab=n[1],filter=paste(n[2],'=',b,sep=""))
  }

  mapply(internal.f,y,names(y))
}
