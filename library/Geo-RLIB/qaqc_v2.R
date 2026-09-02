qaqc<-function(x,y,data=NULL,factor=NULL,label="",title="",skip=FALSE,lpanel=qaqc.scatter,cpanel=qaqc.rank,rpanel=qaqc.AMPD)
{
  lpanel<-qaqc.scatter
  cpanel<-qaqc.rank
  rpanel<-qaqc.AMPD

  # Function to plot graphs using a factor
  qaqc.by<-function(x,f,func)
  {
    xx<-split(x,f)
    n<-names(xx)

    for(i in n)
    {
      labs<-paste(labels,i,sep=":")
      func(xx[[i]],labs)
    }
  }

  # Main plot function
  qaqc.plot<-function(data,labs)
  {
    for(i in 1:length(ofld))
    {
      fix<-qaqc.fix(data,ofld[i],rfld[i])
      x<-fix[[ofld[i]]]
      y<-fix[[rfld[i]]]

      lpanel(x,y);mtext(labs[i],line=4,side=2,cex=0.8)
      cpanel(x,y)
      rpanel(x,y)

      o<-par("mfg")
      if(o[1]==o[3] & o[2]==o[4])
      {
        mtext(title,line=1,side=3,outer=TRUE)
        mtext(paste(output,"pdf",sep="."),line=2,side=1,outer=TRUE,cex=0.6)
      }
    }
  }

  # Function to remove NA's and trace values
  qaqc.fix<-function(data,x,y)
  {
    n<-names(data)
    if(!((x %in% n) & (y %in% n)))
      return(NULL)

    # Reset trace to 0
    data[[x]][data$x<0]<-0
    data[[y]][data$y<0]<-0

    # Remove NA's
    data<-data[!(is.na(data[[x]]) | is.na(data[[y]])),]

    return(data)
  }

  # Set plot region
  pdf(paste(output,"pdf",sep="."),width=8,height=11,onefile=TRUE,title=title)
  op=par(mfrow=c(4,3),mar=c(3,3,2,2),omi=c(0.5,0.5,0.5,0.2),mgp=c(2,1,0),cex.axis=0.6,cex.lab=0.8)

  if(!is.null(factor))
  {
    title<-paste(title,factor,sep=" by ")
    qaqc.by(data,data[[factor]],qaqc.plot)
  }
  else
    qaqc.plot(data,labels)

  par(op)
  dev.off()
}

# Function to either plot 'No data'
plot.nodata<-function()
{
  plot.new()
  text(0.5,0.5,"No data")
  box()
}

# Function to plot AMPD
qaqc.AMPD<-function(x,y,ymax=100,AMPD=10)
{
  if((length(x)==0 | length(y)==0))
  {
    plot.nodata()
    return
  }

  y<-100*abs(x-y)/(x+y)
  y<-sort(y)
  x<-100*seq(1,length(y),1)/length(y)

  plot(x,y,type="l",ylim=c(0,ymax),xlab="Rank %",ylab="AMPD %")
  grid(lty=1,lwd=0.4)

  abline(h=AMPD,col="red",lwd=0.4)
  mtext(paste(AMPD,"%"),side=4,col="red",cex=0.8,at=AMPD)

  a<-approx(y,x,xout=AMPD,ties="ordered")$y
  abline(v=a,col="red",lwd=0.4)
  mtext(paste(formatC(a,digits=0,format="f"),"%"),side=3,col="red",cex=0.8,at=a)
}

# Function to plot ranked scatter graph
qaqc.rank<-function(x,y)
{
  if((length(x)==0 | length(y)==0))
  {
    plot.nodata()
    return
  }

  mn<-min(min(x),min(y))
  mx<-max(max(x),max(y))
  lim<-c(mn,mx)

  plot(sort(x),sort(y),xlab=paste(xlab,"(ranked)"),ylab=paste(ylab,"(ranked)"),cex=0.5,pch=20,xlim=lim,ylim=lim)
  abline(0,1,col="red",lwd=0.4)
  grid(lty=1,lwd=0.4)
}

# Function to plot scatter graph
qaqc.scatter<-function(x,y,xlab="x",ylab="y",corc=0.8)
{
  stats<-function(x,d=2)
  {
    m1<-formatC(mean(x,na.rm=TRUE),digits=d,format="f")
    m2<-formatC(median(x,na.rm=TRUE),digits=d,format="f")
    m3<-formatC(sd(x,na.rm=TRUE),digits=d,format="f")
    t<-c(m1,m2,m3)
    names(t)<-c("mean:","median:","sd:")
    return(t)
  }

  mn<-min(min(x,na.rm=TRUE),min(y,na.rm=TRUE))
  mx<-max(max(x,na.rm=TRUE),max(y,na.rm=TRUE))
  lim<-c(mn,mx)

  plot(x,y,cex=0.5,pch=20,xlim=lim,ylim=lim)

  abline(0,1,col="red",lwd=0.4)
  s<-sd(x,na.rm=TRUE)+sd(y,na.rm=TRUE)
  abline(s,1,col="blue",lwd=0.4,lty="dashed")
  abline(-s,1,col="blue",lwd=0.4,lty="dashed")
  grid(lty=1,lwd=0.4)

  # Plot stats in top-left and bottom-right corners
  #plot.window(xlim=c(0,1),ylim=c(0,1))
  mtext(paste("pairs:",length(x)),line=0,side=3,cex=0.5,adj=0)

  co<-cor(x,y,use="complete")
  cl<-ifelse(co<corc,"red","black")
  mtext(paste("cor:",formatC(co,digits=2,format="f")),line=0,side=3,cex=0.5,adj=1,col=cl)

  # Plot y stats
  o<-par(adj=0,cex=0.8)
  l<-par("cxy")*1.2

  text(mn,mx-l[1],ylab,adj=0)
  text(mn,mx-(2*l[1]),c("mean:  ",formatC(mean(y,na.rm=TRUE),digits=2,format="f")))
  text(mn,mx-(3*l[1]),c("median:",formatC(median(y,na.rm=TRUE),digits=2,format="f")))
  text(mn,mx-(4*l[1]),c("sd:    ",formatC(sd(y,na.rm=TRUE),digits=2,format="f")))

  # Plot x stats
  text(mx-10,4*l[1],xlab)
  t<-stats(x)
  text(c(mx-10,mx),c(3*l[1],2*l[1],l[1]),t)

  par(o)
}
