qaqc<-function(input,ofld,rfld,output=input,labels=ofld,xlab="Original",ylab="Replicate",factor=NULL,ymax=100,title="QAQC",corc=0.8,hard=5,skip=FALSE)
{
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

      if((length(x)>0 | length(y)>0))
      {
        qaqc.scatter(x,y)
        mtext(labs[i],line=4,side=2,cex=0.8)
        qaqc.rank(x,y)
        qaqc.hard(x,y)

        o<-par("mfg")
        if(o[1]==o[3] & o[2]==o[4])
        {
          mtext(title,line=1,side=3,outer=TRUE)
          mtext(paste(output,"pdf",sep="."),line=2,side=1,outer=TRUE,cex=0.6)
        }
      }
      else
        qaqc.nodata(labs[i])
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

  # Function to plot stats in scatter graph
  qaqc.stats<-function(x,y)
  {
    plot.window(xlim=c(0,1),ylim=c(0,1))

    mtext(paste("pairs:",length(x)),line=0,side=3,cex=0.5,adj=0)

    # Plot replicate stats
    # Labels
    text(0,0.95,ylab,adj=0,cex=0.6)
    text(0,0.90,"mean:",adj=0,cex=0.6);
    text(0,0.85,"median:",adj=0,cex=0.6);
    text(0,0.80,"sd:",adj=0,cex=0.6);

    # Stats
    text(0.3,0.90,formatC(mean(y),digits=2,format="f"),adj=1,cex=0.6)
    text(0.3,0.85,formatC(median(y),digits=2,format="f"),adj=1,cex=0.6)
    text(0.3,0.80,formatC(sd(y),digits=2,format="f"),adj=1,cex=0.6)

    # Plot original stats
    # Labels
    text(0.7,0.20,xlab,adj=0,cex=0.6)
    text(0.7,0.15,"mean:",adj=0,cex=0.6);
    text(0.7,0.10,"median:",adj=0,cex=0.6);
    text(0.7,0.05,"sd:",adj=0,cex=0.6);

    # Stats
    text(1,0.15,formatC(mean(x),digits=2,format="f"),adj=1,cex=0.6)
    text(1,0.10,formatC(median(x),digits=2,format="f"),adj=1,cex=0.6)
    text(1,0.05,formatC(sd(x),digits=2,format="f"),adj=1,cex=0.6)
  }

  # Function to plot scatter graph
  qaqc.scatter<-function(x,y)
  {
    mn<-min(min(x),min(y))
    mx<-max(max(x),max(y))
    lim<-c(mn,mx)

    plot(x,y,xlab=xlab,ylab=ylab,cex=0.5,pch=20,xlim=lim,ylim=lim)
    abline(0,1,col="red",lwd=0.4)
    s<-sd(x)+sd(y)
    abline(s,1,col="blue",lwd=0.4,lty="dashed")
    abline(-s,1,col="blue",lwd=0.4,lty="dashed")
    grid(lty=1,lwd=0.4)

    qaqc.stats(x,y)

    co<-cor(x,y)
    cl<-ifelse(co<corc,"red","black")
    mtext(paste("cor:",formatC(co,digits=2,format="f")),line=0,side=3,cex=0.5,adj=1,col=cl)
  }

  # Function to plot ranked scatter graph
  qaqc.rank<-function(x,y)
  {
    mn<-min(min(x),min(y))
    mx<-max(max(x),max(y))
    lim<-c(mn,mx)

    plot(sort(x),sort(y),xlab=paste(xlab,"(ranked)"),ylab=paste(ylab,"(ranked)"),cex=0.5,pch=20,xlim=lim,ylim=lim)
    abline(0,1,col="red",lwd=0.4)
    grid(lty=1,lwd=0.4)
  }

  # Function to plot HARD
  qaqc.hard<-function(x,y)
  {
    y<-100*abs(x-y)/(x+y)
    y<-sort(y)
    x<-100*seq(1,length(y),1)/length(y)

    plot(x,y,type="l",ylim=c(0,ymax),xlab="Rank %",ylab="HARD %")
    abline(h=hard,col="red",lwd=0.4)
    grid(lty=1,lwd=0.4)

    mtext(paste(hard,"%"),side=4,col="red",cex=0.5,at=hard)

    a<-approx(y,x,xout=5,ties="ordered")$y
    abline(v=a,col="red",lwd=0.4)
    mtext(paste(formatC(a,digits=0,format="f"),"%"),side=3,col="red",cex=0.5,at=a)
  }

  # Function to either plot 'No data' or skip
  qaqc.nodata<-function(lab)
  {
    if(!skip)
    {
      plot.new()
      text(0.5,0.5,"No data")
      box()
      mtext(lab,line=4,side=2,cex=0.8)

      plot.new()
      text(0.5,0.5,"No data")
      box()

      plot.new()
      text(0.5,0.5,"No data")
      box()
    }
  }

  if(length(ofld)!=length(rfld))
    return

  # Set plot region
  pdf(paste(output,"pdf",sep="."),width=8,height=11,onefile=TRUE,title=title)
  op=par(mfrow=c(4,3),mar=c(3,3,2,2),omi=c(0.5,0.5,0.5,0.2),mgp=c(2,1,0),cex.axis=0.6,cex.lab=0.8)

  # Get data
  data<-read.csv(paste(input,"csv",sep="."),row.names=NULL)

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