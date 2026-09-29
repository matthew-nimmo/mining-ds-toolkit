assays <- function(obj)
{
	cols <- attr(obj,'assays')
	obj[cols]
}

"assays<-" <- function(x, value)
{
	if(length(value)<1)
		Stop("No assays.")

	isin <- value %in% names(x)
	if(any(!isin))
		stop("Columns do not exist.")

	attr(x,'assays') <- value

	if(!is(x,'assays'))
		class(x) <- c("assays",class(x))
	x
}

cor.assays <- function(x, zone=NULL, fname=NULL, outliers=FALSE, extreme=FALSE)
{
	f1 <- function(a, b)
	{
		if(is.null(zone))
		{
			u1 <- x[[a]]
			u2 <- x[[b]]
		}
		else
		{
			u1 <- x[zone,a]
			u2 <- x[zone,b]
		}
		if(all(is.na(u1)) || all(is.na(u2)))
			return(NULL)

		if(outliers)
		{
			ex <- ifelse(extreme,3,1.5)
			a1 <- which(u1 %in% boxplot.stats(u1,coef=ex)$out)
			a2 <- which(u2 %in% boxplot.stats(u2,coef=ex)$out)
			outliers <- union(a1,a2)
			if(length(outliers)>0)
			{
				u1 <- u1[-outliers]
				u2 <- u2[-outliers]
			}
		}
		k <- suppressWarnings(cor(u1,u2,use='na.or.complete',method='pearson'))
		v <- round(k,4)
		v
	}

	ass <- colnames(assays(x))
	u <- sapply(ass,function(a) sapply(ass, function(b) f1(a,b)))
	n <- length(ass)
	u <- as.data.frame(u)
	i <- sapply(1:ncol(u),function(i) sum(u[,i],na.rm=T))
	i <- order(i,decreasing=T)
	if(!is.null(fname))
		write.csv(u[i,i],fname,quote=F)
	u[i,i]
}

hist.assays <- function(x, main=NULL, png=T, col=NULL, col.grid=NULL, log=T, xlimit=function(o) quantile(o,probs=c(0.005,0.995),na.rm=T), ...)
{
	f1 <- function(i, k)
	{
		x <- z[[i]]
		if(length(x[!is.na(x)])<2)
			return(NULL)

		if (is.null(xlimit))
			r <- range(x, na.rm=T)
		else
			r <- xlimit(x)

		if(png)
		{
			png(paste(i,"_hist.png",sep=''))
			e <- try(histplot(x,main=i,sub=main,xlab=i,breaks='Scott',log=log,col=col,col.grid=col.grid,xlim=r,...),TRUE)
			if(inherits(e,'try-error'))
				histplot(x,main=i,sub=main,xlab=i,breaks='Scott',log=F,col=col,col.grid=col.grid,xlim=r,...)
			dev.off()
		}
		else
		{
			e <- try(histplot(x,main=i,sub=main,xlab=i,breaks='Scott',log=log,col=col,col.grid=col.grid,xlim=r,...),TRUE)
			if(inherits(e,'try-error'))
				histplot(x,main=i,sub=main,xlab=i,breaks='Scott',log=F,col=col,col.grid=col.grid,xlim=r,...)
		}
	}

	f2 <- function(i, k)
	{
		x <- z[dom==k,i]
		if(length(x[!is.na(x)])<2)
			return(NULL)

		if (is.null(xlimit))
			r <- range(x, na.rm=T)
		else
			r <- xlimit(x)

		if(png)
		{
			png(paste(i,"_hist_",zone,k,".png",sep=''))
			e <- try(histplot(x,main=paste(i," [",zone,"=",k,"]",sep=""),sub=main,xlab=i,breaks='Scott',log=log,col=col,col.grid=col.grid,xlim=r,...),TRUE)
			if(inherits(e,'try-error'))
				histplot(x,main=paste(i," [",zone,"=",k,"]",sep=""),sub=main,xlab=i,breaks='Scott',log=F,col=col,col.grid=col.grid,xlim=r,...)
			dev.off()
		}
		else
		{
			e <- try(histplot(x,main=paste(i," [",zone,"=",k,"]",sep=""),sub=main,xlab=i,breaks='Scott',log=log,col=col,col.grid=col.grid,xlim=r,...),TRUE)
			if(inherits(e,'try-error'))
				histplot(x,main=paste(i," [",zone,"=",k,"]",sep=""),sub=main,xlab=i,breaks='Scott',log=F,col=col,col.grid=col.grid,xlim=r,...)
		}
	}

	z <- assays(x)
	zone <- attr(x,"domain")
	dom <- domain(x)
	if(is.null(zone))
	{
		sapply(colnames(z), f1)
	}
	else
	{
		doms <- sort(unique(x[[zone]]))
		sapply(colnames(z), function(i) sapply(doms, function(k) f2(i,k)))
	}
}

cdf.assays <- function(x, log=TRUE, prob=TRUE)
{
	p1 <- function(i)
	{
		y <- cdf(x[[i]],weight=w)
		y
	}

	p2 <- function(i)
	{
		f <- as.formula(paste(i,"~",zone,sep=''))
		y <- cdf(f,x,weight=w)
		y
	}

	w <- weights(x, defaults=F)
	ass <- attr(x,"assays")
	zone <- attr(x,"domain")
	if(is.null(zone))
	{
		value <- sapply(ass, p1)
	}
	else
	{
		value <- sapply(ass, p2, simplify=F)
	}

	value <- value[!is.na(value)]

	class(value) <- c("cdf.assays",class(value))
	attr(value,"domain") <- attr(x,"domain")
	attr(value,"weighted") <- !is.null(attr(x,"weights"))

	value
}

plot.cdf.assays <- function(x, main=NULL, png=T, ...)
{
	p1 <- function(i)
	{
		u <- x[[i]]
		if(is.null(u))
			return(NULL)
		if(png)
		{
			if(is.null(zone))
			{
				png(paste(ass[i],"_cdf_",px,".png",sep=''))
				plot(u,main=ass[i],xlab=ass[i],pch=20,cex=1,show.all=T,...)
			}
			else
			{
				png(paste(paste(ass[i],"cdf",zone,px,sep='_'),".png",sep=''))
				plot(u,main=ass[i],pch=20,cex=1,show.all=T,...)
			}
			mtext(main,side=3,line=0.5,cex=0.8)
			dev.off()
		}
		else
		{
			if(is.null(zone))
				plot(u,main=ass[i],xlab=ass[i],pch=20,cex=1,show.all=T,...)
			else
				plot(u,main=ass[i],pch=20,cex=1,show.all=T,...)
			mtext(main,side=3,line=0.5,cex=0.8)
		}
	}

	if(attr(x,"weighted"))
		px <- "weight"
	else
		px <- "noweight"

	zone <- attr(x,"domain")

	n <- length(x)
	ass <- names(x)
	sapply(1:n, p1)
}

boxplot.assays <- function(x, main='', png=T, suffix=NULL, col=NULL, width=480, height=480, outline=F, ...)
{
	p1 <- function(i)
	{
		u <- x[[i]]
		if(all(is.na(u)))
			return(NULL)

		f <- as.formula(paste(i,"~",zone,sep=''))

		if(png)
		{
			if(is.null(suffix))
				fname <- paste(i,"_boxplot_",zone,".png",sep='')
			else
				fname <- paste(i,"_boxplot_",zone,"_",suffix,".png",sep='')
			png(fname,width=width,height=height)
			k <- boxplot(f,data=x,main=i,xlab=zone,ylab=i,varwidth=T,medlwd=1,pch=16,cex=0.5,outline=outline,col=col,...)
			mtext(main,side=3,line=0.5,cex=0.8)
			dev.off()
		}
		else
		{
			k <- boxplot(f,data=x,main=i,xlab=zone,ylab=i,varwidth=T,medlwd=1,pch=16,cex=0.5,outline=outline,col=col,...)
			mtext(main,side=3,line=0.5,cex=0.8)
		}
		k
	}

	#zone <- colnames(domain(x))
	#ass <- colnames(assays(x))
	zone <- attr(x,"domain")
	ass <- attr(x,"assays")

	if(is.null(zone))
		stop("No domain.")

	sapply(ass, p1)
}

scatter <- function(x, y=x, main=NULL, xlab=ToLabel(x), ylab=ToLabel(y), xlim=NULL, ylim=NULL, png=T, cex=0.4, pch=19, ...)
  UseMethod('scatter',x)

scatter.default <- function(x, y, main=NULL, xlab=ToLabel(x), ylab=ToLabel(y), xlim=NULL, ylim=NULL, png=T, cex=0.4, pch=19, ...)
{
	g <- function() {plotaxis(1); plotaxis(2)}

	i <- !is.na(x) & !is.na(y)
	x <- x[i]
	y <- y[i]

	if (is.null(xlim))
	{
		#u <- sort(x)
		#v <- c(0,diff(u))
		#u <- u[v>sd(u)]
		xlim <- range(x, na.rm=T)
		#xlim <- c(min(x),min(u))
	}
	if (is.null(ylim))
	{
		#u <- sort(y)
		#v <- c(0,diff(u))
		#u <- u[v>sd(u)]
		ylim <- range(y, na.rm=T)
		#ylim <- c(min(y),min(u))
	}

	plot(x,y,xlim=xlim,ylim=ylim,main=main,xlab=xlab,ylab=ylab,panel.first=g(),cex=cex,pch=pch)
}

scatter.assays <- function(x, main='', png=T)
{
	f1 <- function(a, b)
	{
		if(png)
		{
			png(paste(b,a,"scatter.png",sep="_"))
			scatter(x[[a]],x[[b]],main=paste(b,'vs',a),xlab=a,ylab=b)
			mtext(main,side=3,line=0.5,cex=0.8)
			dev.off()
		}
		else
		{
			scatter(x[[a]],x[[b]],main=paste(b,'vs',a),xlab=a,ylab=b)
			mtext(main,side=3,line=0.5,cex=0.8)
		}
	}

	z <- assays(x)
	ass <- colnames(z)
	sapply(ass,function(x) sapply(ass, function(y) if(x!=y) f1(x,y)))
}

formula.assays <- function(x, ...)
{
	if(!is(x,"domain"))
		stop("No domain defined.")

	lhs <- paste(attr(x,'assays'),collapse="+")
	rhs <- attr(x,'domain')

	as.formula(paste(lhs,rhs,sep="~"))
}

stats <- function(x, w, label, csv, cutprob, round)
  UseMethod('stats',x)

stats.default <- function(x, w=NULL, label=ToLabel(x), csv=NULL, cutprob=0.995, round=T)
{
	if (is.null(w))
		w <- rep(1, length(x))

	v <- vector(length=15)

	if (is.null(x) || length(x[!is.na(x)])==0)
	{
		v <- c(0,0,rep(NA,13))
	}
	else
	{
		v[1] <- length(x[!is.na(x)])
		v[2] <- length(x[is.na(x)])
		v[3] <- min(x, na.rm=TRUE)
		v[4] <- max(x, na.rm=TRUE)
		v[5] <- v[4] - v[3]
		v[6] <- wsum(x, w)
		v[7] <- wmean(x, w, na.rm=TRUE)
		v[8] <- wvar(x, w, na.rm=TRUE)
		v[9] <- wsd(x, w, na.rm=TRUE) / wmean(x, w, na.rm=TRUE)
		q <- wquantile(x, w, probs=c(0.1,0.25,0.5,0.75,0.9,cutprob), na.rm=TRUE)
		v[10] <- q[1]
		v[11] <- q[2]
		v[12] <- q[3]
		v[13] <- q[4]
		v[14] <- q[5]
		v[15] <- q[6]
		if (round==TRUE)
		{
			v[7:8] <- round(v[7:8],3)
			v[9:15] <- round(v[9:15],2)
		}
	}
	colls <- c('Number','Missing','Min','Max','Range','Total','Mean','Var','COV','10%','25%','50%','75%','90%',paste(as.character(cutprob*100),'%',sep=''))
	names(v) <- colls
	v[!is.finite(v)] <- NA

	if (is.null(csv))
	{
		return(v)
	}
	else
	{
		cat(c('GRADE',colls,'\n'),file=csv,sep=",")
		cat(c(label,v,'\n'),file=csv,sep=",",append=T)
	}
}

stats.assays <- function(x, w=NULL, csv=NULL, cutprob=0.995, round=T)
{
	z <- assays(x)
	dom <- attr(x, 'domain')
	zone <- domain(x)
	if (is.null(w))
		w <- weights(x)
 
	f1 <- function(assay, code=NULL)
	{
		if (is.null(code))
		{
			s <- stats.default(x[[assay]], label=assay, w=w, cutprob=cutprob)
			s <- c(GRADE=assay,s)
		}
		else
		{
			s <- stats.default(x[zone==code,assay], label=assay, w=w[zone==code], cutprob=cutprob)
			s <- c(DOMAIN=code,GRADE=assay,s)
			names(s)[[1]] <- dom
		}
		s
	}

	if (is.null(dom))
	{
		ass <- colnames(z)
		m <- sapply(ass, f1)
	}
	else
	{
		doms <- sort(unique(zone))
		ass <- colnames(z)
		a1 <- rep(doms, length(ass))
		a2 <- sort(rep(ass,length(doms)))
		m <- mapply(function(j,i) f1(i,code=j), a1, a2)
	}

	colls <- dimnames(m)[[1]]

	if (is.null(csv))
	{
		return(m)
	}
	else
	{
		cat(c(colls,'\n'),file=csv,sep=",")
		write(m, csv, ncolumns=dim(m)[[1]], sep=",", append=TRUE)
	}
}
