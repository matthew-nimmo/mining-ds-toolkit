require(gstat)

varmap <- function(f, data, var=T, plot=T, idw=3, save=NULL, main=NULL, ...)
{
  # call varmap(CaCO3~X+Y+Z, main=NULL)

  f1 <- formula(paste(f[[2]], "~ 1"))
  f2 <- formula(paste("~", f[3]))

  g <- gstat(formula=f1, locations=f2, data=data)
  v <- variogram(g, alpha=seq(0,350,by=10), tol.hor=10, ...)

  if(is.null(main))
    main <- paste("Semivariogram Map of", f[[2]])

  if(var)
    vr <- var(eval(f[[2]], data), na.rm=T)
  else
    vr <- NULL

  if(plot)
    plot.varmap(v, var=vr, main=main, save=save)

  v
}

plot.varmap <- function(v, var=NULL, idw=3, main=paste("Semivariogram Map of",deparse(substitute(x))), save=NULL)
{
  v <- as.data.frame(v)

  v$x1 <- v$dist*(sin(v$dir.hor*pi/180))
  v$x2 <- v$dist*(cos(v$dir.hor*pi/180))

  p <- pretty(v$dist)
  r <- p[length(p)]
  n <- signif(r*sin(10*pi/180)/5, 2)

  # Create circle for limit of Variogram Map
  circle.x <- seq(0, 360, by=5)
  circle.y <- circle.x
  circle.x <- sin(circle.x*pi/180)
  circle.y <- cos(circle.y*pi/180)

  # Estimate gamma onto regular grid for plotting
  v.grid <- makegrid(SpatialPoints(expand.grid(c(-p,p),c(-p,p))), nsig=4, cellsize=n)
  v.grid <- v.grid[point.in.polygon(v.grid$x1, v.grid$x2, r*circle.x, r*circle.y)==1,]

  z <- idw(gamma~1, ~x1+x2, v, v.grid, maxdist=10*n, nmin=1, nmax=10, idp=idw)

  # Plot Variogram Map
  ncols <- 20
  cols <- colorRampPalette(c("steelblue1","darkblue","darkred","red1"))(ncols)
  if(!is.null(var))
  {
    a <- z[!is.na(z$var1.pred),]
    a$var1.pred <- a$var1.pred - var; ar <- range(a$var1.pred)
    a$var1.pred[a$var1.pred<0] <- -(a$var1.pred[a$var1.pred<0] / ar[1])
    a$var1.pred[a$var1.pred>0] <- a$var1.pred[a$var1.pred>0] / ar[2]
    image(a, xaxs="r", yaxs="r", col=cols, axes=F, xlim=c(-1.1*r,1.1*r), ylim=c(-1.1*r,1.1*r), main=main)
    a <- xyz2img(z)
    contour(a$x, a$y, a$z, levels=var, drawlabels=F, add=T, col="grey", lwd=1.5*par("lwd"))
  }
  else
    image(z, xaxs="r", yaxs="r", col=cols, axes=F, xlim=c(-1.1*r,1.1*r), ylim=c(-1.1*r,1.1*r), main=main)
  axis(1, labels=c(-p[length(p):1],p), at=c(-p[length(p):1],p))
  axis(2, labels=c(-p[length(p):1],p), at=c(-p[length(p):1],p))

  # Plot radial grid
  for(i in p)
    polygon(i*circle.x, i*circle.y, border="grey", lwd=0.5*par("lwd"))

  rr <- seq(0, 350, by=10)
  rr.x <- r*(sin(rr*pi/180))
  rr.y <- r*(cos(rr*pi/180))
  segments(0, 0, rr.x, rr.y, col="grey")
  text(1.07*rr.x, 1.07*rr.y, labels=rr, cex=0.6, adj=c(0.5,0.5))

  # Plot legend
  u <- par("usr");
  rx <- u[2] - u[1]; x <- 0.85*u[2]
  ry <- u[4] - u[3]; y <- 0.9*u[4]
  dx <- 0.02*rx; dy <- 0.01*ry
  rect(x, y-dy*1:ncols, x+dx, y-dy*0:(ncols-1), col=cols[ncols:1], border=NA); rect(x, y-dy*ncols, x+dx, y)
  text(x-2*dx, y-dy*ncols/2, labels="\\*g", cex=par("cex"), adj=0, vfont=c("serif symbol","plain"))
  text(x+2*dx, y-dy*c(1,ncols), labels=c("high","low"), cex=0.7*par("cex"), adj=c(0,0))

  if(!is.null(save))
    savePlot(save, type="emf")
}
