ok <- function(x, y, cov, datax, datay, dataz)
{
  Kok <- function(dx, dy, cov)
  {
    d1 <- outer(dx, dx, FUN=function(x, y) (x-y)^2)
    d2 <- outer(dy, dy, FUN=function(x, y) (x-y)^2)
    K <- cov(sqrt(d1 + d2))
    K <- cbind(K, 1)
    K <- rbind(K, 1)
    K[nrow(K), ncol(K)] <- 0
    K
  }

  kok <- function(x, y, dx, dy, cov)
  {
    d1 <- (dx-x)^2
    d2 <- (dy-y)^2
    k <- cov(sqrt(d1 + d2))
    k[n+1] <- 1
    k
  }

  K <- Kok(datax, datay, cov)
  n <- length(datax)

  if(length(x)>1)
  {
    z <- matrix(data=NA, nrow=length(x), ncol=length(y))
    for(i in 1:length(x))
    {
      for(j in 1:length(y))
      {
        k <- kok(x[i], y[j], datax, datay, cov)
        w <- solve(K, k)
        z[i,j] <- as.numeric(t(w[1:n]) %*% dataz)
      }
    }
  }
  else
  {
    k <- kok(x, y, datax, datay, cov)
    w <- solve(K, k)
    z <- as.numeric(t(w[1:n]) %*% dataz)
  }

  invisible(z)
}

show_plot <- function(x, y, z, xp, yp)
{
  c <- heat.colors(10)
  c <- c(c[10:1],c)
  image(x, y, z, col=c)

  #contour(x, y, z, levels=seq(350,750,by=10), add=TRUE)
  contour(x, y, z, levels=seq(0,1,by=0.1), add=TRUE)

  points(xp, yp, pch=19); points(65, 137)
}

data.toy <- function()
{
  x <- c(61,63,64,68,71,73,75)
  y <- c(139,140,129,128,140,141,128)
  v <- c(477,696,227,646,606,791,783)
  u <- c(1,0,1,0,0,0,0)

  o <- list(x=x, y=y, v=v, u=u)
  o
}

sharpen <- function(z, p)
{
  if(p==0)
  {
    zz <- z
    zz[zz<=0.5] <- 0; zz[zz>0.5] <- 1
  }
  else
  {
    c <- (1-p)/(2*p)
    for(i in 1:length(z))
    {
      zz <- (z/p) - c
      zz[zz<0] <- 0; zz[zz>1] <- 1
    }
  }
  zz
}

go <-function(p)
{
  d <- data.toy()
  x <- 60:75; y <- 128:141

  z <- ok(x, y, function(h,...) (10 * exp(-0.3) ^ h), d$x, d$y, d$u)
  show_plot(x, y, sharpen(z,p), d$x, d$y)
}

go(0.5)
