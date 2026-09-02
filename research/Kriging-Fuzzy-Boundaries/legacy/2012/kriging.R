data.toy <- function(indicator)
{
  x <- c(61,63,64,68,71,73,75)
  y <- c(139,140,129,128,140,141,128)
  v <- c(477,696,227,646,606,791,783)
  u <- c(1,0,1,0,0,0,0)
  o <- list(x=x, y=y, v=v, u=u)
  o
}

K.matrix <- function(dx, dy, cov)
{
  d1 <- outer(dx, dx, FUN=function(x, y) (x-y)^2)
  d2 <- outer(dy, dy, FUN=function(x, y) (x-y)^2)
  K <- cov(sqrt(d1 + d2))
  K
}

k.matrix <- function(x, y, dx, dy, cov)
{
  d1 <- (dx-x)^2
  d2 <- (dy-y)^2
  k <- cov(sqrt(d1 + d2))
  k
}

delta.matrix <- function(K, fuzzy)
{
  f1 <- 1 - fuzzy

  # Build Delta matrix
  d1 <- matrix(-1, nrow=n, ncol=n)
  d1[row(d1)==col(d1)] <- 0
  d1 <- d1 * K[1:n,1:n]
  d1 <- f1 %*% d1
  d1[n+1] <- d1[1]
  d1 <- outer(rep(1, n+1), d1)
  d1 <- -K / d1
  d1[row(d1)==col(d1)] <- 1
  f1[n+1] <- 1; fuzzy[n+1] <- 0
  d2 <- outer(f1, fuzzy)
  d2[row(d2)==col(d2)] <- f1
  d <- d1 * d2
  d
}

ok.weight <- function(K, k, delta)
{
  K <- cbind(K, 1)
  K <- rbind(K, 1)
  K[nrow(K), ncol(K)] <- 0
  k[length(k)+1] <- 1

  # Solve
  w <- as.matrix(solve(K, k)[1:length(k)-1])
  w
}

ok <- function(x, y, cov, datax, datay, dataz, datad=NULL, fuzzy=NULL)
{
  if(is.null(datad))
    K <- K.matrix(datax, datay, cov)
  else
  {
    zz <- go2(TRUE, FALSE)
    zone1 <- outer(datad, datad, FUN=function(x,y) (x+y)/2)
    K <- K.matrix(datax, datay, function(h) cov(h, zone1))
  }

  if(!is.null(fuzzy))
    K <- delta(K, fuzzy) %*% K

  if(length(x)>1)
  {
    z <- matrix(data=NA, nrow=length(x), ncol=length(y))
    for(i in 1:length(x))
    {
      for(j in 1:length(y))
      {
        if(!is.null(datad))
        {
          zone2 <- (rep(zz[i,j], length(datay)) + datad)/2
          k <- k.matrix(x[i], y[j], datax, datay, function(h) cov(h, zone2))
        }
        else
          k <- k.matrix(x[i], y[j], datax, datay, cov)
        w <- ok.weight(K, k)
        z[i,j] <- as.numeric(t(w) %*% dataz)
      }
    }
  }
  else
  {
    k <- k.matrix(x, y, datax, datay, cov)
    w <- ok.weight(K, k)
    z <- as.numeric(t(w) %*% dataz)
  }

  invisible(z)
}

show_plot <- function(x, y, z, xp, yp)
{
  # Plot
  image(x, y, z, col=heat.colors(20)[20:1])
  contour(x, y, z, levels=seq(350,750,by=10), add=TRUE)
  points(xp, yp, pch=19)
  points(65, 137)
}

go1 <- function(indicator=FALSE, show=TRUE)
{
  d <- data.toy()
  x <- 60:75
  y <- 128:141
  cv <- function(h,...) (10 * exp(-0.3) ^ h)

  # Estimate for all nodes in matrix z
  if(indicator)
    v <- d$u
  else
    v <- d$v

  z <- ok(x, y, cv, d$x, d$y, v)

  if(indicator)
  {
    z[z<0.5] <- 0
    z[z>=0.5] <- 1
  }

  if(show) show_plot(x, y, z, d$x, d$y)

  invisible(z)
}

go2 <- function(indicator=FALSE, show=TRUE)
{
  # OK
  d <- data.toy()
  x <- 60:75
  y <- 128:141
  cv <- function(h,...) (10 * exp(-0.3) ^ h)

  if(indicator)
    v <- d$u
  else
    v <- d$v
  tr <- spatial::surf.ls(1, d$x, d$y, v)

  v <- (v - predict(tr, d$x, d$y, nrow(d)))

  z <- ok(x, y, cv, d$x, d$y, v)
  for(i in 1:length(x))
    z[i,] <- z[i,] + predict(tr, rep(x[i], length(y)), y, length(y))

  if(indicator)
  {
    z[z<0.5] <- 0
    z[z>=0.5] <- 1
  }

  if(show) show_plot(x, y, z, d$x, d$y)

  invisible(z)
}

go3 <- function(indicator=FALSE, show=TRUE, range=2)
{
  # OK
  d <- data.toy()
  x <- 60:75
  y <- 128:141
  cv <- function(h, z)
  {
    z[z==0] <- 10
    z[z==0.5] <- range
    z[z==1] <- 10
    (10 * exp(-3/z) ^ h)
  }

  z <- ok(x, y, cv, d$x, d$y, d$v, d$u, NULL)

  if(show) show_plot(x, y, z, d$x, d$y)

  invisible(z)
}

d <- data.toy()
z <- ok(65, 137, function(h,...) (10 * exp(-0.3) ^ h), d$x, d$y, d$v)
