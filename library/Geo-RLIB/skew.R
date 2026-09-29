skew <- function(x, na.rm=TRUE)
{ 
  if(na.rm)
    x <- na.exclude(x)

  n <- length(x)
  m <- mean(x)
  v <- x - m

  g <- mean(v^3) / (mean(v^2))^1.5
  sqrt(n*(n-1))/(n-2)*g
}
