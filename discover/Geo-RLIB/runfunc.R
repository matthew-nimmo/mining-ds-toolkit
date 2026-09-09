# Modified from Jarek Tuszynski - code in caTools package

runmin <- function(x, k, endrule=c("NA", "trim", "keep", "constant", "func"))
{
  n <- length(x)
  k <- as.integer(k)
  k2 <- k %/% 2

  if(k == 1)
  {
    y <- x
    attr(y, "k") <- k
    return(x)
  }

  if(k2 < 1)
    stop("'k' must be positive")

  if(k > n)
    k2 = (n - 1) %/% 2

  if(k != 1 + 2 * k2)
    warning("'k' must be odd number between 3 and 'length(x)'.",
      "Changing 'k' to ", k <- as.integer(1 + 2 * k2))

  y <- double(n)

  if(n == k)
    y[k2+1] <- min(x)
  else
  {
    a <- y[k2 + 1] <- min(x[1:k])

    for(i in (2 + k2):(n - k2))
    {
      if(a == y[i - 1])
        y[i] <- min(x[(i - k2):(i + k2)])
      else
        y[i] = min(y[i - 1], x[i + k2])

      a <- x[i-k2]
    }
  }

  y <- EndRule(x, y, k, endrule, min)

  return(y)
}

runmean <- function(x, k, endrule=c("NA", "trim", "keep", "constant", "func"))
{
  n <- length(x)
  k <- as.integer(k)
  k2 <- k %/% 2

  if (k == 1)
  {
    y <- x
    attr(y, "k") <- k
    return (x)
  }

  if (k2 < 1)
    stop("'k' must be positive")
  if (k > n)
    k2 <- (n - 1) %/% 2
  if (k != 1 + 2 * k2)
    warning("'k' must be odd number between 3 and 'length(x)'.",
      "Changing 'k' to ", k <- as.integer(1 + 2 * k2))

  y <- double(n)

  if(n == k)
    y[k2 + 1] <- sum(x) / n
  else
  {
    y <- c(sum(x[1:k]), diff(x, k));
    y <- cumsum(y) / k
    y <- c(rep(0, k2), y, rep(0, k2))
  }

  y = EndRule(x, y, k, endrule, mean)

  return(y)
}

runmax <- function(x, k, endrule=c("NA", "trim", "keep", "constant", "func"))
{
  n <- length(x)
  k <- as.integer(k)
  k2 <- k %/% 2

  if(k == 1)
  {
    y <- x
    attr(y, "k") <- k
    return(x)
  }

  if(k2 < 1)
    stop("'k' must be positive")

  if(k > n)
    k2 = (n - 1) %/% 2

  if(k != 1 + 2 * k2)
    warning("'k' must be odd number bigger than 3 and smaller than 'length(x)'.",
      "Changing 'k' to ", k <- as.integer(1 + 2 * k2))

  y <- double(n)

  if(n == k)
    y[k2 + 1] <- max(x)
  else
  {
    k2 <- k %/% 2
    a <- y[k2 + 1] <- max(x[1:k])

    for(i in (2 + k2):(n - k2))
    {
      if(a == y[i - 1])
        y[i] <- max(x[(i - k2):(i + k2)])
      else
        y[i] <- max(y[i - 1], x[i + k2])

      a <- x[i - k2]
    }
  }

  y = EndRule(x, y, k, endrule, max)

  return(y)
}

EndRule <- function(x, y, k, endrule=c("NA", "trim", "keep", "constant", "func"), Func, ...)
{
  n = length(x)

  if(length(y)!=n)
    stop("vectors 'x' and 'y' have to have the same length.")

  k = as.integer(k)
  k2 = k %/% 2

  if(k2 < 1)
    k2 <- 1
  if(k > n)
    k2 <- (n - 1) %/% 2
  if(k != 1 + 2 * k2)
    warning("'k' must be odd number between 3 and 'length(x)'.",
      "Changing 'k' to ", k <- as.integer(1 + 2 * k2))

  idx1 <- 1:k2
  idx2 <- (n - k2 + 1):n
  endrule <- match.arg(endrule)

  if(endrule == "NA")
  {
    y[idx1] <- NA
    y[idx2] <- NA
  }
  else if(endrule == "keep")
  {
    y[idx1] <- x[idx1]
    y[idx2] <- x[idx2]
  }
  else if(endrule == "constant")
  {
    y[idx1] <- y[k2 + 1]
    y[idx2] <- y[n - k2]
  }
  else if(endrule == "trim")
  {
    y = y[(k2 + 1):(n - k2)]
  }
  else if(endrule=="func")
  {
    for(i in idx1) y[i] <- Func(x[1:i], ...)
    for(i in idx2) y[i] <- Func(x[i:n], ...)
  }

  attr(y, "k") <- k

  return(y)
}
