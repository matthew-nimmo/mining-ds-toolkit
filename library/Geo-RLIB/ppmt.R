#' Projection Pursuit Multivariate Transform (PPMT).
#'
#' Transform a multivariate data set into a multigaussian. All variables are transformed to a gaussian distribution and
#' are decorrelated.
#' @param x data frame of numeric variables.
#'
#' @return list containing transformed values.
#'
#' @references \url{http://www.geostatisticslessons.com/lessons/ppmt}
#'
#' @keywords ppmt
#' @export
ppmt <- function(x) {
  # Zscore 1.
  z.norms1 <- lapply(x, nscore)
  z.nscore1 <- sapply(1:ncol(x), function(i) z.norms1[[i]]$score)
  z.nscore1 <- as.data.frame(z.nscore1)
  names(z.nscore1) <- names(x)

  # Sphereing
  z.cv <- cov(z.nscore1)
  z.ei <- eigen(z.cv)
  V <- z.ei$vectors
  z.W <- V %*% diag(1 / sqrt(z.ei$values)) %*% t(V)
  z.A <- solve(z.W)
  z.sphere <- as.matrix(z.nscore1) %*% z.W
  z.sphere <- as.data.frame(z.sphere)
  names(z.sphere) <- df.flds

  # Projection pursuit.
  #z.pp <- fastICA::fastICA(z.nscore1, ncol(x), alg.typ="deflation", fun="logcosh",
  #                         alpha=1, method="C", row.norm=FALSE, maxit=100,
  #                         tol=1e-2, verbose=FALSE)
  require(ica)
  z.pp <-  icafast(z.sphere, ncol(z.sphere), center=FALSE,
                   alg="def", fun="logcosh")
  z.S <- as.data.frame(z.pp$S)
  names(z.S) <- names(z.sphere)

  # Zscore 2.
  z.norms2 <- lapply(z.S, nscore)
  z.nscore2 <- sapply(1:ncol(z.S), function(i) z.norms2[[i]]$score)
  z.nscore2 <- as.data.frame(z.nscore2)
  names(z.nscore2) <- names(x)

  y <- list(z.norms1=z.norms1, z.sphere=list(W=z.W, A=z.A),
            z.pp=z.pp, z.norms2=z.norms2, X=x, Y=z.nscore2)

  return(y)
}
