#' Inverse Projection Pursuit Multivariate Transform (PPMT).
#'
#' Perform the inverse Projection Pursuit Multivariate Transform.
#' @param x data frame of numeric variables.
#' @param ppmt list as returned by the \code{ppmt} function.
#' @param method one of `back`, `interp` or `MBCn`. Default `back`.
#'
#' @return data frame of back-transformed values.
#'
#' @references \url{http://www.geostatisticslessons.com/lessons/ppmt}
#'
#' @keywords ppmt
#' @export
ippmt <- function(x, ppmt, method=c("back", "interp", "MBCn")) {
  method <- match.arg(method, c("back", "interp", "MBCn"))

  f.back <- function(x) {
    # Zscore 2 inverse.
    y <- sapply(1:ncol(x), function(i) nsbacktr(x[,i], ppmt$z.norms2[[i]]))

    # Projection pursuit inverse.
    #y <- y %*% ppmt$z.pp$A
    y <- tcrossprod(y, ppmt$z.pp$M)

    # Sphereing inverse.
    y <- y %*% ppmt$z.sphere$A

    # Zscore 1 inverse.
    y <- sapply(1:ncol(y), function(i) nsbacktr(y[,i], ppmt$z.norms1[[i]]))

    y <- as.data.frame(y)
    names(y) <- colnames(ppmt$Y)

    return(y)
  }

  f.interp <- function(x) {
    require(FNN)

    k <- ncol(x)
    nn <- get.knnx(df.ppmt$Y, x, k=k, algorithm="kd_tree")

    y <- sapply(1:nrow(x), function(i) {
      w <- 1 / nn$nn.dist[i,]
      w <- w / sum(w)
      y <- t(df.ppmt$X[nn$nn.index[i,],]) %*% w
      return(y)
    })
    y <- as.data.frame(t(y))
    names(y) <- names(x)

    return(y)
  }

  f.MBCn <- function(x) {
    require(MBC)
    u <- MBCn(as.matrix(df.ppmt$X), as.matrix(df.ppmt$Y), as.matrix(x),
              iter=30, silent=TRUE, ratio.max=1, trace=0.01)

    return(u$mhat.p)
  }

  y <- switch(method,
              back = f.back(x),
              interp = f.interp(x),
              MBCn = f.MBCn(x))

  y <- as.data.frame(y)
  names(y) <- names(x)

  return(y)
}
