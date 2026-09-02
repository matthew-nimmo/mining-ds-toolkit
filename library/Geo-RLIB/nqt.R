#' Normal Quantile Transform.
#'
#' Similar to nscore but able to apply to new data.
#' @param x numeric values to calculate the normal score.
#' @param k number of degrees for fitting the zscores.
#'
#' @return list with scoring and inverse scoring functions.
#'
#' @keywords nqt
#' @export
nqt <- function(x, k=30) {
  require(mgcv)

  y <- qqnorm(x, plot.it=FALSE)$x
  m1 <- gam(y ~ s(x, bs="cr", k=k))

  y <- predict(m1, newdata=data.frame(x=x))
  m2 <- gam(x ~ s(y, bs="cr", k=k))

  score <- function(x) {
    y <- predict(m1, newdata=data.frame(x=x))
    return(y)
  }

  iscore <- function(y) {
    x <- predict(m2, newdata=data.frame(y=y))
    return(x)
  }

  return(list(score=score, iscore=iscore))
}
