#' Normal score back transform.
#'
#' Backtransform normal scores.
#' @param x numeric values to calculate the normal score.
#'
#' @return numeric vector of scores.
#'
#' @references from \url{https://msu.edu/~ashton/temp/nscore.R}
#'
#'
#' @keywords nscore
#' @export
nscore <- function(x) {
  y <- qqnorm(x, plot.it=FALSE)$x
  trn.table <- data.frame(x=sort(x), score=sort(y))
  y <- list(score=y, trn.table=trn.table,
            min=max(0, min(x, na.rm=TRUE)-sd(x, na.rm=TRUE)),
            max=max(x, na.rm=TRUE)+sd(x, na.rm=TRUE))
  return (y)
}
