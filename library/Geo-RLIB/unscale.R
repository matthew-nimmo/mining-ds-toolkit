unscale <- function(x) {
  x.scale <- attr(x, "scaled:scale")
  x.center <- attr(x, "scaled:center")
  y <- sweep(x, 2, x.scale, FUN="*")
  y <- sweep(y, 2, x.center, FUN="+")
}
