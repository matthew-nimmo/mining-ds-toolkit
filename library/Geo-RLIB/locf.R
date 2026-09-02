locf <- function(x, blank=is.na) {
  # Find the values
  if (is.function(blank)) {
    isnotblank <- !blank(x)
  } else {
    isnotblank <- x != blank 
  }
  # Fill down
  y <- x[which(isnotblank)][cumsum(isnotblank)]
  
  return(y)
}