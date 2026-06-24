encode_format <- function(x) {
  y <- sapply(x, value_format) |>
    data.frame()
  class(y) <- c("encode_format", "data.frame")

  return(y)
}