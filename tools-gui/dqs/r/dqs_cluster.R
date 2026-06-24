dqs_cluster <- function(dqs) {
  if (!is(dqs, "dqs")) {
    return(dqs)
  }

  if (is.null(dqs$metadata)) {
    return(dqs)
  }

  x <- dqs$metadata |>
    datafile_class() |>
    unlist()

  y <- dqs$metadata |>
    lapply("[[", "colnames") |>
    lapply(\(x) {
      if (is.null(x)) {
        return(c(empty=1))
      }
      y <- table(x)
      n <- names(y)
      y <- as.numeric(y)
      names(y) <- n
      return(y)
    }) |>
    clope(progress=FALSE)

  df <- data.frame(file_class = x,
                   file_cluster = y$Clust)
  dqs$cluster <- df

  return(dqs)
}
