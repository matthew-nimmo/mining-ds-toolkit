dps_cluster <- function(dps) {
  if (!is(dps, "dps")) {
    return(dps)
  }

  if (is.null(dps$metadata)) {
    return(dps)
  }

  x <- dps$metadata |>
    datafile_class() |>
    unlist()

  y <- dps$metadata |>
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
  dps$cluster <- df

  return(dps)
}
