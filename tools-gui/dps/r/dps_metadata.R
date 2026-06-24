dps_metadata <- function(dps) {
  if (!is(dps, "dps")) {
    return(dps)
  }

  if (is.null(dps$datafiles)) {
    return(dps)
  }

  x <- dps$datafiles |>
    lapply(metadata)
  dps$metadata <- x

  return(dps)
}
