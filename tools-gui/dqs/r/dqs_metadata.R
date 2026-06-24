dqs_metadata <- function(dqs) {
  if (!is(dqs, "dqs")) {
    return(dqs)
  }

  if (is.null(dqs$datafiles)) {
    return(dqs)
  }

  x <- dqs$datafiles |>
    lapply(metadata)
  dqs$metadata <- x

  return(dqs)
}
