dqs_profile <- function(dqs) {
  if (!is(dqs, "dqs")) {
    return(dqs)
  }

  if (is.null(dqs$datafiles)) {
    return(dqs)
  }

  if (is.null(dqs$metadata)) {
    return(dqs)
  }

  x <- lapply(1:length(dqs$datafiles), \(i) {
    profile_data(dqs$datafiles[[i]], dqs$metadata[[i]])
  })
  dqs$profile <- x

  return(dqs)
}
