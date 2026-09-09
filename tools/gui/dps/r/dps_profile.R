dps_profile <- function(dps) {
  if (!is(dps, "dps")) {
    return(dps)
  }

  if (is.null(dps$datafiles)) {
    return(dps)
  }

  if (is.null(dps$metadata)) {
    return(dps)
  }

  x <- lapply(1:length(dps$datafiles), \(i) {
    profile_data(dps$datafiles[[i]], dps$metadata[[i]])
  })
  dps$profile <- x

  return(dps)
}
