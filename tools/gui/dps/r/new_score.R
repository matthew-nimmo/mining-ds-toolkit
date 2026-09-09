new_score <- function(dataset, datafiles, metadata, profile) {
  z <- list(
    dataset = dataset,
    datafiles = datafiles,
    metadata = metadata,
    profile = profile,
    score = list()
  )
  class(z) <- "score"

  return(z)
}

score <- function(object) {
  if (!is(object, "score")) {
    return(NULL)
  }
  return(object$score)
}
