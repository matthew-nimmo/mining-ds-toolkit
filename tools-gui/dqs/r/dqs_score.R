dqs_score <- function(dqs) {
  if (!is(dqs, "dqs")) {
    return(dqs)
  }

  if (is.null(dqs$dataset)) {
    return(dqs)
  }

  z1 <- new_score(dqs$dataset, dqs$datafiles, dqs$metadata, dqs$profile) |>
    score_accessibility(level="doc") |>
    score_metadata(level="doc") |>
    score()

  dqs$score <- list(
    table = z1,
    fields = NULL
  )

  if (is.null(dqs$datafiles)) {
    return(dqs)
  }
  
  if (is.null(dqs$metadata)) {
    return(dqs)
  }
  
  if (is.null(dqs$profile)) {
    return(dqs)
  }

  z2 <- new_score(dqs$dataset, dqs$datafiles, dqs$metadata, dqs$profile) |>
    score_accessibility(level="table") |>
    score_metadata(level="table") |>
    score_completeness() |>
    score_complexity() |>
    score_consistency() |>
    score_skewness() |>
    score_uniqueness() |>
    score_usability() |>
    score()

  dqs$score$fields <- z2

  return(dqs)
}
