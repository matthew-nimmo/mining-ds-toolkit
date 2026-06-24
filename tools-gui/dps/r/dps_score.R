dps_score <- function(dps) {
  if (!is(dps, "dps")) {
    return(dps)
  }

  if (is.null(dps$dataset)) {
    return(dps)
  }

  z1 <- new_score(dps$dataset, dps$datafiles, dps$metadata, dps$profile) |>
    score_accessibility(level="doc") |>
    score_metadata(level="doc") |>
    score()

  dps$score <- list(
    table = z1,
    fields = NULL
  )

  if (is.null(dps$datafiles)) {
    return(dps)
  }
  
  if (is.null(dps$metadata)) {
    return(dps)
  }
  
  if (is.null(dps$profile)) {
    return(dps)
  }

  z2 <- new_score(dps$dataset, dps$datafiles, dps$metadata, dps$profile) |>
    score_accessibility(level="table") |>
    score_metadata(level="table") |>
    score_completeness() |>
    score_complexity() |>
    score_consistency() |>
    score_skewness() |>
    score_uniqueness() |>
    score_usability() |>
    score()

  dps$score$fields <- z2

  return(dps)
}
