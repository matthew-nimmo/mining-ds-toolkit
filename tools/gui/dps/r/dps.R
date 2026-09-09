dps <- function(dir) {
  f <-  dataset(dir)
  if (is.null(f)) {
    return(NULL)
  }

  x <- list(
    dataset = f
  )
  class(x) <- c("dps", class(x))

  x <- x |>
    dps_datafiles() |>
    dps_metadata() |>
    dps_cluster() |>
    dps_profile() |>
    dps_score()

  return(x)
}

do_dps <- function(proj_id, proj_dir) {
  suppressWarnings(dps(proj_dir) |>
                     dps_export(proj_id))
}
