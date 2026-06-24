dqs <- function(dir) {
  f <-  dataset(dir)
  if (is.null(f)) {
    return(NULL)
  }

  x <- list(
    dataset = f
  )
  class(x) <- c("dqs", class(x))

  x <- x |>
    dqs_datafiles() |>
    dqs_metadata() |>
    dqs_cluster() |>
    dqs_profile() |>
    dqs_score()

  return(x)
}

do_dqs <- function(proj_id, proj_dir) {
  suppressWarnings(dqs(proj_dir) |>
                     dqs_export(proj_id))
}
