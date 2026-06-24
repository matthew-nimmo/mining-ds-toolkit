dps_datafiles <- function(dps) {
  if (!is(dps, "dps")) {
    return(dps)
  }

  x <- dps$dataset |>
    dplyr::filter(data_type == "data")
  if (nrow(x) == 0) {
    dps$datafiles <- NULL
    return(dps)
  }

  x <- lapply(1:nrow(x), \(i) {
      new_datafile(x$file_path[i], x$file_ext[i], x$id[i])
    }) |>
    unlist()
  dps$datafiles <- x

  return(dps)
}
