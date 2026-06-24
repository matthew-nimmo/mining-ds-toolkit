dqs_datafiles <- function(dqs) {
  if (!is(dqs, "dqs")) {
    return(dqs)
  }

  x <- dqs$dataset |>
    dplyr::filter(data_type == "data")
  if (nrow(x) == 0) {
    dqs$datafiles <- NULL
    return(dqs)
  }

  x <- lapply(1:nrow(x), \(i) {
      new_datafile(x$file_path[i], x$file_ext[i], x$id[i])
    }) |>
    unlist()
  dqs$datafiles <- x

  return(dqs)
}
