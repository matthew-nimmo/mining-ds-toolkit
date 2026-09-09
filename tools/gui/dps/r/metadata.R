metadata <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  stopifnot(inherits(x, "datafile"))

  df <- load_datafile(x)
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  i <- row.names(df)[1] |>
    as.numeric()
  if (i > 1 & is(x, "csv")) {
    mt <- readr::read_lines(x@filename, n_max=i-1)
  } else {
    mt <- NULL
  }

  i <- min(3, ncol(df)) - 1
  if (i > 0) {
    mr <- detect_rownames(df[, 1:i, drop=FALSE])
  } else {
    mr <- NULL
  }
  mc <- detect_colnames(df[1:20, ])
  mm <- detect_method(df[1:20, ])
  mu <- detect_units(df[1:20, ])
  ml <- detect_limit(df[1:20, ])

  hb <- sapply(list(mc,mm,mu,ml), \(x) attr(x, "rowid")) |>
    unlist() |>
    sort()
  #stopifnot(all(diff(sort(header_rows)) == 1))

  m <- list(
    table = mt,
    rownames = mr,
    colnames = mc,
    method = mm,
    unit = mu,
    limit = ml,
    header_rows = hb
  )

  return(m)
}