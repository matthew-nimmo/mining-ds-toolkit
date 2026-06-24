score_usability <- function(x) {
  if (!is(x, "score")) {
    return(NULL)
  }

  z <- sapply(x$metadata, \(m) {
    if (is.null(m)) {
      return(0)
    }
    if (is.null(m$colnames)) {
      return(0)
    }

    n <- length(m$colnames)

    # Blank column name.
    prev <- tolower(m$colnames) |>
      v => ifelse(v %in% c("", " ", "na"), 1, 0) |>
      v => (sum(v) / n) |>
      v => ifelse(is.null(m$colnames), 0, v)
    conf <- 1 - prev
    z1 <- 1 - conf * prev

    # Duplicated column name.
    z2 <- tolower(m$colnames) |> 
      duplicated() |>
      v => (sum(v) / n) |>
      v => ifelse(is.null(m$colnames), 0, v)
    conf <- 1 - prev
    z2 <- 1 - conf * prev

    return(z1 * z2)
  })

  x$score[["usability"]] <- z

  return(x)
}