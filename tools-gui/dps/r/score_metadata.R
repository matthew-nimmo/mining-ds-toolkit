score_metadata <- function(x, level="doc") {
  if (!is(x, "score")) {
    return(NULL)
  }

  if (level == "doc") {
    # Structured - may have metadata in header.
    ext <- c("csv", "duckdb", "feather", "parquet", "sqlite",
             "tab", "tsv", "txt", "xls", "xlsx")
    z1 <- ifelse(x$dataset$file_ext %in% ext, 1, 0)

    # Docs - likely to have metadata if n is low.
    # Only if there are data files.
    ext <- c("docx", "md", "pdf", "rtx", "txt")
    if (any(z1 == 1)) {
      z2 <- ifelse(x$dataset$file_ext %in% ext, 1, 0)
      prev <- sum(z2) / length(z2)
      conf <- 1 - prev
      z2 <- z2 * (1 - conf * prev)
    } else {
      z2 <- rep(0, length(z1))
    }

    z <- pmax(z1, z2)
  } else {
    z <- sapply(x$metadata, \(m) {
      if (is.null(m)) {
        return(0)
      }
      if (is.null(m$colnames)) {
        return(0)
      }

      pb1 <- ifelse(!is.null(m$table), 0, 1)
      if (is.null(m$unit)) {
        prev <- m$colnames |>
          sapply(has_unit) |>
          x => (sum(!x) / length(x))
        conf <- 1 - prev
        pb2 <- 1 - conf*prev
      } else {
        pb2 <- 1 - attr(m$unit, "prob")
      }
      pb3 <- ifelse(!is.null(m$method), 1 - attr(m$method, "prob"), 1)
      pb4 <- ifelse(!is.null(m$limit), 1 - attr(m$limit, "prob"), 1)
      pb5 <- tolower(m$colnames) |>
        x => grepl("discr", x) |>
        x => ifelse(any(x), 0, 1)
      pb6 <- tolower(m$colnames) |>
        x => grepl("date", x) |>
        x => ifelse(any(x), 0, 1)
      pb7 <- tolower(m$colnames) |>
        x => grepl("time", x) |>
        x => ifelse(any(x), 0, 1)
      pb8 <- tolower(m$colnames) |>
        x => grepl("(^id)|(id$)", x) |>
        x => ifelse(any(x), 1, 0)
      prev <- sum(c(pb1,pb2,pb3,pb4,pb5,pb6,pb7,pb8)) / 8
      conf <- 1 - prev
      z <- 1 - prev*conf

      return(z)
    })
  }

  x$score[["metadata"]] <- z
  
  return(x)
}
