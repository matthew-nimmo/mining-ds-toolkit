score_accessibility <- function(x, level="doc") {
  if (!is(x, "score")) {
    return(NULL)
  }

  if (level == "doc") {
    # Can easily open in R and get data.
    ext <- c("csv", "duckdb", "feather", "json", "kml",
             "md", "parquet", "rds", "rtx", "shp", "sqlite",
             "tab", "tif", "tiff", "tsv", "txt", "xls", "xlsx", "xml")
    z <- ifelse(x$dataset$file_ext %in% ext, 1, 0)
  } else {
    z <- sapply(x$profile, \(p) {
      if (is.null(p)) {
        return(0)
      } else {
        return(1)
      }
    })
  }

  x$score[["accessibility"]] <- z

  return(x)
}