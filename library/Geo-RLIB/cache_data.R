cache_data <- function(fname, func, out.csv=TRUE, out.feather=FALSE, out.rds=TRUE) {
  fn <- gsub("[.].{1,4}$", "", fname)
  fname_rds <- paste0(fn, ".rds")
  fname_csv <- paste0(fn, ".csv")
  fname_feather <- paste0(fn, ".feather")

  if (!file.exists(fname)) {
    df <- func()

    if (!is.data.frame(df)) {
      out.csv <- FALSE
      out.dat <- FALSE
      out.feather <- FALSE
      out.rds <- TRUE
    }

    if (out.csv) {
      write.csv(df, file=fname_csv, na="", row.names=FALSE)
    }

    if (out.feather) {
      write_feather(df, fname_feather)
    }

    if (out.rds) {
      saveRDS(df, fname_rds)
    }
  } else {
    if (file.exists(fname_rds)) {
      df <- readRDS(fname)
    } else {
      df <- read.csv(fname, na.strings="", stringsAsFactors=FALSE)
    }
  }

  return(df)
}
