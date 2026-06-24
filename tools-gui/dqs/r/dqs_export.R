dqs_export <- function(dqs, opath) {
  if (!is(dqs, "dqs")) {
    return(dqs)
  }

  con <- dbConnect(SQLite(), paste0(opath, ".sqlite"))

  # Output list for export to Excel
  o <- list()

  # Data set
  df1 <- dqs_extract(dqs, "dataset")
  dbWriteTable(con, "dataset", df1, overwrite=TRUE)
  o[["dataset"]] <- df1

  # Data files data frame
  df2 <- dqs_extract(dqs, "datafiles")
  if (!is.null(df2)) {
    dbWriteTable(con, "datafiles",
                 df2[, setdiff(names(df2), c("file_path","file_name"))],
                 overwrite=TRUE)
    o[["datafiles"]] <- df2
  }

  # Data profile - table
  df3 <- dqs_extract(dqs, "profile_table")
  if (!is.null(df3)) {
    dbWriteTable(con, "profile_table", df3, overwrite=TRUE)
    df3 <- merge(df2[,c("id","file_path","file_name","sheet_num","sheet_name")],
                 df3,
                 by = c("id","sheet_num"))
    o[["profile_table"]] <- df3
  }

  # Data profile - fields
  df4a <- dqs_extract(dqs, "profile_fields")
  df4b <- dqs_extract(dqs, "datafields")
  if (!is.null(df4a)) {
    df4 <- df4a
    if (!is.null(df4b)) {
      df4 <- merge(df4, df4b,
                   by = c("id","sheet_num","field"), all=TRUE)
    }
    dbWriteTable(con, "profile_field", df4, overwrite=TRUE)
    df4 <- merge(df2[,c("id","file_path","file_name","sheet_num","sheet_name")],
                 df4,
                 by = c("id","sheet_num"))
    o[["profile_field"]] <- df4
  }

  dbDisconnect(con)
  write_xlsx(o, path=paste0(opath, ".xlsx"))

  return(dqs)
}
