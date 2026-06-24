csv <- setClass(
  "csv",
  contains = "datafile")

setMethod(
  "show",
  "csv",
  function(object) {
    cat("Id:", object@id, "\n")
    cat("File Path:", dirname(object@filename), "\n")
    cat("File Name:", basename(object@filename), "\n")
    cat("Type: CSV\n")
  })

setMethod(
  "as.data.frame",
  "csv",
  function(x) {
    data.frame(
      id = x@id,
      type = "csv",
      file_path = dirname(x@filename),
      file_name = basename(x@filename),
      sheet_num = NA,
      sheet_name = NA)
  })

load_datafile.csv <- function(object) {
  df <- readr::read_lines(object@filename, n_max=30)
  df <- df[!grepl("^$", df)]
  i <- grepl(",", df)
  skip_start <- which(i)[1] - 1
  if (length(skip_start) == 0) {
    return(NULL)
  }

  df <- read.csv(
    object@filename,
    header = FALSE,
    skip = skip_start,
    colClasses = "character")
  row.names(df) <- skip_start + 1:nrow(df)

  return(df)
}
