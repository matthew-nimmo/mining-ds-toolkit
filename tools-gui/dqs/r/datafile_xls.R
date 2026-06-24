xls <- setClass(
  "xls",
  slots = list(sheet_num = "numeric",
               sheet_name = "character"),
  contains = "datafile")

setMethod("show",
          "xls",
          function(object) {
            cat("Id:", object@id, "\n")
            cat("File Path:", dirname(object@filename), "\n")
            cat("File Name:", basename(object@filename), "\n")
            cat("Type: XLS\n")
            cat("Sheet Number:", object@sheet_num, "\n")
            cat("Sheet Name:", object@sheet_name, "\n")
          })

setMethod("as.data.frame",
          "xls",
          function(x) {
            data.frame(
              id = x@id,
              type = "xls",
              file_path = dirname(x@filename),
              file_name = basename(x@filename),
              sheet_num = x@sheet_num,
              sheet_name = x@sheet_name)
          })

load_datafile.xls <- function(object) {
  df <- readxl::read_excel(
    object@filename,
    sheet = object@sheet_num,
    col_types = "text",
    col_names = FALSE,
    .name_repair = "minimal",
    n_max = 20)

  if (nrow(df) == 0) {
    return(NULL)
  }

  row.names(df) <- 1:nrow(df)

  return(df)
}
