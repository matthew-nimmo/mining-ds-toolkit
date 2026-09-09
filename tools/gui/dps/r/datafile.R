datafile <- setClass(
  "datafile",
  slots = list(id = "numeric",
               filename = "character"))

setMethod("show",
          "datafile",
          function(object) {
            cat("Id:", object@id, "\n")
            cat("File Path:", dirname(object@filename), "\n")
            cat("File Name:", basename(object@filename), "\n")
          })

setMethod("as.data.frame",
          "datafile",
          function(x) {
            data.frame(
              id = x@id,
              file_type = "data",
              file_path = dirname(x@filename),
              file_name = basename(x@filename))
          })

new_datafile <- function(x, y, i) {
  if (y == "csv") {
    f <- csv(id = i,
             filename = x)
  } else if (y %in% c("xls","xlsx")) {
    n <- readxl::excel_sheets(x)
    f <- lapply(1:length(n), \(k) {
      xls(id = i,
          filename = x,
          sheet_num = k,
          sheet_name = n[[k]])
    })
  } else {
    f <- unknwn(id = i,
                filename = x)
  }

  return(f)
}

load_datafile <- function(object)
  UseMethod("load_datafile")

load_datafile.default <- function(object) {
  return(NULL)
}
