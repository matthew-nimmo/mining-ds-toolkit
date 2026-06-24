unknwn <- setClass(
  "unknwn",
  contains = "datafile")

setMethod("show",
          "unknwn",
          function(object) {
            cat("Id:", object@id, "\n")
            cat("File Path:", dirname(object@filename), "\n")
            cat("File Name:", basename(object@filename), "\n")
            cat("Type: Unknown\n")
          })

setMethod("as.data.frame",
          "unknwn",
          function(x) {
            data.frame(
              id = x@id,
              type = "unknwn",
              file_path = dirname(x@filename),
              file_name = basename(x@filename),
              sheet_num = NA,
              sheet_name = NA)
          })
