dataset <- function(path, include_ext=NULL, exclude_ext=NULL, list_all=TRUE) {
  ptn <- "."
  if (!list_all & !is.null(include_ext)) {
    ptn <- paste(include_ext, collapse="|")
    ptn <- paste0(".(", ptn, ")")
  }

  x_filename <- list.files(
    path = path,
    pattern = ptn,
    full.names = TRUE,
    all.files = TRUE,
    recursive = TRUE,
    include.dirs = FALSE)

  N <- length(x_filename)
  if (N == 0) {
    return(NULL)
  }

  x_basename = basename(x_filename)
  x_ext <- strsplit(x_basename, ".", fixed=TRUE) |>
    sapply(tail, 1) |>
    tolower()

  x_incl <- rep(TRUE, length(x_filename))
  if (!is.null(include_ext)) {
    x_incl <- x_incl & (x_ext %in% include_ext)
  }
  if (!is.null(exclude_ext)) {
    x_incl <- x_incl & !(x_ext %in% exclude_ext)
  }

  x_name <- sub("[.].+$", "", x_basename)
  x_dup <- duplicated(x_basename) | duplicated(x_basename, fromLast=TRUE)

  x_type <- rep("other", N)
  ext <- c("accdb", "csv", "dat", "dm", "duckdb", "feather",
           "json", "mdb", "parquet", "sqlite", "tab", "tsv",
           "xls", "xlsx")
  x_type <- ifelse(x_ext %in% ext, "data", x_type)
  ext <- c("docx", "md", "qmd", "rmd", "pdf", "ppt", "pptx", "rtf", "txt")
  x_type <- ifelse(x_ext %in% ext, "doc", x_type)
  ext <- c("ai", "bmp", "dwg", "eps", "gif", "jpeg", "jpg",
           "png", "psd", "raw", "svg", "tif")
  x_type <- ifelse(x_ext %in% ext, "image", x_type)
  ext <- c("aux", "bil", "bip", "bsq", "cpg", "dbf", "dem", "dlg",
           "ers", "ecw", "gdb", "geojson", "gml", "gpkg", "gpx",
           "id", "img", "ind", "j2i", "j2w", "jp2", "kml", "kmz",
           "map", "mshp", "mxd", "ovr", "pix", "prj", "qgs", "qpj", "rdc",
           "rrd", "rst", "sbn", "sbx", "sdw", "shp", "shx", "sl3",
           "sid", "tfw", "tif", "tiff", "vct", "vdc", "vmds")
  x_type <- ifelse(x_ext %in% ext, "gis", x_type)

  o <- order(x_name)
  y <- x_name[o] |>
    z => ifelse(z == dplyr::lag(z, default=""), 0, 1) |>
    cumsum()
  o <- order(o)
  x_set <- y[o]

  meta = data.frame(
    id = 1:N,
    file_path = x_filename,
    file_name = x_name,
    file_ext = x_ext,
    file_size = file.size(x_filename),
    data_type = x_type,
    file_set = x_set,
    duplicated = x_dup,
    include = x_incl)

  return(meta)
}
