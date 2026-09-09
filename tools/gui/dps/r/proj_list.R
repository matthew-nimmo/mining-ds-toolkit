proj_list <- function(base_dir) {
  proj_dir <- list.files(
    base_dir,
    include.dirs = TRUE,
    all.files = FALSE,
    recursive = FALSE)
  i <- grepl("^[[:digit:]]{7}P?[^.]+$", proj_dir)
  proj_dir <- proj_dir[i]

  i <- sapply(proj_dir, \(f) {
    data_dir <- paste0(base_dir, "/", f, "/3 _Client Data")
    if (!file.exists(data_dir)) {
      return(FALSE)
    }
    N <- list.files(
      path = data_dir,
      recursive = TRUE,
      include.dirs = FALSE) |>
      length()
    return(ifelse(N==0, FALSE, TRUE))
  }, USE.NAMES = FALSE)
  proj_dir <- proj_dir[i]

  proj_id <- sub("[[:space:]][^.]+$", "", proj_dir)
  proj_name <- sub("^[[:digit:]]{7}P?[[:space:]]", "", proj_dir)
  proj_dir <- paste0(base_dir, "/", proj_dir, "/3 _Client Data")

  x <- data.frame(
    proj_id = proj_id,
    proj_name = proj_name,
    proj_dir = proj_dir
  )

  return(x)
}