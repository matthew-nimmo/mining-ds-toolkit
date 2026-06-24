value_format <- function(x) {
  if (is.null(x)) {
    return(NA)
  }

  v <- x
  v[is.na(x)] <- ""
  v <- iconv(v, sub="s")

  v <- gsub("^[[:space:]]+$", "", v)
  v <- gsub("^[-]*$", "", v)
  v <- gsub("(?![Ee]-?)([[:alpha:]])", "a", v, perl=TRUE)
  v <- gsub("a[Ee]-?a*", "aa-a", v, perl=TRUE)
  v <- gsub("E-", "e-", v, perl=TRUE)
  v <- gsub("[[:digit:]]", "d", v, perl=TRUE)
  v <- gsub("[[:space:]]*([:-])[[:space:]]*", "\\1", v)

  v <- sapply(v, \(x) {
    if (x == "") {
      return(x)
    }
    x <- rle(unlist(strsplit(x, "")))
    x <- paste0(x$values,
                ifelse(x$lengths > 1, paste0("{", x$lengths, "}"), ""),
                collapse="")
    x
  }, USE.NAMES = FALSE)

  return(v)
}