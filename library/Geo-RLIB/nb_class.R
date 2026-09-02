#' nb_class
#'
#' Classify text using naive bayes.
#'
#' @param x character vector with text to classify.
#' @param lut.txt character, list of tokens (words).
#' @param lut.class character, list of labels to apply for the given token.
#'
#' @export
nb_class <- function(x, lut.txt, lut.class) {
  dtm <- table(data.frame(text=tolower(lut.txt), label=lut.class, stringsAsFactors=FALSE))
  dtm <- dtm / rowSums(dtm)
  dtm[dtm == 0] <- 0.00001

  n <- row.names(dtm)
  y <- strsplit(tolower(x), " ")
  z <- vapply(y, function(v) {
    i <- n %in% v
    if (any(i)) {
      probs <- dtm[i,]
      if(!is.null(nrow(probs))) {
        probs <- apply(dtm[i,], 2, prod)
      }
      return(names(which.max(probs)))
    } else {
      return(as.character(NA))
    }
  }, FUN.VALUE="")

  return(z)
}
