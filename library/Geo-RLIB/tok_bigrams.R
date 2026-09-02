#' Combine bigram tokens.
#' @param docs character vector.
#' @param size size of collocations. Defaults to 2.
#'
#' @return a quanteda \code{tokens} object.
#'
#' @keywords tok_bigrams
#' @export
tok_bigrams <- function(docs, size=2) {
  if (size < 2) {
    stop("Size needs to be at least 2!")
  }

  if (is.null(docs) | length(docs) == 0) {
    return(NULL)
  }

  docs <- quanteda::corpus(docs[!is.na(docs)])

  # Split into words.
  tok <- quanteda::tokens(docs)
  #tok2 <- tokens_ngrams(tok, n=2, skip=0, concatenator="_")

  for (i in size:2) {
    # Get bigrams
    cls <- quanteda::textstat_collocations(tok, size=i, min_count=10)
    if (nrow(cls) > 0) {
      tok <- quanteda::tokens_compound(tok, cls, join=TRUE)
    }
  }

  return(tok)
}
