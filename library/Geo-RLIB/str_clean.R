#' Helper function to clean up text.
#'
#' @param x character vector to clean.
#' @param lut.trans look-up table to convert from SPANISH to ENGLISH. Must contain the columns SPANISH and ENGLISH. Defaults to lut_trans.
#' @param lut.abbr look-up table to convert abbreviations to long form. Must contain the columns FROM and TO. Defaults to lut_abbr.
#' @param lut.spell look-up table to correct spelling errors. Must contain the columns FROM and TO. Defaults to lut_spell.
#' @param lut.stopwords character list of stop words to remove. Defaults to lut_stopwords.
#' @param stem logical apply stemming. Defaults to FALSE.
#'
#' @return cleaned \code{x}.
#'
#' @keywords str_clean
#' @export
str_clean <- function(x, lut.trans=lut_trans, lut.abbr=lut_abbr, lut.spell=lut_spell, lut.stopwords=lut_stopwords, stem=FALSE) {
  if(all(is.na(x))) {
    return(x)
  }

  x <- toupper(x)
  x[x == ""] <- "none"

  x <- gsub("^[[:punct:]]+", " ", x)
  x <- gsub("[[:punct:]]+$", " ", x)
  x <- gsub("[[:space:]]*[-][[:space:]]*", " ", x)
  x <- gsub("\\b([[:alpha:]])[/]([[:alpha:]])[/]?([[:alpha:]])?\\b", "\\1\\2\\3", x, perl=TRUE)
  x <- gsub("\\b[[:punct:]]+\\b", " ", x)
  x <- gsub("[[:space:]]?[[:punct:]][[:space:]]?", " ", x)

  x <- gsub("(?<=\\bPOS)[[:space:]](?=[[:digit:]])", "_", x, perl=TRUE)
  x <- gsub("(?<=\\bPSTN)[[:space:]](?=[[:digit:]])", "_", x, perl=TRUE)

  x <- gsub("\\b[[:digit:]]+[[:alpha:]]*[[:alnum:]]*\\b", " ", x)
  x <- gsub("\\b[[:alpha:]]+[[:digit:]]+[[:alnum:]]*\\b", " ", x)
  x <- gsub("\\b[[:digit:]]+\\b", " ", x)
  #x <- gsub("\\b[[:alpha:]]{1,2}\\b", " ", x)

  x <- gsub("[[:space:]]+", " ", x)
  x <- gsub("^[[:space:]]+|[[:space:]]+$", "", x)

  x[x == ""] <- "none"

  x <- strsplit(x, " ", fixed=TRUE)

  n <- vapply(x, FUN=length, FUN.VALUE=0)
  n[n == 0] <- 1
  indx <- inverse.rle(list(lengths=n, values=1:length(n)))

  y <- unlist(x, FALSE, FALSE)
  y <- factor(y)

  # Convert SPANISH
  if (!is.null(lut.trans)) {
    from <- toupper(lut.trans$SPANISH)
    to <- toupper(lut.trans$ENGLISH)
    i <- levels(y) %in% from
    if (any(i)) {
      z <- levels(y)
      lut <- to
      names(lut) <- from
      z[i] <- lut[levels(y)[i]]
      levels(y) <- z
    }
  }

  # Replace ABREVIATIONS.
  if (!is.null(lut.abbr)) {
    from <- toupper(lut.abbr$FROM)
    to <- toupper(lut.abbr$TO)
    i <- levels(y) %in% from
    if (any(i)) {
      z <- levels(y)
      lut <- to
      names(lut) <- from
      z[i] <- lut[levels(y)[i]]
      levels(y) <- z
    }
  }

  # Replace SPELLING.
  # Add alternative process for correcting spelling.
  if (!is.null(lut.spell)) {
    from <- toupper(lut_spell$FROM)
    to <- toupper(lut_spell$TO)
    i <- levels(y) %in% from
    if (any(i)) {
      z <- levels(y)
      lut <- to
      names(lut) <- from
      z[i] <- lut[levels(y)[i]]
      levels(y) <- z
    }
  }

  # Replace STOP-WORDS.
  if (!is.null(lut.stopwords)) {
    i <- levels(y) %in% toupper(lut.stopwords)
    if (any(i)) {
      z <- levels(y)
      z[i] <- ""
      levels(y) <- z
    }
  }

  # Stem words.
  if (stem) {
    z <- tm::stemDocument(levels(y))
    i <- z != levels(y)
    if (any(i)) {
      zz <- tm::stemCompletion(z[i], dictionary=levels(y), type="shortest")
      ii <- !is.na(zz)
      zz[ii] <- names(zz)[ii]
      z[i] <- zz
      levels(y) <- z
    }
  }

  x <- data.frame(indx=indx, txt=levels(y)[unclass(y)], stringsAsFactors=FALSE) %>%
    dplyr::group_by(indx) %>%
    dplyr::summarise(txt = paste(txt, collapse=" ")) %>%
    dplyr::ungroup()

  X <- x$txt
  x <- gsub("[[:space:]]+", " ", x)
  x <- gsub("^[[:space:]]+|[[:space:]]+$", "", x)

  return(X)
}

#clean_corpus <- function(docs, words=NULL, pattern=NULL, replace=NULL) {
#  if (length(docs) == 0) {
#    return(NULL)
#  }
#
#  # Create corpus.
#  docs <- tm::Corpus(tm::VectorSource(docs))
#
#  # Clean corpus.
#  docs <- tm::tm_map(docs, tm::content_transformer(function(x) gsub("/|-", " ", x)))
#  docs <- tm::tm_map(docs, tm::removeNumbers)
#  docs <- tm::tm_map(docs, tm::removePunctuation)
#  docs <- tm::tm_map(docs, tm::stripWhitespace)
#  docs <- tm::tm_map(docs, tm::content_transformer(tolower))
#  docs <- tm::tm_map(docs, tm::removeWords, tm::stopwords("english"))
#  docs <- tm::tm_map(docs, tm::content_transformer(function(x) gsub("^\\s+|\\s+$", "", x)))
#
#  # Remove stop words
#  if (!is.null(words)) {
#    docs <- tm::tm_map(docs, tm::removeWords, words)
#  }
#
#  # Replace words
#  if (!is.null(pattern)) {
#    (f <- tm::content_transformer(function(x) stringi::stri_replace_all_regex(x, pattern, replace, vectorize_all=FALSE)))
#    docs <- tm::tm_map(docs, f)
#  }
#
#  dict <- docs
#  docs <- tm::tm_map(docs, tm::stemDocument, language="english")
#  docs <- tm::tm_map(docs, tm::stemCompletion, dictionary=dict)
#
#  return(docs)
#}
