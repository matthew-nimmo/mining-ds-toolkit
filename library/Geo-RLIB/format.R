#' Format data frame columns.
#'
#' @param x column to format.
#' @param formats the type of format.
#'
#' @return formated vector.
#'
#' @keywords format_cols
#' @export
format_cols <- function(x, formats) {
  formats <- formats[formats != "character"]
  n <- length(formats)

  qual_pass <- rep(0, nrow(x))

  if (n == 0) {
    x$qual_pass <- qual_pass
    return(x)
  }

  cols <- names(formats)
  func <- paste("format", formats, sep="_")

  for (i in 1:n) {
    if (exists(func)) {
      y <- do.call(func[i], list(x[[cols[i]]]))
      qa <- attr(y, "qual_pass")
      attr(y, "qual_pass") <- NULL
      qual_pass <- qual_pass + qa
      x[[cols[i]]] <- y
    }
  }
  x$qual_pass <- qual_pass / n

  return(x)
}

#' Format as datetime.
#'
#' @param x column to format.
#'
#' @return formated vector.
#'
#' @keywords format_datetime
#' @export
format_datetime <- function(x) {
  x[x == ""] <- NA
  has.data <- !is.na(x)

  y <- rep(as.POSIXct(NA), length(x))

  if (sum(has.data) == 0) {
    qual_pass <- rep(0, length(y))
    attr(y, "qual_pass") <- qual_pass
    return(y)
  }

  z <- x[has.data]

  if (sum(grepl("[-/:]", z)) == 0) {
    # Is in number format (Microsoft Excel)
    z <- as.POSIXct(readr::parse_number(z) * (60*60*24), origin="1899-12-30", tz="UTC")
  } else {
    z <- gsub("[./]", "-", z)
    z <- gsub("([[:digit:]]{2})-([[:digit:]]{2})-([[:digit:]]{4})", "\\3-\\2-\\1", z)
    z <- gsub("([[:digit:]])-([[:digit:]]{2})-([[:digit:]]{4})", "\\3-\\2-0\\1", z)
    z <- gsub("([[:digit:]]{2})-([[:digit:]])-([[:digit:]]{4})", "\\3-0\\2-\\1", z)
    z <- gsub("([[:digit:]])-([[:digit:]])-([[:digit:]]{4})", "\\3-0\\2-0\\1", z)

    indx <- grepl(" (AM|PM)$", z)
    if (indx) {
      # Probably should only do this for those records with AM or PM.
      z <- readr::parse_datetime(z, "%F %T %p")
    } else {
      z <- readr::parse_datetime(z)
    }
  }

  y[has.data] <- z

  # Add format quality attribute.
  qual_pass <- rep(1, length(y))
  qual_pass[has.data & is.na(y)] <- 0
  attr(y, "qual_pass") <- qual_pass

  return(y)
}

#' Format as Date.
#'
#' @param x column to format.
#'
#' @return formated vector.
#'
#' @keywords format_date
#' @export
format_date <- function(x) {
  x[x == ""] <- NA
  x[x == "0"] <- NA
  has.data <- !is.na(x)

  y <- rep(as.Date(NA), length(x))

  if (sum(has.data) == 0) {
    qual_pass <- rep(0, length(y))
    attr(y, "qual_pass") <- qual_pass
    return(y)
  }

  # Check if there are no separators.
  # Date either %Y%m%d or in Microsoft Excel format.
  indx <- has.data & !grepl("[-/.]", x)
  if (any(indx)) {
    indx2 <- stringr::str_length(x) == 8 & grepl("^19|20", x)
    if (any(indx & indx2)) {
      y[indx & indx2] <- readr::parse_date(x[indx & indx2], "%Y%m%d")
    }
    if (any(indx & !indx2)) {
      y[indx & !indx2] <- as.Date(readr::parse_number(x[indx & !indx2]), origin="1899-12-30")
    }
  }

  indx <- has.data & grepl("[[:digit:]]+[-][[:alpha:]]+[-][[:digit:]]+", x)
  if (any(indx)) {
    y[indx] <- readr::parse_date(x[indx], "%d-%b-%y")
  }

  indx <- has.data & !indx
  if (any(indx)) {
    z <- x[indx]
    z <- gsub("[./]", "-", z)
    z <- gsub("([[:digit:]]{2})-([[:digit:]]{2})-([[:digit:]]{4})", "\\3-\\2-\\1", z)
    z <- gsub("([[:digit:]])-([[:digit:]]{2})-([[:digit:]]{4})", "\\3-\\2-0\\1", z)
    z <- gsub("([[:digit:]]{2})-([[:digit:]])-([[:digit:]]{4})", "\\3-0\\2-\\1", z)
    z <- gsub("([[:digit:]])-([[:digit:]])-([[:digit:]]{4})", "\\3-0\\2-0\\1", z)
    z <- gsub("([[:space:]]|T)[[:digit:]]+:.+$", "", z)
    y[indx] <- readr::parse_date(z)
  }

  # Add format quality attribute.
  qual_pass <- rep(1, length(y))
  qual_pass[has.data & is.na(y)] <- 0
  attr(y, "qual_pass") <- qual_pass

  return(y)
}

#' Format as Time.
#'
#' @param x column to format.
#'
#' @return formated vector.
#'
#' @keywords format_time
#' @export
format_time <- function(x) {
  x[x == ""] <- NA
  x[x == "0"] <- NA
  has.data <- !is.na(x)

  y <- hms::hms(rep(NA, length(x)))

  if (sum(has.data) == 0) {
    qual_pass <- rep(0, length(y))
    attr(y, "qual_pass") <- qual_pass
    return(y)
  }

  z <- x[has.data]

  # Check if number format.
  if (sum(grepl(":", z)) == 0) {
    if (any(stringr::str_length(z) > 6)) {
      z <- readr::parse_number(z)
      max.x <- max(z, na.rm=TRUE)
      has.decimal <- any((z %% 1) > 0)
      if (!has.decimal) {
        z <- hms::hms(seconds=z)
      } else {
        if (max.x > 1) {
          z <- hms::hms(hours=z)
        } else {
          z <- hms::hms(days=z)
        }
      }
    } else {
      z <- stringr::str_pad(z, 6, pad="0")
      z <- readr::parse_time(z, "%H%M%S")
    }
  } else {
    z <- gsub("[[:space:]]*:[[:space:]]*:[[:space:]]*", "00:00:00", z)
    z <- gsub("24:00:00", "23:59:59", z)
    z <- gsub(".+([[:space:]]|T)", "", z)
    z <- gsub("z$", "", z)
    z <- readr::parse_time(z)
  }

  y[has.data] <- z

  # Add format quality attribute.
  qual_pass <- rep(1, length(y))
  qual_pass[has.data & is.na(y)] <- 0
  attr(y, "qual_pass") <- qual_pass

  return(y)
}

#' Format as number
#'
#' @param x column to format.
#'
#' @return formated vector.
#'
#' @keywords format_number
#' @export
format_number <- function(x) {
  x[x == ""] <- NA
  x[x == "#"] <- NA
  has.data <- !is.na(x)

  y <- rep(as.numeric(NA), length(x))

  if (sum(has.data) == 0) {
    qual_pass <- rep(0, length(y))
    attr(y, "qual_pass") <- qual_pass
    return(y)
  }

  z <- x[has.data]

  if (any(grepl("^[[:digit:]]+[.][[:digit:]]+[,]", z))) {
    z <- gsub("[.]", "", z)
    z <- gsub("[,]", ".", z)
    z <- readr::parse_number(z)
  } else {
    z <- readr::parse_number(z)
  }

  y[has.data] <- z

  # Add format quality attribute.
  qual_pass <- rep(1, length(y))
  qual_pass[has.data & is.na(y)] <- 0
  attr(y, "qual_pass") <- qual_pass

  return(y)
}
