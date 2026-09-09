#' Fill missing values in column using another column.
#' 
#' @param to vector to fill missing values.
#' @param from vector from which missing values will be drawn.
#' 
#' @return vector with missing values replaced.
#' 
#' @keywords fill_missing
#' @export
fill_missing <- function(to, from) {
  indx <- is.na(to)
  to[indx] <- from[indx]

  return(to)
}
