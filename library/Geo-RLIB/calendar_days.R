#' Function to get number of calendar days in a month.
#'
#' @param date The date string in %Y-%m format.
#'
#' @return number of days.
#'
#' @keywords calendar_days
#' @export
calendar_days <- function(date) {
  last_days <- 28:31
  ndays <- rev(last_days[which(!is.na(as.Date(paste(substr(date,1,8), last_days, sep=""), "%Y-%m-%d")))])[1]

  return(ndays)
}
