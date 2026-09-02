#' Sun angle and azimuth calculation.
#'
#' Calculate the angle and azimuth of the sun at a given latitude, longitude, date and time.
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @param date Date to get sun angle for.
#' @param time Time of day o get sun angle for.
#'
#' @return numeric, the angle of the sun in degrees.
#'
#' @examples
#' lat <- -20.31215
#' lon <- 118.61059
#' x <- seq(0, 86400, 1440)
#' y <- sapply(x, function(t) sun_angle(lat, lon, Sys.Date(), t)$altitude)
#' plot(x, y)
#'
#' @references Modified from \url{https://gist.github.com/JasonDalton/3304916}
#'
#' @keywords sun_angle
#' @export
sun_angle <- function(lat, lon, date, time) {
  if (class(date) == "character") {
    date <- as.Date(date)
  }
  date <- julian(date)[1]
  time <- time / 3600

  t <- 2 * pi * ((date-1) / 365)

  dec <- 0.322003 - 22.971*cos(t) - 0.357898*cos(2*t) - 0.14398*cos(3*t) +
    3.94638*sin(t) + 0.019334*sin(2*t) + 0.05928*sin(3*t)

  t <- (279.134 + 0.985647 * date) * (pi / 180)
  fEq <- 5.0323 - 100.976*sin(t) + 595.275*sin(2*t) + 3.6858*sin(3*t) - 12.47*sin(4*t) -
    430.847*cos(t) + 12.524*cos(2*t) + 18.25*cos(3*t)

  angle <- (15 * (time - 12) * (pi / 180))

  t <- sin(dec) * sin(lat)
  u <- cos(dec) * cos(lat) * cos(angle)
  altitude <- asin(t+u)

  #Solar Azimuth
  t <- cos(lat) * sin(dec)
  u <- cos(dec) * sin(lat) * cos(angle)
  ts <- -cos(dec) * sin(angle)
  azimuth <- acos((t-u) / cos(altitude))

  azimuth <- azimuth * 180 / pi
  altitude <- altitude * 180 / pi

  return(list(azimuth=azimuth, altitude=altitude))
}
