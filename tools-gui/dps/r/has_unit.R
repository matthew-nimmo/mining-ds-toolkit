has_unit <- function(x) {
  units1 <- c("cm","cm2","cm3","deg","g/cm3","g/t","g/qm","ft","ft/s",
              "Hz","kg","kg/m2","kg/m3","km","km2","km3","mg/kg",
              "mm","m/s","%","ppm","ppb","MWt")
  u1 <- tolower(units1) |>
    x => sub("/", "[/._p]", x) |>
    x => sub("%", "%|pct", x)
  units2 <- c("in","g","m","m2","m3","s")
  u2 <- tolower(units2)

  ptn <- c(
    paste0("^[[:space:][:punct:]]?", u1, "|", u1, "[[:space:][:punct:]]?$"),
    paste0("[[:space:][:punct:]]", u2, "[[:space:][:punct:]]?$"))

  y <- x
  if (Encoding(y) == "UTF-8") {
    y <- iconv(y, "ASCII", sub="")
    y <- sub("B([[:digit:]])", "\\1", y)
  }
  y <- tolower(y)
  y <- sapply(ptn, grepl, y)
  names(y) <- c(units1, units2)

  if (any(y)) {
    y <- y[y]
  } else {
    y <- c(no = FALSE)
  }
  y <- y[which.max(nchar(names(y)))]

  return(y)
}