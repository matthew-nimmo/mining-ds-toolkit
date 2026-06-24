detect_units <- function(x) {
  stopifnot(is.data.frame(x))

  units <- c("cm","cm2","cm3","deg","g","g/cm3","gcm3","gqm","g/t",
             "gt","gpt","g/qm","ft","ft/s","Hz","kg","kg/m2","kg/m3",
             "kgm3","km","km2","km3","mg/kg","mm","m/s","MWt",
             "Nm","pct","%","ppm","ppb")

  p <- apply(x, 1, \(y) {
    sum(y %in% units) / length(y)
  })
  i <- which.max(p)
  if (p[i] < 0.8) {
    return(NULL)
  }

  z <- as.character(x[i, ])
  z <- ifelse(z %in% units, z, NA)
  attr(z, "prob") <- p[i]
  attr(z, "rowid") <- as.numeric(i)

  return(z)
}