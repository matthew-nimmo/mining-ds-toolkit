detect_method <- function(x) {
  stopifnot(is.data.frame(x))

  methods <- c("WEI-23g","WEI-22g","OA-GRA05g","ME-XRF26s","ME-XRF26s",
               "ME-XRF26s","ME-XRF26s","ME-XRF26s","ME-XRF26s","ME-XRF26s",
               "ME-XRF26s","ME-XRF26s","ME-XRF26s","ME-XRF26s","ME-XRF26s",
               "ME-XRF26s","ME-XRF26s","ME-XRF26s","ME-XRF26s","ME-XRF26s",
               "ME-XRF26s","ME-XRF26s","ME-GRA05","ME-MS81","ME-MS81",
               "ME-MS81","ME-MS81","ME-MS81","ME-MS81","ME-MS81","ME-MS81",
               "ME-MS81","ME-MS81","ME-MS81","ME-MS81","ME-MS81","ME-MS81",
               "ME-MS81","ME-MS81","ME-MS81","ME-MS81","ME-MS81","ME-MS81",
               "ME-MS81","ME-MS81","ME-MS81","ME-MS81","ME-MS81","ME-MS81",
               "ME-MS81","ME-MS81","ME-MS81","ME-MS81","ME-MS81","ME-ICP61",
               "ME-ICP61","ME-ICP61","ME-ICP61","ME-ICP61","ME-MS62","ME-MS62",
               "ME-MS62","ME-MS62","ME-MS62","SCR-61","SCR-61","SCR-61",
               "SCR-61","PUL-QC")

  p <- apply(x, 1, \(y) {
    sum(y %in% methods) / length(y)
  })
  i <- which.max(p)
  if (p[i] < 0.8) {
    return(NULL)
  }

  z <- as.character(x[i, ])
  z <- ifelse(z %in% methods, z, NA)
  attr(z, "prob") <- p[i]
  attr(z, "rowid") <- as.numeric(i)

  return(z)
}