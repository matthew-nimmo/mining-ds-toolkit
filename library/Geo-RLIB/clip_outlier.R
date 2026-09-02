clip_outlier <- function(x, range=1.5, clip=FALSE) {
  qnt <- quantile(x, probs=c(0.25, 0.75), na.rm=TRUE)
  caps <- quantile(x, probs=c(0.05, 0.95), na.rm=TRUE)
  caps <- ifelse(clip, caps, c(NA,NA))
  H <- range * IQR(x, na.rm=TRUE)

  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]

  return(x)
}
