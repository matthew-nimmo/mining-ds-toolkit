# Sum squared error.
error.SSE <- function(x, y) {
  return(sum((y - x)^2, na.rm=TRUE))
}

# Sum absolute error.
error.SAE <- function(x, y) {
  return(sum(abs(y - x), na.rm=TRUE))
}

# Mean squared error.
error.MSE <- function(x, y) {
  return(mean((y - x)^2, na.rm=TRUE))
}

# Mean absolute error.
error.MAE <- function(x, y) {
  return(mean(abs(y - x), na.rm=TRUE))
}

# Relative squared error.
error.RSE <- function(x, y) {
  return(error.SSE(x,y) / sum((mean(x, na.rm=TRUE) - x)^2))
}

# Relative absolute error.
error.RAE <- function(x, y) {
  return(error.SAE(x,y) / sum(abs(mean(x, na.rm=TRUE) - x)))
}

# Root mean squared error.
error.RMSE <- function(x, y) {
  return(sqrt(error.MSE(x,y)))
}

# Normalised relative mean squared error.
error.NRMSE_R <- function(x, y) {
  return(error.RMSE(x,y) / range(x, na.rm=TRUE))
}

# Normalised relative mean squared error.
error.NRMSE_CV <- function(x, y) {
  return(error.RMSE(x,y) / mean(x, na.rm=TRUE))
}

# Normalised relative mean squared error.
error.NRMSE_IQR <- function(x, y) {
  return(error.RMSE(x,y) / IQR(x, na.rm=TRUE))
}

# Relative relative mean squared error.
error.RRMSE <- function(x, y) {
  return(error.RMSE(x,y) / var(x, na.rm=TRUE))
}

error.HUBER <- function(x, y, delta=NA) {
  delta <- ifelse(is.na(delta), quantile(abs(x-mean(x, na.rm=TRUE)), probs=0.95, na.rm=TRUE), delta)
  ae <- abs(y - x)
  se <- (y - x)^2
  e <- ifelse(ae < delta, se/2, delta*ae - (delta^2)/2)
  return(e)
}

error.LOGCOSH <- function(x, y) {
  return(sum(log(cosh(y - x))))
}

# Quantile loss.
error.Q <- function(x, y, q=0.5) {
  e <- abs(x - y)
  e <- ifelse(x < y, sum((q-1)*e), sum(q*e))
  return(e)
}
