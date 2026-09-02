ok <- function(x_samples, y_samples, v_samples, cov_func, x_target, y_target) {
  n <- length(x_samples)

  # Compute standard Euclidean spatial distances (N x N)
  dx <- outer(x_samples, x_samples, "-")
  dy <- outer(y_samples, y_samples, "-")
  H <- sqrt(dx^2 + dy^2)

  # Spatial Covariance Matrix (K)
  K_base <- cov_func(H)

  # Construct Augmented Ordinary Kriging System (N+1 x N+1)
  K_sys <- cbind(K_base, 1)
  K_sys <- rbind(K_sys, c(rep(1, n), 0))

  # Right-Hand Side Vector k
  h_target <- sqrt((x_samples - x_target)^2 + (y_samples - y_target)^2)
  k_base <- cov_func(h_target)
  k_sys <- c(k_base, 1)

  # Solve for Kriging Weights
  w_augmented <- solve(K_sys, k_sys)
  w <- w_augmented[1:n]

  # Compute Estimate
  estimate <- sum(w * v_samples)
  return(estimate)
}
