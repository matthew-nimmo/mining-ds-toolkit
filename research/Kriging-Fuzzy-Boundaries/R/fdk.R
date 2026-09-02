#' Variant A: Fuzzy Domain Kriging via Symmetric Covariance Scaling (Corrected)
fdk_a <- function(datax, datay, dataz, u, cov_func, target_x, target_y, target_u = 1.0) {
  n <- length(datax)
  u <- 1 - u

  dx <- outer(datax, datax, "-")
  dy <- outer(datay, datay, "-")
  H <- sqrt(dx^2 + dy^2)

  # Mask covariance symmetrically
  U_mask <- outer(u, u, function(ui, uj) sqrt(ui * uj))
  K_A <- cov_func(H) * U_mask

  # Add tiny regularization epsilon to diagonal to ensure numerical stability for mu=0
  diag(K_A) <- diag(K_A) + 1e-8

  K_sys <- cbind(K_A, 1)
  K_sys <- rbind(K_sys, c(rep(1, n), 0))

  h_target <- sqrt((datax - target_x)^2 + (datay - target_y)^2)
  k_base <- cov_func(h_target)
  k_A <- k_base * sqrt(u * target_u)
  k_sys <- c(k_A, 1)

  w_aug <- solve(K_sys, k_sys)
  w <- w_aug[1:n]

  return(sum(w * dataz))
}

#' Variant B: Fuzzy Domain Kriging via Inverse Variance Penalization (Corrected)
fdk_b <- function(datax, datay, dataz, u, cov_func, target_x, target_y, target_u = 1.0, sigma2_R = 10.0) {
  n <- length(datax)

  # Base Distance & Covariance
  dx <- outer(datax, datax, "-")
  dy <- outer(datay, datay, "-")
  H <- sqrt(dx^2 + dy^2)
  K_base <- cov_func(H)

  # Diagonal Variance Penalty (R)
  R <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    if (u[i] > 1e-6) {
      R[i, i] <- sigma2_R * (1.0 - u[i]) / u[i]
    } else {
      R[i, i] <- 1e8 # Numerical infinity for u_i = 0
    }
  }

  K_B <- K_base + R

  # Augmented Matrix (OK System)
  K_sys <- cbind(K_B, 1)
  K_sys <- rbind(K_sys, c(rep(1, n), 0))

  # Target RHS Vector
  h_target <- sqrt((datax - target_x)^2 + (datay - target_y)^2)
  k_base <- cov_func(h_target)
  k_sys <- c(k_base, 1)

  # Solve Weights & Estimate
  w_aug <- solve(K_sys, k_sys)
  w <- w_aug[1:n]

  return(sum(w * dataz))
}
