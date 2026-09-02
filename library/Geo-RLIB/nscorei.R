#' Normal score back transform.
#'
#' Back-transform normal scores.
#' @param scores the normal scores.
#' @param nscore list as produced by \code{nscore}.
#'
#' @return numeric vector of backtransformed values.
#'
#' @references from \url{https://msu.edu/~ashton/temp/nscore.R}
#'
#' @keywords nscore
#' @export
nscorei <- function(y, nscore) {
  # ltail, utail options:
  #   1 = linear interpolation
  #   2 = power model interpolation
  #   4 = hyperbolic model interpolation (only for upper tail)
  backtr0 <- function(y, x, x.nscore, zmin, zmax,
                      ltail=1, ltpar=1, utail=1, utpar=1) {
    powint <- function(xlow, xhigh, ylow, yhigh, xval, pow) {
      if((xhigh-xlow) < 1.0e-20) {
        z <- (yhigh + ylow) / 2.0
      } else {
        z <- ylow + (yhigh-ylow) * (((xval-xlow)/(xhigh-xlow))^pow)
      }
      return(z)
    }

    nt <- length(x)
    if(y <= x.nscore[1]) {
      bktr <- x.nscore[1]
      cdflo <- pnorm(x.nscore[1])
      cdfbt <- pnorm(y)
      if (ltail == 1) {
        bktr <- powint(0.0, cdflo, zmin, x[1], cdfbt, 1.0)
      } else if (ltail == 2) {
        cpow <- 1.0 / ltpar
        bktr <- powint(0.0, cdflo, zmin, x[1], cdfbt, cpow)
      }
    } else if(y >= x.nscore[nt]) {
      bktr = x[nt]
      cdfhi  = pnorm(x.nscore[nt])
      cdfbt  = pnorm(y)
      if(utail == 1) {
        bktr = powint(cdfhi, 1.0, x[nt], zmax, cdfbt, 1.0)
      } else if(utail == 2) {
        cpow   = 1.0 / utpar
        bktr = powint(cdfhi, 1.0, x[nt], zmax, cdfbt, cpow)
      } else if(utail == 4) {
        lambda <- (x[nt]^utpar) * (1.0 - cdfhi)
        bktr <- (lambda / (1.0 - cdfbt))^(1.0 / utpar)
      }
    } else {
      j <- findInterval(y, x.nscore)
      j <- max(min((nt-1), j), 1)
      bktr <- powint(x.nscore[j], x.nscore[j+1], x[j], x[j+1], y, 1.0)
    }

    return(bktr)
  }

  sapply(y, backtr0, nscore$trn.table$x, nscore$trn.table$score,
         nscore$min, nscore$max)
}
