norm_sulfide <- function(cu_pct, s_pct, cnscu_pct=0, as_pct=0) {
  cu_pct <- ifelse(is.na(cu_pct), 0, cu_pct)
  s_pct <- ifelse(is.na(s_pct), 0, s_pct)
  cnscu_pct <- ifelse(is.na(cnscu_pct), 0, cnscu_pct)
  as_pct <- ifelse(is.na(as_pct), 0, as_pct)
  
  #browser()
  #Calculate molar proportions.
  cu_w <- cu_pct / 63.55
  cnscu_w <- cnscu_pct / 63.55
  s_w <- s_pct / 32.06
  as_w <- as_pct / 74.92
  
  apy <- 0
  py <- 0
  cc <- 0
  cpy <- 0
  
  #Calculate proportion of arsenianpyrite.
  if (as_w > 0) {
    apy <- as_w
    s_w <- s_w - 2 * as_w
  }
  
  #Calculate proportion of chalcocite.
  if (cnscu_w > 0) {
    cc <- cnscu_w
    cnscu_w <- 0
  } else if (cu_w > 0 & s_w > 0) {
    if (cu_w / s_w > 0.5) {
      cc <- cu_w
      cu_w <- 0
      s_w <- s_w - cu_w / 2
    }
  }
  
  #Calculate proportion of chalcopyrite.
  if (cu_w > 0) {
    cpy <- cu_w
    s_w <- s_w - 2 * cpy
  }
  
  #Calculate proportion of pyrite.
  if (s_w > 0) {
    py = s_w / 2
  }
  
  apy <- 129.5 * apy
  py <- 119.96 * py
  cc <- 127.67 * cc
  cpy <- 183.51 * cpy
  
  return(c(py=py, cpy=cpy, cc=cc, apy=apy))
}
norm_sulfide_v <- Vectorize(norm_sulfide, SIMPLIFY=TRUE, USE.NAMES=TRUE)
