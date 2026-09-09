# Invert named list.
invert <- function(lst) {
  tmp <- names(lst)
  names(tmp) <- lst
  
  return(tmp)
}
