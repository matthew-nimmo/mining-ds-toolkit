value_class<- function(x) {
  y <- rep("empty", length(x))
  y <- ifelse(grepl("^$", x), "empty", y)
  y <- ifelse(grepl("a", x), "character", y)
  y <- ifelse(grepl("d", x), "numeric", y)
  
  return(y)
}