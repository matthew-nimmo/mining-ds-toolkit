ToLabel <- function(x)
{
  if (missing(x))
    return("")

  lab <- deparse(substitute(x))

  start <- regexpr("\\$", lab)[1] + 1
  if(start==0)
    start=1

  end <- regexpr("\\[", lab)[1] - 1
  if(end<0)
    end <- nchar(lab)

  s <- gsub(".\\$","",deparse(substitute(x)))
  s
}
