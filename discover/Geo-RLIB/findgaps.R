require("goldR")
data(nilat)

gaps <- function(bhid, from, to)
{
	n <- length(bhid)
	first <- TRUE
	gaps <- NULL
	for (i in 1:(n-1))
	{
		if (to[i] < from[i+1] & bhid[i] == bhid[i+1])
		{
			mgap <- list(bhid=bhid, from=to[i], to=from[i+1])
			if (first==TRUE)
			{
				gaps <- mgap
				first <- FALSE
			}
			else
			{
				gaps <- rbind(gaps, mgap)
			}
		}
	}
	return(gaps)
}

x <- gaps(nilat.dh$BHID,nilat.dh$FROM,nilat.dh$TO)
