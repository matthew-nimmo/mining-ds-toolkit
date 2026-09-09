which.peaks <-
function(x, partial=TRUE, sd.multiple=2/3)
{
	if (partial)
		ndx <- which(diff(c(TRUE,diff(x)>=0,FALSE))<0)
	else
		ndx <- which(diff(diff(x)>=0)<0)+1
	i <- x[ndx] > sd.multiple*sd(x)
	ndx[i]
}

#which.peaks <-
#function(x, partial=TRUE, decreasing=FALSE, sd.multiple=2/3)
#{
#	if (decreasing)
#	{
#		if (partial)
#			which(diff(c(FALSE,diff(x)>0,TRUE))>0)
#		else
#			which(diff(diff(x)>0)>0)+1
#	}
#	else
#	{
#		if (partial)
#			which(diff(c(TRUE,diff(x)>=0,FALSE))<0)
#		else
#			which(diff(diff(x)>=0)<0)+1
#	}
#}