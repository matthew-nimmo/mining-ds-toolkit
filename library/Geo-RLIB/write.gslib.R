write.gslib <-
function(dat, file, nodatavalue=-99999)
{
	cat('GSLIB file created in R\n', file=file)
    cat(length(names(dat)), file=file, append=TRUE)
    cat('\n', file=file, append=TRUE)
    write(cbind(names(dat)), file=file, append=TRUE)
    for(i in 1:ncol(dat))
	{
		dat[is.na(dat[,i]),i] <- nodatavalue
		dat[dat[,i]=='NA',i] <- nodatavalue
		dat[is.infinite(dat[,i]),i] <- nodatavalue
	}
    write.table(dat,file=file, append=TRUE, sep='\t', col.names=FALSE, row.names=FALSE)
}
