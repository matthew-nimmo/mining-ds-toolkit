read.gslib <-
function(filename) 
{
    char <- scan(filename, what=character())
    nums <- which(as.numeric(char)!='NaN')    
    nrcol <- as.numeric(char[nums[1]])
    begin <- nums[2]
    eind <- nums[length(nums)]
    nrrows <- (eind-begin+1)/nrcol
    indices <- seq(1,nrrows*nrcol-1,nrcol)
    mat <- matrix(ncol=nrcol, nrow=nrrows)
    i <- 1
    while(i <= nrcol)
    {
        mat[,i] <- as.numeric(char[(indices+i+begin-2)])
        i <- i+1
    }
    mat <- as.data.frame(mat)
    names(mat) <- c(char[(nums[1]+c(1:nrcol))])

    return(mat)
}
