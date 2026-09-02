read.DM <- function(filename)
{
  #------
  # read the data binary file
  byte.order <- 0
  ieee <- if(.Platform$endian == "big") 1 else 0
  endian <- if(ieee == byte.order | byte.order < 0) .Platform$endian else "swap"

  if (!file.exists(filename))
    stop("read.DM: Could not open input file: ", filename)

  f = file(filename, "rb")

  #------
  # Read header
  filename <- rawToChar(readBin(f, raw(), n=8, endian=endian))
  directory <- rawToChar(readBin(f, raw(), n=8, endian=endian))
  description <- rawToChar(readBin(f, raw(), n=64, endian=endian))
  owner <- rawToChar(readBin(f, raw(), n=8, endian=endian))
  ownerPerms <- readBin(f, numeric(), size=4, endian=endian)
  otherPerms <- readBin(f, numeric(), size=4, endian=endian)
  modifyDate <- readBin(f, numeric(), size=4, endian=endian)
  numFields <- readBin(f, numeric(), size=4, endian=endian)
  numPages <- readBin(f, numeric(), size=4, endian=endian)
  recsLastPage <- readBin(f, numeric(), size=4, endian=endian)

  #------
  # Read fields
  fieldname <- vector()
  type <- vector()
  type <- vector()
  logicalRecPos <- vector()
  wordNumber <- vector()
  unit <- vector()
  default <- vector()
  size <- vector()

  pname <- vv <- nf <- 0
  for (v in 1:numFields)
  {
    xfieldname <- rawToChar(readBin(f, raw(), n=8, endian=endian))
    xtype <- rawToChar(readBin(f, raw(), n=4, endian=endian)[1])
    xlogicalRecPos <- readBin(f, numeric(), size=4, endian=endian)
    xwordNumber <- readBin(f, numeric(), size=4, endian=endian)
    xunit <- readBin(f, numeric(), size=4, endian=endian)
    if(xtype == "N")
      xdefault <- readBin(f, numeric(), size=4, endian=endian)
    else
    {
      xdefault <- rawToChar(readBin(f, raw(), n=4, endian=endian))
      xdefault <- sub('[[:space:]]+$', '', xdefault)
    }

    if(pname == xfieldname)
    {
      size[vv] <- size[vv] + 4
      default[vv] <- paste(default[vv], xdefault, sep="")
    }
    else
    {
      vv <- vv + 1
      fieldname[vv] <- sub('[[:space:]]+$', '', xfieldname)
      type[vv] <- sub('[[:space:]]+$', '', xtype)
      logicalRecPos[vv] <- xlogicalRecPos
      wordNumber[vv] <- xwordNumber
      unit[vv] <- xunit
      default[vv] <- xdefault
      size[vv] <- 4
      pname <- xfieldname
    }

    if(xlogicalRecPos > 0)
      nf <- nf + 1
  }
  #------
  # Skip to end of header page
  x <- 1936 - (numFields * 28)
  #x <- readBin(f, raw(), n=x, endian=endian)
  seek(f, x, origin="current")

  numFields <- length(fieldname)

  #------
  # Create empty data
  if(nf > 0)
    nrp <- as.integer(508 / nf)
  else
    nrp = 0
  nd <- (numPages - 2) * nrp + recsLastPage;
  nrb <- 2048 - (nf * nrp * 4)

  np <- numPages - 1;
  if(np < 0)
    np <- 0;
  for(v in 1:numFields)
  {
    d <- as.vector(1:nd, mode="numeric")
    if(exists("adata"))
      adata[v] <- d
    else
      adata <- data.frame(d)
  }
  colnames(adata) <- fieldname

  #------
  # Read data
  r <- 1
  for (p in 1:np)
  {
    if(p == np)
      nrp <- recsLastPage

    for(pr in 1:nrp)
    {
      for(v in 1:numFields)
      {
        if(logicalRecPos[v] != 0)
        {
          if(type[v] == "N")
          {
            # check if variable is implicit or not
            adata[r, v] <- readBin(f, numeric(), size=4, endian=endian)
          }
          else
          {
            # check if variable is implicit or not
            x <- rawToChar(readBin(f, raw(), n=size[v], endian=endian))
            adata[r, v] <- sub('[[:space:]]+$', '', x)
          }
        }
        else
          adata[r, v] <- default[v]
      }
      r <- r + 1
    }

    x <- readBin(f, raw(), n=nrb, endian=endian)
  }

  close(f)

  return(adata)
}

#write.ENVI = function(X, filename, interleave=c("bsq", "bil", "bip") )
#{ # write matrix or data cube to binary ENVI file
#  if (is.vector(X)) {
#    nCol = length(X)
#    nRow <- nBand <- 1
#  } else {
#    d = dim(X)
#    nRow  = d[1]
#    nCol  = d[2]
#    nBand = prod(d)/(nRow*nCol)
#  }
#  dim(X) = c(nRow, nCol, nBand)    # make it into 3D array in case it was not
#
#  # check data type
#  data.type = 0
#  if (is.double (X)) data.type = 5 # 64-bit double
#  if (is.integer(X)) data.type = 3 # 32-bit int
#  if (is.complex(X)) data.type = 9 # 2x64-bit complex<double>
#  if (data.type == 0) {            # do not know what is it -> make it a double
#	  X = as.double(X)
#    data.type = 5
#  }
#
#  # change interleave and store tha data
#  interleave = match.arg(interleave)
#  if      (interleave=="bil") X=aperm(X, c(2,3,1))  # R's [row,col,band] -> bil [col,band,row]
#  else if (interleave=="bip") X=aperm(X, c(3,2,1))  # R's [row,col,band] -> bip [band,col,row]
#  else if (interleave=="bsq") X=aperm(X, c(2,1,3))  # R's [row,col,band] -> bsq [col,row,band]
#  writeBin(as.vector(X), filename)                  # write Envi file
#
#  # write header file
#  out  = "ENVI\ndescription = { R-language data }\n"
#  out  = paste(out, "samples = ", nCol, "\n", sep="")
#  out  = paste(out, "lines = ", nRow, "\n", sep="")
#  out  = paste(out, "bands = ", nBand, "\n", sep="")
#  out  = paste(out, "data type = ",data.type,"\n", sep="")
#  out  = paste(out, "header offset = 0\n", sep="")
#  out  = paste(out, "interleave = ",interleave,"\n", sep="")   # interleave is assumed to be bsq - in case of 1 band images all 3 formats are the same
#  ieee = if(.Platform$endian=="big") 1 else 0       # does this machine uses ieee (UNIX) format? or is it intel format?
#  out  = paste(out, "byte order = ", ieee, "\n", sep="")
#  cat(out, file=paste(filename, ".hdr", sep=""))
#  invisible(NULL)
#}


