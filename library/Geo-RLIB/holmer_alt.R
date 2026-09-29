dh_intersect <- function(af, at, ai, bf, bt) {
  na <- length(af)
  nb <- length(bf)

  intersects <- vector("list", na)

  for (i in 1:na) {
    for (j in 1:nb) {
      # intersect FROM
      if (at[i] > bf[j] & af[i] < bf[j]) {
        intersects[i] <- append(intersects[i], bf[j])
      }
      # intersect TO
      if (at[i] > bt[j] & af[i] < bt[j]) {
        intersects[i] <- append(intersects[i], bt[j])
      }
    }
  }

  zf <- list()
  zt <- list()
  zi <- list()
  for (i in 1:na) {
    zf <- append(zf, af[i])
    for (j in sort(intersects[i])) {
      zt <- append(zt, j)
      zi <- append(zi, ai[i])
      zf <- append(zf, j)
      zt <- append(zt, at[i])
      zi <- append(zi, ai[i])
    }
  }

  return(list(zf, zt, zi))
}

dh_min_int <- function(la, lb, ia, ib, tol=0.01) {
  # equal ?
  if ((lb-1) <= la & la <= (lb+1)) {
    return(ia+1, ib+1, (la+lb)/2)
  }

  # la < lb ?
  if (la<lb) {
    return(ia+1, ib, la)
  }

  # lb < la ?
  if (lb<la) {
    return(c(ia, ib+1, lb))
  }
}

dh_merge_one_dhole <- function(la, lb, ida, idb, tol=0.01) {
  # general declarations
  ia <- 0
  ib <- 0

  maxia <- length(la)
  maxib <- length(lb)
  maxiab <- length(lb) + length(la)
  inhole <- TRUE
  n <- -1

  # prepare output as numpy arrays
  np_newida <- rep(0, maxiab)
  np_newidb <- rep(o, maxiab)
  np_lab <- rep(0, maxiab)

  # get memory view of the numpy arrays
  newida <- np_newida
  newidb <- np_newidb
  lab <- np_lab

  #check those are complete dholes
  #assert la[maxia-1]==lb[maxib-1]

  #loop on drillhole
  while (inhole) {
    # get the next l interval and l idex for drillhole a and b
    x = min_int(la[ia], lb[ib], ia, ib, tol=0.01)
    n <- n + 1
    newida[n] <- ida[x[1]-1]
    newidb[n] <- idb[x[2]-1]
    lab[n] <- x[3]

    #this is the end of hole (this fails if maxdepth are not equal)
    if (ia==maxia | ib==maxib) {
      inhole <- FALSE
    }
  }

  return(list(n, np_lab[n+1], np_newida[n+1], np_newidb[n+1]))
}

dh_fillgap1Dhole <- function(in_f, in_t, id, tol=0.01, endhole=-1) {
  nint <- -1
  ngap <- -1
  noverlap <- -1
  ndat <- length(in_f)

  #make a deep copy of the from to intervals and create a memory view
  np_f <- rep(0, ndat)
  np_t <- rep(0, ndat)
  f <- np_f
  t <- np_t
  for (i in 1:ndat) {
    f[i] <- in_f[i]
    t[i] <- in_t[i]

    #make a long array (reserve memory space)
    np_nf <- rep(0, ndat*2+4)
    np_nt <- rep(0, ndat*2+4)
    np_nID <- rep(0, ndat*2+4)
    np_gap <- rep(0, ndat*2+4)
    np_overlap <- rep(0, ndat*2+4)
    nt <- np_nt
    nID <- np_nID
    gap = np_gap
    nf <- np_nf
    overlap <- np_overlap

    # gap first interval
    if (f[0]>tol) {
      nint <- nint + 1
    }
    nf[nint] <- 0.0
    nt[nint] <- f[0]
    nID[nint] <- -999
    ngap <- ngap + 1
    gap[ngap]=id[0]
  }

  for (i in 1:(ndat-1)) {
    # there is no gap?
    if (-tol <= f[i+1]-t[i] & f[i+1]-t[i] <= tol) {
      # existing sample
      nint <- nint + 1
      nf[nint] <- f[i]
      nt[nint] <- f[i+1]
      nID[nint] <- id[i]
      continue()
    }

    # there is a gap?
    if (f[i+1]-t[i] >= tol) {
      # existing sample
      nint <- nint + 1
      nf[nint] <- f[i]
      nt[nint] <- t[i]
      nID[nint] <- id[i]
      #gap
      nint <- nint + 1
      nf[nint] <- t[i]
      nt[nint] <- f[i+1]
      nID[nint] <- -999
      ngap <- ngap + 1
      gap[ngap] <- id[i]
      continue()
    }

    # there is overlap?
    if (f[i+1]-t[i]<=-tol) {
      # the overlap is smaller that the actual sample?
      if (f[i+1]>f[i]) {
        # existing sample
        nint <- nint + 1
        nt[nint] <- max(f[i+1],f[i]) # ising max here to avoid negative interval from>to
        nf[nint] <- f[i]
        nID[nint] <- id[i]
        noverlap <- noverlap + 1
        overlap[noverlap] <- id[i]
        continue()
        # large overlap?
      } else {
        #whe discard next interval by making it 0 length interval
        # this will happen only in unsorted array...
        noverlap <- noverlap + 1
        overlap[noverlap] <- id[i]
        # update to keep consistency in next loopondh
        nint <- nint + 1
        nt[nint] <- t[i] # ising max here to avoid negative interval from>to
        nf[nint] <- f[i]
        nID[nint] <- id[i]
        f[i+1] <- t[i+1]
      }
    }

    # there are not problem (like a gap or an overlap)
    if (((f[ndat-1]-t[ndat-2]) >= -tol &  (f[ndat-1]-t[ndat-2]) <= tol) & ndat>1) {
      nint <- nint + 1
      nt[nint] <- t[ndat-1] # ising max here to avoid negative interval from>to
      nf[nint] <- f[ndat-1]
      nID[nint] <- id[ndat-1]
    } else {
      # just add the sample (the problem was fixed in the previous sample)
      nint <- nint + 1
      nt[nint] <- t[ndat-1] # ising max here to avoid negative interval from>to
      nf[nint] <- f[ndat-1]
      nID[nint] <- id[ndat-1]
    }

    # add end of hole
    if (endhole>-1) {
      # there is an end of hole gap?
      if ((tol>endhole-t[ndat-2]) & ndat>1) {
        nint <- nint + 1
        nt[nint] <- endhole
        nf[nint] <- t[ndat-1]
        nID[nint] <- -999
        ngap <- ngap + 1
        gap[ngap] <- -888  # this is a gap at end of hole
      }
      # there is an end of hole overlap?
      if ((tol>endhole-t[ndat-2]) & ndat>1) {
        nint <- nint + 1
        nt[nint] <- endhole
        nf[nint] <- t[ndat-1]
        nID[nint] <- -999
        noverlap <- noverlap + 1
        overlap[noverlap] <- -888 # this is an overlap at end of hole
      }
      # there is no gap or overlap, good... then fix small differences
      if ((tol<endhole-t[ndat-2]) & (endhole-t[ndat-2]>-tol)) {
        nt[nint] <- endhole
      }
      # make first interval start at zero, if it is != to zero but close to tolerance
      if (0 < nf[1] & nf[1] <= tol) {
        nf[1] <- 0
      }
    }
  }

  return (np_nf[nint+1], np_nt[nint+1], np_nID[nint+1], np_gap[ngap+1], np_overlap[noverlap+1])
}
