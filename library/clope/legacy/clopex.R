clope <- function(x, R=2) {
  clust <- clope.init(x, R=R)

  pass <- 0
  repeat {
    moved <- FALSE
    pass <- pass + 1
    cat(paste("Pass", pass, "\n"))
    pb <- txtProgressBar(min=0, max=n, char=".", style=3)
    for (i in 1:n) {
      indx <- tran.clust[i]
      cl <- remove.tran(x[[i]], clust.mat[[indx]])

      P1 <- clust.D + clust.D[indx]

      add.N <- clust.N + 1
      add.S <- clust.S + sum(x[[i]], na.rm=TRUE)
      add.W <- clust.W + vapply(clust.mat, function(o) sum(!(names(x[[i]]) %in% names(o)), na.rm=TRUE), FUN.VALUE=0)
      add.D <- add.N * add.S / add.W^R
      add.D[indx] <- clust.D[indx]

      rem.D <- ifelse(length(cl)==0, -clust.D[indx], (clust.N[indx]-1) * sum(cl, na.rm=TRUE) / length(cl)^R)
      P2 <- add.D + rem.D
      P <- P2 - P1
      P[indx] <- clust.D[indx]

      best <- which.max(P)
      if (best != indx) {
        # Move transaction to new cluster
        clust.N[indx] <- clust.N[indx] - 1
        clust.S[indx] <- clust.S[indx] - length(x[[i]])
        clust.mat[[indx]] <- cl
        clust.W[indx] <- length(clust.mat[[indx]])
        clust.D[indx] <- clust.N[indx] * clust.S[indx] / clust.W[indx]^R

        clust.mat[[best]] <- add.tran(x[[i]], clust.mat[[best]])
        clust.N[best] <- add.N[best]
        clust.S[best] <- add.S[best]
        clust.W[best] <- add.W[best]
        clust.D[best] <- add.D[best]

        tran.clust[i] <- best
        moved <- TRUE
      }
      setTxtProgressBar(pb, i)
    }
    close(pb)

    if (!moved) {
      break
    }
  }

  P <- sum(clust.N * clust.S / clust.W^R, na.rm=TRUE) / sum(clust.N, na.rm=TRUE)
  v <- list(Items=clust.mat, Clust=tran.clust, Profit=P)

  return(v)
}

new.clust <- function(x=NULL, trans=1, R=2) {
  if (is.null(x)) {
    cl <- list(OTHER=1)
    attr(cl, "N") <- 0
    attr(cl, "S") <- 0
    attr(cl, "W") <- 0
    attr(cl, "D") <- 0
    attr(cl, "trans") <- NULL
  } else {
    cl <- x
    attr(cl, "N") <- 1
    attr(cl, "S") <- sum(x, na.rm=TRUE)
    attr(cl, "W") <- length(x)
    attr(cl, "D") <- sum(x, na.rm=TRUE) / length(x)^R
    attr(cl, "trans") <- trans
  }

  return(cl)
}

clope.init <- function(x, R=2) {
  clust <- list()

  clust[1] <- new.clust(x[[1]], R)
browser()
  # Pre-allocate arrays to improve performance
  n <- length(x)
  
  num_mat <- sum(x[[1]])
  
  cat("Assigning clusters\n")
  pb <- txtProgressBar(min=0, max=n, char=".", style=3)
  for (i in 2:n) {
    add.N <- clust.N + 1
    add.S <- clust.S + sum(x[[i]], na.rm=TRUE)
    add.W <- clust.W + vapply(clust.mat, function(o) sum(!(names(x[[i]]) %in% names(o))), FUN.VALUE=0)
    add.D <- add.N * add.S / add.W^R
    add.DD <- add.D - clust.D
    best <- which.max(add.DD)
    
    D <- sum(x[[i]], na.rm=TRUE) / length(x[[i]])^R
    
    if (add.DD[best] > D) {
      clust.mat[[best]] <- add.tran(x[[i]], clust.mat[[best]])
      clust.tran[[best]] <- c(clust.tran[[best]], i)
      
      clust.N[best] <- add.N[best]
      clust.S[best] <- add.S[best]
      clust.W[best] <- add.W[best]
      clust.D[best] <- add.D[best]
      
      tran.clust[i] <- best
    } else {
      nn <- length(clust.mat) + 1
      
      clust.mat[[nn]] <- add.tran(x[[i]], NULL)
      clust.tran[[nn]] <- i
      
      clust.N[nn] <- 1
      clust.S[nn] <- length(x[[i]])
      clust.W[nn] <- clust.S[nn]
      clust.D[nn] <- D
      
      tran.clust[i] <- nn
    }
    num_mat <- num_mat + sum(x[[i]], na.rm=TRUE)
    if (sum(unlist(clust.mat), na.rm=TRUE) != num_mat)
      stop(paste("Transaction", i, "not added"))
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
}

add.tran <- function(tran, clust) {
  i <- intersect(names(clust), names(tran))
  if (length(i) > 0) {
    z <- clust[i] + tran[i]
    i1 <- !(names(clust) %in% names(tran))
    i2 <- !(names(tran) %in% names(clust))
    if (any(i1))
      z <- c(clust[i1], z)
    if (any(i2))
      z <- c(z, tran[i2])
  } else {
    if (is.null(clust)) {
      z <- tran
    } else {
      z <- c(clust, tran)
    }
  }

  return(z)
}
#add.tran <- cmpfun(add.tran)

remove.tran <- function(tran, clust) {
  i <- intersect(names(clust), names(tran))
  if (length(i) > 0) {
    clust[i] <- clust[i] - tran[i]
  }
  clust <- clust[clust > 0]

  return(clust)
}
#remove.tran <- cmpfun(remove.tran)



# ----

a_list <- list(
  c(c=2,e=1),
  c(a=1,c=3),
  c(a=1,b=1),
  c(a=1,b=1,d=1),
  c(a=1,b=1),
  c(a=1,b=1),
  c(d=1,e=3,f=1),
  c(e=1,f=1),
  c(a=2,b=1),
  c(a=3,b=2,d=1),
  c(b=1,d=3),
  c(d=1,e=2),
  c(e=1,f=1)
)
clust <- clope.init(a_list, 0.5)
