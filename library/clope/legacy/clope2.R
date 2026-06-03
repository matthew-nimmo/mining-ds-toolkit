add.tran <- function(tran, clust) {
  cnt <- table(tran)
  new.tran <- cnt
  names(new.tran) <- names(cnt)

  i <- intersect(names(clust), names(new.tran))
  if (length(i) > 0) {
    z <- clust[i] + new.tran[i]
    i1 <- !(names(clust) %in% names(new.tran))
    i2 <- !(names(new.tran) %in% names(clust))
    if (any(i1))
      z <- c(clust[i1], z)
    if (any(i2))
      z <- c(z, new.tran[i2])
  } else {
    z <- new.tran
  }

  return(z)
}
library(compiler)
add.tran <- cmpfun(add.tran)

remove.tran <- function(tran, clust) {
  cnt <- table(tran)
  new.tran <- cnt
  names(new.tran) <- names(cnt)

  i <- intersect(names(clust), names(new.tran))
  if (length(i) > 0) {
    clust[i] <- clust[i] - new.tran[i]
  }
  clust <- clust[clust > 0]

  return(clust)
}
library(compiler)
remove.tran <- cmpfun(remove.tran)


# ---

clope2 <- function(x, R=2) {
  n <- length(x)

  clust.mat <- list()
  clust.tran <- list()
  tran.clust <- vector("numeric")
  clust.N <- vector("numeric")
  clust.S <- vector("numeric")
  clust.W <- vector("numeric")
  clust.D <- vector("numeric")

  clust.N[1] <- 1
  clust.S[1] <- length(x[[1]])
  clust.W[1] <- clust.S[1]
  clust.D[1] <- clust.N[1] * clust.S[1] / clust.W[1]^R
  clust.mat[[1]] <- add.tran(x[[1]], NULL)
  clust.tran[[1]] <- 1
  tran.clust[1] <- 1

  #print("Assigning clusters")
  #pb <- txtProgressBar(min=0, max=n, style=3)
  for (i in 2:n) {
    add.N <- clust.N + 1
    add.S <- clust.S + length(x[[i]])
    add.W <- clust.W + sapply(clust.mat, function(o) sum(!(x[[i]] %in% names(o))))
    add.D <- add.N * add.S / add.W^R
    add.DD <- add.D - clust.D
    best <- which.max(add.DD)

    D <- length(x[[i]]) / length(x[[i]])^R

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
    #setTxtProgressBar(pb, i)
  }
  #close(pb)

  pass <- 0
  repeat {
    moved <- FALSE
    pass <- pass + 1
    #print(paste("Pass", pass))
    #pb <- txtProgressBar(min=0, max=n, style=3)
    for (i in 1:n) {
      indx <- tran.clust[i]
      cl <- remove.tran(x[[i]], clust.mat[[indx]])

      P1 <- clust.D + clust.D[indx]
      add.N <- clust.N + 1
      add.S <- clust.S + length(x[[i]])
      add.W <- clust.W + sapply(clust.mat, function(o) sum(!(x[[i]] %in% names(o))))
      add.D <- add.N * add.S / add.W^R
      rem.D <- (clust.N[indx]-1) * sum(cl) / length(cl)^R
      P2 <- add.D + rem.D
      P <- P2 - P1
      P[indx] <- 0

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
      #setTxtProgressBar(pb, i)
    }
    #close(pb)
  
    if (!moved) {
      break
    }
  }

  P <- sum(clust.N * clust.S / clust.W^R) / sum(clust.N)
  v <- list(Items=clust.mat, Clust=tran.clust, Profit=P)
  return(v)
}


a_list <- list(
  c("a","b","c"),
  c("a","b"),
  c("a","b","d"),
  c("c","e"),
  c("a","b"),
  c("a","b"),
  c("d","e","f"),
  c("e","f"),
  c("a","b"),
  c("a","b","d"),
  c("b","d"),
  c("d","e"),
  c("e","f"),
  c("a","c")
)
clust <- clope2(a_list)
