# Cluster using the CLOPE algorithm (Yiling Yang et al., 2002)

# References:
# 2002, Yiling Yang, Xudong Guan, Jinyuan You, CLOPE: A Fast and Effective Clustering
# Algorithm for Transactional Data, KDD '02 Proceedings of the eighth ACM SIGKDD
# international conference on Knowledge discovery and data mining, Pages 682-687  
# http://www.inf.ufrgs.br/~alvares/CMP259DCBD/clope.pdf
#
# 2001, Ching-Huang Yun, Kun-Ta Chuang, Ming-Syan Chen, An Efficient Clustering
# Algorithm for Market Basket Data Based on Small Large Ratios, Proceedings of the 25th
# International Computer Software and Applications Conference (COMPSAC 2001), pp. 505-510.
# http://arbor.ee.ntu.edu.tw/~mschen/paperps/compsac150.pdf

# This implementation is very slow.
# Will need to try and vectorize it.

library(compiler)

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
  #cnt <- table(tran)
  #new.tran <- cnt
  #names(new.tran) <- names(cnt)

  #i <- intersect(names(clust), names(new.tran))
  i <- intersect(names(clust), names(tran))
  if (length(i) > 0) {
    #clust[i] <- clust[i] - new.tran[i]
    clust[i] <- clust[i] - tran[i]
  }
  clust <- clust[clust > 0]

  return(clust)
}
#remove.tran <- cmpfun(remove.tran)


# ---

clope3 <- function(x, R=2) {
  # Pre-allocate arrays to improve performance
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

  num_mat <- sum(x[[1]])

  print("Assigning clusters")
  pb <- txtProgressBar(min=0, max=n, style=3)
  for (i in 2:n) {
    add.N <- clust.N + 1
    add.S <- clust.S + sum(x[[i]])
    add.W <- clust.W + sapply(clust.mat, function(o) sum(!(names(x[[i]]) %in% names(o))))
    add.D <- add.N * add.S / add.W^R
    add.DD <- add.D - clust.D
    best <- which.max(add.DD)

    D <- sum(x[[i]]) / length(x[[i]])^R

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
    num_mat <- num_mat + sum(x[[i]])
    if (sum(unlist(clust.mat)) != num_mat)
      stop("Transaction not added")

    setTxtProgressBar(pb, i)
  }
  close(pb)

  pass <- 0
  repeat {
    moved <- FALSE
    pass <- pass + 1
    print(paste("Pass", pass))
    pb <- txtProgressBar(min=0, max=n, style=3)
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
      setTxtProgressBar(pb, i)
    }
    close(pb)

    if (!moved) {
      break
    }
  }

  P <- sum(clust.N * clust.S / clust.W^R) / sum(clust.N)
  v <- list(Items=clust.mat, Clust=tran.clust, Profit=P)

  return(v)
}


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
#clust <- clope3(a_list, 0.5)
#P <- sapply((1:30)*0.1, function(r) clope3(a_list, r)$Profit)
#plot((1:30)*0.1, P)
