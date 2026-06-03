# Cluster object.
cluster <- setClass("cluster", slots=c(S="numeric", W="numeric", N="numeric", Items="vector"))
setMethod("initialize", "cluster",
          function(.Object, N=numeric(0), S=numeric(0), W=numeric(0), Items=NULL) {
            .Object@N <- N
            .Object@S <- S
            .Object@W <- W
            .Object@Items <- Items
            .Object
          })

# Add transaction to cluster
add.tran <- function(tran, clust) {
  if (is.null(clust))
    clust <- new("cluster", N=0, S=0, W=0, Items=vector("numeric"))
  
  for(i in tran) {
    if (is.na(clust@Items[i]))
      clust@Items[i] <- 1
    else
      clust@Items[i] <- clust@Items[i] + 1
  }
  
  clust@N <- clust@N + 1
  clust@S <- sum(clust@Items, na.rm=TRUE)
  clust@W <- length(clust@Items)
  
  return(clust)
}

# Add transaction to cluster
remove.tran <- function(tran, clust) {
  for(i in tran) {
    clust@Items[i] <- clust@Items[i] - 1
  }
  clust@Items <- clust@Items[clust@Items > 0]

  clust@N <- clust@N - 1
  clust@S <- sum(clust@Items, na.rm=TRUE)
  clust@W <- length(clust@Items)
  
  return(clust)
}

# Calculate change in metrics for
# New transaction added to cluster.
add.delta <- function(clust, tran, r=2) {
  if (clust@N == 0)
    p.old <- 0
  else
    p.old <- (clust@N * clust@S) / clust@W^r
  
  clust.new <- add.tran(tran, clust)
  p.new <- (clust.new@N * clust.new@S) / clust.new@W^r
  
  delta <- p.new - p.old
  
  return(delta)
}

# Calculate change in metrics for
# New transaction removed from cluster.
remove.delta <- function(clust, tran, r=2) {
  if (clust@N == 0)
    p.old <- 0
  else
    p.old <- (clust@N * clust@S) / clust@W^r
  
  clust.new <- remove.tran(tran, clust)
  p.new <- (clust.new@N * clust.new@S) / clust.new@W^r
  
  delta <- p.new - p.old
  
  return(delta)
}

move.delta <- function(clust1, clust2, tran, r=2) {
  clust1.P <- clust1@N * clust1@S / clust1@W^r
  clust2.P <- clust2@N * clust2@S / clust2@W^r
  P1 <- clust1.P + clust2.P

  clust.rem <- remove.tran(tran, clust2)
  clust.add <- add.tran(tran, clust1)

  clust1.P <- clust.rem@N * clust.rem@S / clust.rem@W^r
  clust2.P <- clust.add@N * clust.add@S / clust.add@W^r
  P2 <- clust1.P + clust2.P

  delta <- P2 - P1

  return(delta)
}

# Calculate profit for clusters
profit <- function(clusters, r) {
  N <- sapply(clusters, function(o) o@N)
  S <- sapply(clusters, function(o) o@S)
  W <- sapply(clusters, function(o) o@W)
  
  P <- sum(N * S / W^r) / sum(N)
  
  return(P)
}

# ---

clope1 <- function(x, R=2) {
  n <- length(x)

  clust.mat <- list()
  clust.tran <- list()
  tran.clust <- vector("numeric", n)

  clust.mat[[1]] <- add.tran(x[[1]], NULL)
  clust.tran[[1]] <- 1
  tran.clust[1] <- 1

  #print("Assigning clusters")
  #pb <- txtProgressBar(min=0, max=n, style=3)
  for (i in 2:n) {
    cl <- clust.mat
    cl[length(cl)+1] <- new("cluster", N=0, S=0, W=0, Items=vector("numeric"))
    delta <- sapply(cl, add.delta, x[[i]], R)
    best <- which.max(delta)

    if (best > length(clust.mat)) {
      clust.mat[best] <- add.tran(x[[i]], NULL)
      clust.tran[[best]] <- i
    } else {
      clust.mat[best] <- add.tran(x[[i]], clust.mat[[best]])
      clust.tran[[best]] <- c(clust.tran[[best]], i)
    }
    tran.clust[i] <- best
    #setTxtProgressBar(pb, i)
  }
  #close(pb)

  pass <- 0

  #print("Refining clusters")
  repeat {
    moved <- FALSE
    pass <- pass + 1
    #print(paste("Pass", pass))
    #pb <- txtProgressBar(min=0, max=n, style=3)
    for (i in 1:n) {
      delta <- sapply(clust.mat, move.delta, clust.mat[[tran.clust[i]]], x[[i]], R)
      delta[tran.clust[i]] <- 0
      best <- which.max(delta)
      if (best != tran.clust[i]) {
        # Move transaction to new cluster
        clust.mat[tran.clust[i]] <- remove.tran(x[[i]], clust.mat[[tran.clust[i]]])
        clust.mat[best] <- add.tran(x[[i]], clust.mat[[best]])
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

  P <- profit(clust.mat, R)
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
clust <- clope1(a_list)
