#' class_CLOPE
#'
#' Cluster text using the CLOPE transaction clustering algorithm.
#'
#' @param txt character vector with text to classify.
#' @param labels to apply to use for initial clusters.
#' @param R CLOPE parameter (default = 2).
#' @param remove.rare logic, drop rare tokens (default remove.rare=TRUE).
#' @param remove.frequent logic, drop frequent (common words that can be found in multiple clusters) tokens (default remove.frequent=TRUE).
#' @param progress logic, show a progress bar (default progress=TRUE).
#'
#' @export
class_CLOPE <- function(txt, labels=NULL, R=2, remove.rare=TRUE, remove.frequent=TRUE, progress=TRUE) {
  tkns <- txt_to_tran(txt, labels, remove.rare=remove.rare, remove.frequent=remove.frequent)
  id <- as.numeric(names(tkns))

  clust_id <- rep(NA, length(txt))
  clust_desc <- clust_id

  if (!is.null(labels)) {
    if (length(labels) != length(txt)) {
      stop("Length of labels needs to be the same as txt!")
    }

    id <- 1:length(txt)
    labels <- paste(id, labels, sep=".")

    cl1 <- tran_to_clust(tkns, labels)
    cl2 <- clope(cl1$Items, R=R, progress=progress)

    clust_id[id] <- cl2$Clust[cl1$Clust]
    desc <- sapply(cl2$Items, function(x) {
      nn <- names(sort(x, decreasing=TRUE))
      paste(nn[1:min(3, length(x))], collapse=" ")
    })
    clust_desc[id] <- desc[cl2$Clust][cl1$Clust]
    profit <- cl2$Profit
  } else {
    cl <- clope(tkns, R=R, progress=progress)

    clust_id[id] <- seq_along(cl$Items)[cl$Clust]
    desc <- sapply(cl$Items, function(x) {
      nn <- names(sort(x, decreasing=TRUE))
      paste(nn[1:min(3, length(x))], collapse=" ")
    })
    clust_desc[id] <- desc[cl$Clust]
    profit <- cl$Profit
  }

  cl <- data.frame(txt, clust_id, clust_desc)
  attr(cl, "profit") <- profit

  return(cl)
}

#' CLOPE - Transaction clustering.
#'
#' Clusters a list of lists (transactions) using the CLOPE algorithm developed by Yiling Yang et al (2002).
#' This implementation is very slow.
#'
#' @param x transactions list.
#' @param R repulsion factor (default R=2).
#' @param progress logic, show a progress bar (default progress=TRUE).
#' @return List clustered transactions.
#'
#' @references
#' Ching-Huang Yun, Kun-Ta Chuang, Ming-Syan Chen.
#'   An Efficient Clustering Algorithm for Market Basket Data Based on Small Large Ratios.
#'   Proceedings of the 25th International Computer Software and Applications Conference (COMPSAC 2001),
#'   pp. 505-510, 2001.
#'   \url{http://arbor.ee.ntu.edu.tw/~mschen/paperps/compsac150.pdf}
#'
#' Yiling Yang, Xudong Guan, Jinyuan You.
#'   CLOPE: A Fast and Effective Clustering Algorithm for Transactional Data.
#'   KDD '02 Proceedings of the eighth ACM SIGKDD International Conference on Knowledge
#'   Discovery and Data Mining, Pages 682-687, 2002.
#'   \url{http://www.inf.ufrgs.br/~alvares/CMP259DCBD/clope.pdf}
#'
#' @keywords clope
#' @export
clope <- function(x, R=2, progress=TRUE) {
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
  clust.S[1] <- sum(x[[1]], na.rm=TRUE)
  clust.W[1] <- length(x[[1]])
  clust.D[1] <- clust.N[1] * clust.S[1] / clust.W[1]^R
  clust.mat[[1]] <- add_tran(x[[1]], NULL)
  clust.tran[[1]] <- 1
  tran.clust[1] <- 1

  num_mat <- sum(x[[1]])

  if (progress) {
    cat("Assigning clusters\n")
    pb <- txtProgressBar(min=0, max=n, char=".", style=3, width=50)
  }

  for (i in 2:n) {
    add.N <- clust.N + 1
    add.S <- clust.S + sum(x[[i]], na.rm=TRUE)
    add.W <- clust.W + vapply(clust.mat, function(o) sum(!(names(x[[i]]) %in% names(o))), FUN.VALUE=0)
    add.D <- add.N * add.S / add.W^R
    add.DD <- add.D - clust.D
    best <- which.max(add.DD)

    D <- sum(x[[i]], na.rm=TRUE) / length(x[[i]])^R

    if (add.DD[best] > D) {
      clust.mat[[best]] <- add_tran(x[[i]], clust.mat[[best]])
      clust.tran[[best]] <- c(clust.tran[[best]], i)

      clust.N[best] <- add.N[best]
      clust.S[best] <- add.S[best]
      clust.W[best] <- add.W[best]
      clust.D[best] <- add.D[best]

      tran.clust[i] <- best
    } else {
      nn <- length(clust.mat) + 1

      clust.mat[[nn]] <- add_tran(x[[i]], NULL)
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

    if (progress) {
      setTxtProgressBar(pb, i)
    }
  }
  if (progress) {
    close(pb)
  }

  pass <- 0
  repeat {
    moved <- FALSE
    pass <- pass + 1

    if (progress) {
      cat(paste("Pass", pass, "\n"))
      pb <- txtProgressBar(min=0, max=n, char=".", style=3, width=50)
    }

    for (i in 1:n) {
      indx <- tran.clust[i]
      cl <- remove_tran(x[[i]], clust.mat[[indx]])

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
        clust.S[indx] <- clust.S[indx] - sum(x[[i]], na.rm=TRUE)
        clust.mat[[indx]] <- cl
        clust.W[indx] <- length(clust.mat[[indx]])
        clust.D[indx] <- clust.N[indx] * clust.S[indx] / clust.W[indx]^R

        clust.mat[[best]] <- add_tran(x[[i]], clust.mat[[best]])
        clust.N[best] <- add.N[best]
        clust.S[best] <- add.S[best]
        clust.W[best] <- add.W[best]
        clust.D[best] <- add.D[best]

        tran.clust[i] <- best
        moved <- TRUE
      }

      if (progress) {
        setTxtProgressBar(pb, i)
      }
    }
    if (progress) {
      close(pb)
    }

    if (!moved) {
      break
    }
  }

  P <- sum(clust.N * clust.S / clust.W^R, na.rm=TRUE) / sum(clust.N, na.rm=TRUE)
  v <- list(Items=clust.mat, Clust=tran.clust, Profit=P)

  return(v)
}
clope <- compiler::cmpfun(clope)

add_tran <- function(tran, clust) {
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
add_tran <- compiler::cmpfun(add_tran)

remove_tran <- function(tran, clust) {
  i <- intersect(names(clust), names(tran))
  if (length(i) > 0) {
    clust[i] <- clust[i] - tran[i]
  }
  clust <- clust[clust > 0]

  return(clust)
}
remove_tran <- compiler::cmpfun(remove_tran)

txt_to_tran <- function(txt, labels=NULL, remove.rare=TRUE, remove.frequent=TRUE) {
  txt <- gsub("[[:punct:]]", " ", txt)
  id <- 1:length(txt)

  has_char <- !(is.na(txt) | nchar(txt) == 0)
  txt <- txt[has_char]
  id <- id[has_char]

  # Get tokens (words).
  #tkns <- as.list(quanteda::tokens(txt))
  tkns <- tok_bigrams(txt, size=3)
  names(tkns) <- id

  if (remove.rare | remove.frequent) {
    x <- table(unlist(tkns))
    y <- data.frame(TEXT=names(x), N=as.numeric(x), stringsAsFactors=FALSE)
    y <- y[order(y$N, decreasing=TRUE), ]

    y.m <- sum(y$N * (1:nrow(y)-1)) / nrow(y)
    y.s <- sqrt(sum(y$N * (1:nrow(y) - y.m)^2) / nrow(y))
    n.min <- ifelse(remove.rare, 3, 0)
    n.max <- ifelse(remove.frequent, (y.m + 5*y.s), max(y$N))
    indx <- y$N >= n.min & y$N <= n.max
    y <- y$TEXT[indx]

    # Need to make sure that we don't delete all words in a text.
    tkns <- lapply(tkns, function(x) {
      indx <- x %in% y
      if (!all(!indx)) {
        x <- x[indx]
      }
      if (length(x) == 0) {
        x <- NULL
      }
      x
    })
  }

  tkns <- Filter(Negate(is.null), tkns)

  tkns <- lapply(tkns, function(x) {
    unlist(as.list(table(unlist(x))))
  })

  return(tkns)
}
txt_to_tran <- compiler::cmpfun(txt_to_tran)

tran_to_clust <- function(x, labels) {
  clust <- list(Items <- list(), Clust=vector("numeric"), Profit=NA)

  f <- factor(labels)

  y <- split(x, f)
  clust$Items <- lapply(y, function(x) {
    Reduce(add_tran, x)
  })
  clust$Clust <- as.integer(unclass(f))

  return(clust)
}
tran_to_clust <- compiler::cmpfun(tran_to_clust)
