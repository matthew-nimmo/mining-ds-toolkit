setClass("transaction", representation(id="numeric", cluster_pos="numeric", cluster_id="numeric", items="character"),
         prototype(id=0, cluster_pos=0, cluster_id=0, items=character(0)))

setClass("cluster", representation(id="numeric", transactions="list", items="character", occ="numeric", n="numeric", s="numeric", w="numeric"),
         prototype(id=0, transactions=list(), items=character(0), occ=numeric(0), n=1, s=0, w=0))

setMethod(initialize, "cluster", function(.Object, transactions=list(), ...) {
  .Object <- callNextMethod(.Object, ...)

  .Object@transactions <- transactions
  x <- table(transactions[[1]]@items)
  .Object@occ <- as.numeric(x)
  names(.Object@occ) <- names(x)
  .Object@n <- length(transactions)
  .Object@s <- length(.Object@occ)
  .Object@w <- .Object@s

  return(.Object)
})

setGeneric("add_item", function(cl, item) standardGeneric("add_item"))
setMethod("add_item", signature("cluster", "character"), function(cl, item) {
  if (item %in% names(cl@occ)) {
    cl@occ[item] = cl@occ[item] + 1
  } else {
    cl@occ[item] <- 1
  }

  return(cl)
})

setGeneric("delete_item", function(cl, item) standardGeneric("delete_item"))
setMethod("delete_item", signature("cluster", "character"), function(cl, item) {
  if (item %in% names(cl@occ)) {
    if (cl@occ[item] == 1) {
      cl@occ <- cl@occ[!(names(cl@occ) %in% item)]
    } else {
      cl@occ[item] = cl@occ[item] - 1
    }
  }

  return(cl)
})

setGeneric("get_delta", function(cl, items, repulsion=2) standardGeneric("get_delta"))
setMethod("get_delta", signature("cluster", "character", "numeric"), function(cl, items, repulsion) {
  s_new <- cl@s + length(items)
  w_new <- cl@w

  for (item in items) {
    if (!(item %in% names(cl@occ))) {
      w_new <- w_new + 1
    }
  }

  if (cl@n == 0) {
    delta_profit <- s_new / w_new^repulsion
  } else {
    profit <- cl@s * cl@n / cl@w^repulsion
    profit_new <- s_new * (cl@n + 1) / w_new^repulsion
    delta_profit <- profit_new - profit
  }

  return(delta_profit)
})

setGeneric("add_transaction", function(cl, trans) standardGeneric("add_transaction"))
setMethod("add_transaction", signature("cluster", "transaction"), function(cl, trans) {
  for (item in trans@items) {
    cl <- add_item(cl, item)
  }

  cl@transactions <- c(cl@transactions, trans)
  cl@s <- cl@s + length(trans@items)
  cl@w <- length(cl@occ)
  cl@n <- cl@n + 1

  return(cl)
})

setGeneric("remove_transaction", function(cl, trans) standardGeneric("remove_transaction"))
setMethod("remove_transaction", signature("cluster", "transaction"), function(cl, trans) {
  for (item in trans@items) {
    cl <- delete_item(cl, item)
  }

  cl@transactions[trans@cluster_pos] <- NA

  cl@s <- cl@s - length(trans@items)
  cl@w <- length(cl@occ)
  cl@n <- cl@n - 1

  return(cl)
})

clear_empty_transactions <- function(clusters) {
  j <- sapply(clusters, class) != "NULL"
  clusters <- clusters[j]

  for (i in seq_along(clusters)) {
    j <- sapply(clusters[[i]]@transactions, class) != "transaction"
    clusters[[i]]@transactions[j] <- NULL
  }

  return(clusters)
}

clear_empty_clusters <- function(clusters) {
  i <- sapply(clusters, function(cluster) cluster@n == 0)
  if (any(i)) {
    clusters <- clusters[i]
  }

  return(clusters)
}

clusterize <- function(transactions, repulsion=4.0) {
  clusters <- list()

  cat("Assigning clusters\n")
  pb <- txtProgressBar(min=0, max=length(transactions), char=".", style=3, width=50)
  for (i in seq_along(transactions)) {
    #clusters = add_instance_to_best_cluster(clusters, transaction, repulsion)
    best_cluster <- best_cluster(clusters, transactions[[i]], repulsion)
    transactions[[i]]@cluster_id <- best_cluster
    if (best_cluster <= length(clusters)) {
      transactions[[i]]@cluster_pos <- clusters[[best_cluster]]@n + 1
      clusters[[best_cluster]] <- add_transaction(clusters[[best_cluster]], transactions[[i]])
    } else {
      transactions[[i]]@cluster_pos <- 1
      clusters[[best_cluster]] <- new("cluster", id=i, list(transactions[[i]]))
    }

    setTxtProgressBar(pb, i)
  }
  close(pb)

  pass <- 0
  while(TRUE) {
    pass <- pass + 1
    cat(paste("Pass", pass, "\n"))
    pb <- txtProgressBar(min=0, max=length(transactions), char=".", style=3, width=50)

    moved <- FALSE

    for (i in seq_along(transactions)) {
      original_cluster_id <- transactions[[i]]@cluster_id
      clusters[[original_cluster_id]] <- remove_transaction(clusters[[original_cluster_id]], transactions[[i]])

      #clusters <- add_instance_to_best_cluster(clusters, transaction, repulsion)
      best_cluster <- best_cluster(clusters, transactions[[i]], repulsion)
      if (best_cluster <= length(clusters)) {
        clusters[[best_cluster]] <- add_transaction(clusters[[best_cluster]], transactions[[i]])
        transactions[[i]]@cluster_id <- best_cluster
      } else {
        cluster <- new("cluster", id=i, list(transactions[[i]]))
        clusters[[i]] <- cluster
      }

      if (transactions[[i]]@cluster_id != original_cluster_id) {
        moved <- TRUE
      }

      setTxtProgressBar(pb, i)
    }
    close(pb)

    if (!moved) {
      break
    }
  }

  clusters <- clear_empty_transactions(clusters)
  clusters <- clear_empty_clusters(clusters)

  return(transactions)
}

add_instance_to_best_cluster <- function(clusters, transaction, repulsion) {
  best_cluster <- 0
  items = transaction@items
  temp_s = length(items)
  temp_w = temp_s

  max_delta <- temp_s / temp_w^repulsion
  best_delta <- 0

  for (cluster in clusters) {
    delta <- get_delta(cluster, items, repulsion)
    if (delta > best_delta) {
      if (delta > max_delta) {
        cluster <- add_transaction(cluster, transaction)
        return(clusters)
      } else {
        best_delta <- delta
        best_cluster <- cluster
      }
    }
  }

  if (best_delta >= max_delta) {
    best_cluster <- add_transaction(best_cluster, transaction)
    return(clusters)
  }

  i <- length(clusters) + 1
  cluster <- new("cluster", id=i, list(transaction))
  clusters[[i]] <- cluster

  return(clusters)
}

best_cluster <- function(clusters, transaction, repulsion) {
  best_cluster <- 0
  items = transaction@items
  temp_s = length(items)
  temp_w = temp_s

  max_delta <- temp_s / temp_w^repulsion
  best_delta <- 0

  for (i in seq_along(clusters)) {
    if (is.null(clusters[[i]])) {
      delta <- 0
    } else {
      delta <- get_delta(clusters[[i]], items, repulsion)
    }
    if (delta > best_delta) {
      if (delta > max_delta) {
        return(i)
      } else {
        best_delta <- delta
        best_cluster <- i
      }
    }
  }

  best_cluster <- ifelse(best_cluster == 0, length(clusters) + 1, best_cluster)

  return(best_cluster)
}

# ------------

#txt <- c("a b c d", "a a b c", "g h d", "g h c")
#txt <- strsplit(txt, " ")
#trans <- vector("list", length(txt))
#for (i in seq_along(txt)) {
#  trans[[i]] <- new("transaction", id=i, items=txt[[i]])
#}
#x <- clusterize(trans, repulsion=4)

# ------------

library(feather)
library(dplyr)
library(plates)

df <- read_feather("test_data.feather")

txt <- tolower(df$work_order_desc)
txt <- gsub("\\b[[:alpha:]]*[[:digit:]]+[[:alpha:]]*\\b", "", txt)
txt <- gsub(" [[:punct:]]+ ?", " ", txt)
txt <- gsub("^[[:punct:]]+", "", txt)
txt <- gsub("[[:punct:]]+$", "", txt)
txt <- gsub("[[:punct:]]", " ", txt)
txt <- gsub("\\b[[:alpha:]]{1,2}\\b", " ", txt)
txt <- gsub(" +", " ", txt)
txt <- gsub("^ ", "", txt)
txt <- gsub(" $", "", txt)
df$txt <- txt

txt <- strsplit(txt, " ")

trans <- vector("list", length(txt))
for (i in seq_along(txt)) {
  trans[[i]] <- new("transaction", id=i, items=txt[[i]])
}

x <- clusterize(trans, repulsion=3)
df$cluster_id <- sapply(x, function(o) o@cluster_id)
df <- arrange(df, cluster_id)
