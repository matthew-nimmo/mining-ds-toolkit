holmer <- function(x, y, id="hole_id", from="from", to="to") {
  i <- c(id, from, to) %in% names(x)
  if (!all(i)) {
    stop(paste0("Missing key field in x [", c(id, from, to)[i], "]\n"))
  }
  i <- c(id, from, to) %in% names(y)
  if (!all(i)) {
    stop(paste0("Missing key field in y [", c(id, from, to)[i], "]\n"))
  }

  x <- x[order(x[[id]], x[[from]]), ]
  y <- y[order(y[[id]], y[[from]]), ]

  x$row_x <- 1:nrow(x)
  y$row_y <- 1:nrow(y)

  xx <- x[, c(id, from, to, "row_x")]
  yy <- y[, c(id, from, to, "row_y")]
  max_depths <- sapply(base::split(c(xx[[to]],yy[[to]]), c(xx[[id]], yy[[id]])), max)

  fix_interval <- function(z) {
    hole <- z[[id]][1]
    n <- nrow(z)
    # Add missing top interval.
    if (z[1, from] != 0) {
      zz <- data.frame(hole, 0, z[1, from], 0)
      names(zz) <- names(z)
      z <- rbind(z, zz)
    }
    # Add missing interval.
    if (n > 1) {
      zz <- data.frame(z[2:n, id], z[1:(n-1), to], z[2:n, from], 0)
      names(zz) <- names(z)
      i <- zz$from != zz$to
      if (any(i)) {
        z <- rbind(z, zz[i, ])
      }
    }
    # Add missing bottom interval.
    if (z[n, to] < max_depths[hole]) {
      zz <- data.frame(hole, z[n, to], max_depths[hole], 0)
      names(zz) <- names(z)
      z <- rbind(z, zz)
    }
    z <- z[order(z[[id]], z[[from]]), ]

    return(z)
  }

  xx <- lapply(base::split(xx, xx[[id]]), fix_interval)
  xx <- do.call("rbind", xx)
  row.names(xx) <- NULL

  yy <- lapply(base::split(yy, yy[[id]]), fix_interval)
  yy <- do.call("rbind", yy)
  row.names(yy) <- NULL

  zz1 <- data.frame(id = c(xx[[id]], xx[[id]]),
                    interval = c(xx[[from]], xx[[to]]),
                    row_x = c(xx$row_x, xx$row_x), stringsAsFactors=FALSE)
  zz2 <- data.frame(id = c(yy[[id]], yy[[id]]),
                    interval = c(yy[[from]], yy[[to]]),
                    row_y = c(yy$row_y, yy$row_y), stringsAsFactors=FALSE)

  zz <- dplyr::bind_rows(zz1, zz2)
  zz <- zz[order(zz$id, zz$interval, zz$row_x, zz$row_y), ]
  zz$row_x[1] <- ifelse(is.na(zz$row_x[1]), 0, zz$row_x[1])
  zz$row_y[1] <- ifelse(is.na(zz$row_y[1]), 0, zz$row_y[1])

  isnotblank <- !is.na(zz$row_x)
  ri <- zz$row_x[which(isnotblank)]
  ri <- ri[cumsum(isnotblank)]
  zz$row_x <- ri

  isnotblank <- !is.na(zz$row_y)
  ri <- zz$row_y[which(isnotblank)]
  ri <- ri[cumsum(isnotblank)]
  zz$row_y <- ri

  zz <- lapply(base::split(zz, zz$id), function(r) {
    n <- nrow(r)
    df <- data.frame(id = r$id[1:(n-1)],
                     from = r$interval[1:(n-1)],
                     to = r$interval[2:n],
                     row_x = r$row_x[2:n],
                     row_y = r$row_y[2:n])
  })
  zz <- do.call("rbind", zz)
  zz$length <- zz$to - zz$from
  zz <- zz[zz$length>0, ]
  row.names(zz) <- NULL
  names(zz) <- c(id, from, to, "row_x", "row_y", "length")

  z <- merge(zz, x[, setdiff(names(x), c(id, from, to)), drop=FALSE], by="row_x", all.x=TRUE)
  z <- merge(z, y[, setdiff(names(y), c(id, from, to)), drop=FALSE], by="row_y", all.x=TRUE)
  z <- z[, setdiff(names(z), c("length", "row_x", "row_y"))]
  
  return(z)
}
