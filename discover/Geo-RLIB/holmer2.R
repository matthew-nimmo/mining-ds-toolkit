holmer2 <- function(x, y, id="bhid", from="from", to="to", decimals=NA) {
  i <- c(id, from, to) %in% names(x)
  if (!all(i)) {
    stop(paste0("Missing key field in x [", c(id, from, to)[i], "]\n"))
  }
  i <- c(id, from, to) %in% names(y)
  if (!all(i)) {
    stop(paste0("Missing key field in y [", c(id, from, to)[i], "]\n"))
  }

  max_level <- function(x) {
    y <- table(x, useNA="ifany")
    y[is.na(names(y))] <- 0
    names(which.max(y))
  }

  wsum <- function(x, w) {
    if (all(is.na(x))) {
      return(NA)
    } else {
      y <- sum(x * w, na.rm=TRUE) / sum(w, na.rm=TRUE)
      if (!is.na(decimals)) {
        y <- round(y, decimals)
      }
      return(y)
    }
  }

  x <- x[order(x[[id]], x[[from]]), ]
  y <- y[order(y[[id]], y[[from]]), ]

  i <- intersect(unique(x[[id]]), unique(y[[id]]))
  if (length(i) > 1) {
    x$samp_ <- 1:nrow(x)
    z <- holmer(x[, c(id, from, to, "samp_")], y, id=id, from=from, to=to)
    z <- z[!is.na(z[["samp_"]]), ]
    z <- z[order(z[["samp_"]]), ]
    z$length <- z[[to]] - z[[from]]

    v <- z %>%
      dplyr::filter(length > 0) %>%
      dplyr::select(-all_of(c(id, from, to))) %>%
      dplyr::group_by(samp_) %>%
      dplyr::summarise(
        across(where(is.numeric) & !c(length), ~ wsum(.x, .data[["length"]])),
        across(where(is.character), ~ max_level(.x))) %>%
      dplyr::ungroup()

    x <- dplyr::left_join(x, v, by="samp_") %>%
      dplyr::select(-samp_)
  }

  return(x)
}

test <- function() {
  x <- data.frame(
    BHID = c(rep("A", 2), rep("B",3)),
    FROM = c(2,7, 1,3,4),
    TO = c(7,7.1, 3,4,4.1),
    lab = c("a","a","e","c","d")
  )
  x$LENGTH <- x$TO - x$FROM

  y <- data.frame(
    BHID = c(rep("A",3), rep("B",3)),
    FROM = c(1.5,2.5,3.5, 2.2,2.8,4.1),
    TO = c(2.5,3.5,4, 2.8,4.1,4.3),
    lab2 = c("z","z","x", "y","u","u"),
    lith = c("a","bc","aa", "c", "cc","c"),
    au = c(0.1, 0.9, 3.4, 2.2, 1.7, NA)
  )
  y$LENGTH <- y$TO - y$FROM

  x <- holmer2(x, y, id="BHID", from="FROM", to="TO", length="LENGTH")
  return(x)
}
