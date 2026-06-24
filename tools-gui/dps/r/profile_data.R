profile_data <- function(x, meta) {
  stopifnot(inherits(x, "datafile"))
  if (is.null(meta) || !is.list(meta)) {
    return(NULL)
  }

  df <- load_datafile(x)

  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  if (!is.null(meta$header_rows)) {
    df <- df[-meta$header_rows, ]
  }
  nn <- paste0("V", 1:ncol(df))
  if (!is.null(meta$colnames)) {
    n <- as.character(meta$colnames)
    n <- ifelse(is.na(n) | n =="", nn, n)
    names(df) <- n
  } else {
    names(df) <- nn
  }

  n <- names(df)
  p <- lapply(1:ncol(df), \(i) {
    if (all(is.na(df[[i]]))) {
      z <- list(
        N = nrow(df),
        N_missing = nrow(df),
        field = sub("[.][.][.][[:digit:]]+$","", names(df)[i])
      )
      return(z)
    }

    y <- readr::guess_parser(df[[i]], na=c("", " ", "-"))
    func <- switch(y,
                   character = profile_character,
                   logical = profile_logical,
                   time = profile_time,
                   date = profile_datetime,
                   datetime = profile_datetime,
                   double = profile_numeric,
                   number = profile_numeric)
    z <- func(df[[i]])
    z[["field"]] <- names(df)[i]

    return(z)
  })

  y <- lapply(df, \(x) {
      y <- readr::parse_guess(x, na=c("", " ", "-"))
      if (!is.numeric(y)) {
        y <- as.numeric(factor(y))
      }
      return(y)
    }) |>
    as.data.frame()
  y_cor <- tryCatch({
    cor(y, use="pairwise.complete.obs")
    #y_cor <- collinear::cor_matrix (y)
    #y_cor <- polycor::hetcor(y, use="pairwise.complete.obs")
    p <- lapply(1:ncol(df), \(i) {
      x <- p[[i]]
      if (all(is.na(y_cor[, i]))) {
        return(x)
      }
      
      v <- y_cor[i, -i]
      v <- v[which.max(v)]
      x[["correlated_with"]] <- sub("[.][.][.][[:digit:]]+$","", names(v))
      x[["correlated_with_cor"]] <- as.numeric(v)
      
      return(x)
    })
  }, error = function(e) {
    return(NULL)
  })

  p <- list(
    table = list(
      ncol = ncol(df),
      nrow = nrow(df),
      n_values = ncol(df) * nrow(df),
      n_numeric = sum(sapply(y, is.numeric)),
      n_character = sum(sapply(y, is.character)),
      n_logical = sum(sapply(y, is.logical)),
      n_time = sum(sapply(y, is, "hms")),
      n_date = sum(sapply(y, is, "POSIXt")),
      n_datetime = sum(sapply(y, is, "Date")),
      n_missing = sum(is.na(y)),
      n_duplicate_rows = sum(duplicated(df)),
      mean_cor = ifelse(is.null(y_cor), NA, mean(y_cor, na.rm=TRUE))
    ),
    fields = p
  )

  return(p)
}

profile_numeric <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  z <- value_format(x)
  z <- ifelse(z == "", NA, z)
  zz <- table(z)

  y <- readr::parse_guess(x, na=c("", " ", "-"))
  if (all(is.na(y))) {
    z <- list(
      type = "empty",
      N = length(x),
      N_missing = length(x)
    )
    return(z)
  }

  if (!is.numeric(y) | all(is.na(y))) {
    m <- NA
  } else {
    m <- univariateML::model_select(
      na.omit(y),
      models = c("norm", "lnorm", "exp", "gamma", "power", "pareto",
                 "unif", "weibull", "logis"))
    m <- attr(m, "model")
  }

  p <- list(
    type = readr::guess_parser(x, na=c("", " ", "-")),
    format = names(zz[which.max(zz)])[1],
    N = length(y),
    N_format = as.numeric(zz[which.max(zz)]),
    N_unique_formats = length(unique(z)),
    N_unique = length(unique(y)),
    N_missing = sum(is.na(y)),
    N_duplicate = sum(duplicated(y)),
    N_below0 = sum(y < 0, na.rm=TRUE),
    N_zero = sum(y == 0, na.rm=TRUE),
    N_above100 = sum(y > 100, na.rm=TRUE),
    N_above1000 = sum(y > 1000, na.rm=TRUE),
    N_above10000 = sum(y > 10000, na.rm=TRUE),
    min = min(y, na.rm=TRUE),
    mean = mean(y, na.rm=TRUE),
    max = max(y, na.rm=TRUE),
    sd = sd(y, na.rm=TRUE),
    skewness = moments::skewness(y, na.rm=TRUE),
    kurtosis = moments::kurtosis(y, na.rm=TRUE),
    distribution = m
  )

  return(p)
}

profile_character <- function(x) {
  z <- value_format(x)
  z <- ifelse(z == "", NA, z)
  zz <- table(z)

  if (all(is.na(z))) {
    z <- list(
      type = "empty",
      N = length(x),
      N_missing = length(x)
    )
    return(z)
  }

  p <- list(
    type = "character",
    format = names(zz[which.max(zz)])[1],
    N = length(x),
    N_format = as.numeric(zz[which.max(zz)]),
    N_unique_formats = length(unique(x)),
    N_unique = length(unique(x)),
    N_missing = sum(is.na(x)),
    N_duplicate = sum(duplicated(x))
  )

  return(p)
}

profile_logical <- function(x) {
  p <- profile_character(x)
  p$type <- ifelse(p$type == "character", "logical", p$type)

  return(p)
}

profile_time <- function(x) {
  p <- profile_character(x)
  p$type <- ifelse(p$type == "character", "time", p$type)

  return(p)
}

profile_datetime <- function(x) {
  p <- profile_character(x)
  p$type <- ifelse(p$type == "character", "datetime", p$type)

  return(p)
}
