dqs_extract <- function(dqs, table_name) {
  if (!is(dqs, "dqs")) {
    return(NULL)
  }

  func <- switch(table_name,
         dataset = dataset_to_df,
         datafiles = datafiles_to_df,
         datafields = datafields_to_df,
         profile_table = profile_table_to_df,
         profile_fields = profile_fields_to_df)
  if (is.null(func)) {
    return(NULL)
  }

  func(dqs)
}

dataset_to_df <- function(dqs) {
  if (is.null(dqs$score$table)) {
    return(NULL)
  }

  x <- do.call(cbind, dqs$score$table)
  df <- cbind(dqs$dataset, x)

  return(df)
}

datafiles_to_df <- function(dqs) {
  if (is.null(dqs$datafiles)) {
    return(NULL)
  }

  df <- dqs$datafiles |>
    lapply(as.data.frame) |>
    x => do.call(dplyr::bind_rows, x)
  df$sheet_num <- ifelse(is.na(df$sheet_num), 0, df$sheet_num)
  x <- do.call(cbind, dqs$score$fields) |>
    as.data.frame()
  df <- cbind(df, x)
  df <- cbind(df, dqs$cluster)

  return(df)
}

datafields_to_df <- function(dqs) {
  if (is.null(dqs$metadata)) {
    return(NULL)
  }

  if (all(sapply(dqs$metadata, is.null))) {
    return(NULL)
  }

  df <- lapply(1:length(dqs$metadata), \(i) {
    z <- dqs$metadata[[i]]$colnames
    if (is.null(z)) {
      return(NULL)
    }
    n <- length(z)
    m <- dqs$metadata[[i]]$method
    if (is.null(m)) {
      m <- rep(NA, n)
    }
    u <- dqs$metadata[[i]]$unit
    if (is.null(u)) {
      u <- rep(NA, n)
    }
    l <- dqs$metadata[[i]]$limit
    if (is.null(l)) {
      l <- rep(NA, n)
    }
    if (is(dqs$datafiles[[i]], "xls")) {
      sheet_num <- dqs$datafiles[[i]]@sheet_num
    } else {
      sheet_num = 0
    }
    data.frame(field = z,
               method = m,
               unit = u,
               limit = l,
               id = dqs$datafiles[[i]]@id,
               sheet_num = sheet_num)
  })
  df <- do.call(rbind, df) |>
    dplyr::filter(!is.na(field) & (!is.na(method) | !is.na(unit) | !is.na(limit)))

  return(df)
}

profile_table_to_df <- function(dqs) {
  if (is.null(dqs$profile)) {
    return(NULL)
  }

  if (all(sapply(dqs$profile, is.null))) {
    return(NULL)
  }

  df <- (p <- lapply(dqs$profile, "[[", "table")) |>
    sapply(as.data.frame) |>
    x => do.call(rbind, x) |>
    as.data.frame()

  id <- sapply(dqs$datafiles, \(o) { o@id })
  sheet_num <- sapply(dqs$datafiles, \(o) {
    if (is(o, "xls")) {
      return(o@sheet_num)
    } else {
      return(0)
    }
  })

  df$id <- id[!sapply(p, is.null)]
  df$sheet_num <- sheet_num[!sapply(p, is.null)]

  return(df)
}

profile_fields_to_df <- function(dqs) {
  if (is.null(dqs$profile)) {
    return(NULL)
  }

  df <- lapply(dqs$profile, "[[", "fields")
  if (all(sapply(dqs$profile, is.null))) {
    return(NULL)
  }

  id <- sapply(dqs$datafiles, \(o) { o@id })
  sheet_num <- sapply(dqs$datafiles, \(o) {
    if (is(o, "xls")) {
      return(o@sheet_num)
    } else {
      return(0)
    }
  })
  df <- lapply(1:length(df), \(i) {
    if (is.null(df[[i]])) {
      return(NULL)
    }
    z <- do.call(dplyr::bind_rows, df[[i]])
    z$id <- id[[i]]
    z$sheet_num <- sheet_num[[i]]
    return(z)
  })
  df <- do.call(dplyr::bind_rows, df)

  return(df)
}
