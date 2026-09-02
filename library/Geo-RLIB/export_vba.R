locf <- function(x, blank=is.na) {
  # Find the values
  if (is.function(blank)) {
    isnotblank <- !blank(x)
  } else {
    isnotblank <- x != blank 
  }
  # Fill down
  y <- x[which(isnotblank)][cumsum(isnotblank)]
  
  return(y)
}

write_avg_func <- function(con, na.value="-3.402823E38") {
  txt <- list()
  txt[1] <- "Function Avg(x() As Single) As Double"
  txt[2] <- "\tDim y As Double"
  txt[3] <- "\tDim n As Integer"
  txt[4] <- "\tFor i = 1 To UBound(x)"
  txt[5] <- paste("\tIf x(i) >", na.value, "Then")
  txt[6] <- "\t\ty = y + x(i)"
  txt[7] <- "\t\tn = n + 1"
  txt[8] <- "\tEnd If"
  txt[9] <- "\tNext"
  txt[10] <- "\ty = y / n"
  txt[11] <- "\tAvg = y"
  txt[12] <- "End Function"
  
  lapply(txt, writeLines, con)
  writeLines("", con)
}

write_minmax_func <- function(con) {
  txt <- list()
  txt[1] <- "Function MinMax(x As Single, min As Single, max As Single, Optional f As Single = 1) As Single"
  txt[2] <- "\tDim r As Single"
  txt[3] <- "\tDim ub As Single"
  txt[4] <- "\tDim lb As Single"
  txt[5] <- "\tr = f * (max - min)"
  txt[6] <- "\tub = max + r"
  txt[7] <- "\tIf ub > 100 Then"
  txt[8] <- "\t\tub = 100"
  txt[9] <- "\tEnd If"
  txt[10] <- "\tlb = min - r"
  txt[11] <- "\tIf lb < 0 Then"
  txt[12] <- "\t\tlb = 0"
  txt[13] <- "\tEnd If"
  txt[14] <- "\tMinMax = x"
  txt[15] <- "\tIf x < lb Then"
  txt[16] <- "\t\tMinMax = lb"
  txt[17] <- "\tEnd If"
  txt[18] <- "\tIf x > ub Then"
  txt[19] <- "\t\tMinMax = ub"
  txt[20] <- "\tEnd If"
  txt[21] <- "End Function"

  lapply(txt, writeLines, con)
  writeLines("", con)
}

export_vba <- function(model, ...) UseMethod('export_vba')

export_vba.default <- function(model)
{
}

export_vba.list <- function(model, con)
{
  con <- file(con, open="wt")

  write_minmax_func(con)
  write_avg_func(con)

  for (i in 1:length(model)) {
    export_vba(model[[i]], names(model)[i], con)
  }

  close(con)
}

export_vba.rpart <- function(model, funcname, con=stdout()) {
  if (is.character(con)) {
    con <- file(con, open="wt")
    write_minmax_func(con)
    write_avg_func(con)
  }

  txt <- capture.output(print(model))
  txt <- txt[grepl("^ *[[:digit:]]*[)]", txt)]
  lvl <- unlist(lapply(txt, function(x) {nchar(x) - nchar(gsub("^[[:space:]]*","", x))})) / 2
  txt <- gsub("[[:space:]]+", " ", txt)
  txt <- gsub("^[[:space:]]+", "", txt)
  txt <- gsub("([<>=]) ", "\\1", txt)
  vars <- caret::predictors(model)
  i <- sapply(vars, function(x) any(grepl(x, txt)))
  vars <- vars[i]
  type <- sapply(vars, function(x) any(grepl(paste0(x,"[><=]+[-]?[[:digit:]]"), txt)))
  x <- paste0("Function ", funcname, "(")
  for (i in 1:length(vars)) {
    if (i>1) {
      x <- paste0(x, ", ")
    }
    x <- paste0(x, "head_", vars[i], ifelse(type[i], " As Double", " As String"))
  }
  x <- paste0(x, ") As String")
  writeLines(x, con)

  writeLines(paste0("\tDim class As String"), con)

  for (j in 1:length(txt)) {
    node <- txt[j]
    tab <- paste(rep("\t", lvl[j]), collapse="")
    z <- unlist(strsplit(node, " "))

    if (z[2] == "root") {
      writeLines(paste0("\tclass = \"", z[5], "\""), con)
    } else {
      writeLines(paste0(tab, "If head_", z[2], " Then"), con)
      if (j < length(txt) & lvl[j] < lvl[j+1]) {
        writeLines(paste0(tab, "\tclass = \"", z[5], "\""), con)
      } else {
        writeLines(paste0(tab, "\tclass = \"", z[5], "\""), con)
        writeLines(paste0(tab, "End If"), con)
      }
      if ((j < length(txt) & lvl[j] > lvl[j+1]) | (j == length(txt) & lvl[j] > 1)) {
        tab <- paste(rep("\t", lvl[j+1]), collapse="")
        writeLines(paste0(tab, "End If"), con)
      }
    }
  }

  writeLines(paste0("\t", funcname, " = class"), con=con)
  writeLines("End Function", con=con)
  writeLines("", con=con)

  if (is.character(con)) {
    close(con)
  }
}

export_vba.cubist <- function(model, funcname, con=stdout(), na.value="-3.402823E38") {
  if (is.character(con)) {
    con <- file(con, open="wt")
    write_minmax_func(con)
    write_avg_func(con)
  }

  txt <- model$model
  txt <- gsub("([[:alpha:]])[.]", "\\1_", txt)
  txt <- unlist(strsplit(gsub("\\\"", "", txt), "\n"))

  x <- unlist(strsplit(txt[2], " "))
  eval(parse(text=x))

  txt <- txt[-(1:3)]

  x <- paste0("Function ", funcname, "(")
  z <- txt[grepl("^att", txt)]
  z <- strsplit(z, " ")
  FIRST <- TRUE
  for (i in z) {
    if (grepl("weight", i[2])) {
      next()
    }
    if (FIRST) {
      FIRST <- FALSE
    } else {
      x <- paste0(x, ", ")
    }
    if (grepl("mode", i[2])) {
      x <- paste0(x, gsub("[.]", "_", gsub("att=", "head_", i[1])), " As String")
    }
    if (grepl("mean", i[2])) {
      x <- paste0(x, gsub("[.]", "_", gsub("att=", "head_", i[1])), " As Single")
    }
  }
  x <- paste0(x, ") As Double")
  writeLines(x, con=con)

  txt <- txt[!grepl("^att", txt)]
  txt <- txt[!grepl("^redn", txt)]

  z <- txt[grepl("^rules", txt)]
  z <- gsub("rules=", "", z)
  n.rules <- sum(as.numeric(z))

  writeLines("\tDim y As Single", con=con)
  writeLines(paste0("\tDim u(1 To ", n.rules, ") As Single"), con=con)
  writeLines(paste("For i = 1 To", n.rules))
  writeLines(paste("u(i) =", na.value))
  writeLines("Next")
  writeLines("", con=con)

  indx <- rep(NA, length(txt))
  i <- grepl("^rules", txt)
  indx[i] <- 1:sum(i)
  indx <- locf(indx)
  z <- split(txt, indx)
  i <- 0
  k <- 0
  FIRST <- TRUE
  for (m in z) {
    if (FIRST) {
      FIRST <- FALSE
    } else {
      writeLines("", con=con)
    }
    i <- i + 1
    writeLines(paste("\t'Model", i), con=con)
    j <- 0
    SECOND <- FALSE
    HAS.COND <- FALSE
    x <- ""
    for (r in m) {
      if (grepl("^conds", r)) {
        j <- j + 1
        eval(parse(text=unlist(strsplit(r, " "))))
        HAS.COND <- ifelse(conds > 0, TRUE, FALSE)
        if (HAS.COND) {
          x <- "\tIf "
        }
        writeLines(paste0("\t'Rule ", i, "/", j, ": [", gsub("^conds=[[:digit:]]* ","",r), "]"), con=con)
      }
      if (grepl("^type", r)) {
        if (!SECOND) {
          SECOND <- TRUE
        } else {
          x <- paste(x, "And ")
        }
        if (grepl("^type=2", r)) {
          y <- unlist(strsplit(r, " "))
          x <- paste0(x, gsub("att=","head_", y[grepl("att",y)]))
          x <- paste(x, gsub("result=","", y[grepl("result",y)]))
          x <- paste(x, gsub("cut=","", y[grepl("cut",y)]))
        }
        if (grepl("^type=3", r)) {
          y <- unlist(strsplit(r, " "))
          att <- paste0(gsub("att=", "head_", y[grepl("att",y)]), "=\"")
          z <- gsub("elts=",  att, y[grepl("elts",y)])
          z <- gsub(",", paste("\" Or ", att), z)
          x <- paste0(x, " (", z, ")")
        }
      }
      if (grepl("^coef", r)) {
        k <- k + 1
        y <- unlist(strsplit(r, " "))
        y <- gsub("att=", "* head_", gsub("coeff=", "+ ", y))
        y <- paste(rev(y), collapse=" ")
        y <- gsub("^[+] ", "", y)
        y <- gsub("[+] [-]", "- ", y)
        y <- paste("y = ", y)

        if (HAS.COND) {
          x <- paste(x, "Then")
          writeLines(x, con=con)
          y <- paste0("\t\t", y)
          writeLines(y, con=con)
          writeLines(paste0("\t\ty = MinMax(y, ", loval, ", ", hival, ", ", extrap, ")"), con=con)
          writeLines(paste0("\t\tu(", k, ") = y"), con=con)
          writeLines("\tEnd If", con=con)
        } else {
          y <- paste0("\t", y)
          writeLines(y, con=con)
          writeLines(paste0("\ty = MinMax(y, ", loval, ", ", hival, ", ", extrap, ")"), con=con)
          writeLines(paste0("\tu(", k, ") = y"), con=con)
        }
        SECOND <- FALSE
      }
    }
  }
  writeLines("", con=con)

  floor <- ifelse(floor < 0, 0, floor)
  ceiling <- ifelse(ceiling > 100, 100, ceiling)
  writeLines(paste0("\t", funcname, " = MinMax(Avg(u), ", floor, ", ", ceiling, ", ", extrap, ")"), con=con)
  writeLines("End Function", con=con)
  writeLines("", con=con)

  if (is.character(con)) {
    close(con)
  }
}

x <- readRDS("219054__C__model__2019.rds")
y <- list(oretype=x$oretype)
y <- append(y, x$RO)
y <- append(y, x$PO)
names(y) <- c("oretype", paste0("ro_", names(x$RO)), paste0("po_", names(x$PO)))
export_vba(y, "test.vba")
#export_vba(x$oretype, "oretype")
