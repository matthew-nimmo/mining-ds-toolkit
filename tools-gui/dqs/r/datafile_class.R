datafile_class <- function(meta) {
  N <- length(meta)
  x <- lapply(1:N, \(i) {
    m <- meta[[i]]
    if (is.null(m)) {
      return("unkwn")
    }
    y <- label_from_colnames(m$colnames)

    return(y)
  })

  return(x)
}

label_from_colnames <- function(h) {
  if (is.null(h)) {
    return("unkwn")
  }

  lut_pt_oxide <- c("Al2O3","BaO","CaO","CoO","Cr2O3","CuO","Fe2O3","K2O",
                    "MgO","MnO","Na2O","NiO","P2O5","PbO","SO3","SiO2",
                    "TiO2","ZnO","LOI")
  lut_pt_major <- c("O","Na","Mg","Al","Si","K","Ca","Fe")
  lut_pt_minor <- c("H","C","F","P","S","Cl","V","Cr","Mn","Sr","Zr","Ba")
  lut_pt_trace <- c("He","Li","Be","B","N","Ne","Ar","Ti","Co","Ni","Cu",
                    "Zn","Ga","Ge","As","Se","Br","Kr","Rb","Nb","Mo",
                    "Tc","Ru","Rh","Pd","Ag","Cd","In","Sn","Sb","Te",
                    "I","Xe","Cs","Hf","Ta","W","Re","Os","Ir","Pt","Au",
                    "Hg","Tl","Pb","Bi","Po","At","Rn","Fr","Ra","Ac","Th",
                    "Pa","U","Np","Pu","Am","Cm","Bk","Cf","Es","Fm","Md",
                    "No","Lr","Rf","Db","Sg","Bh","Hs","Mt","Ds","Rg","Cn")
  lut_pt_ree <- c("Sc","Y","La","Ce","Pr","Nd","Pm","Sm","Eu","Gd","Tb",
                  "Dy","Ho","Er","Tm","Yb","Lu")
  lut_assays <- c(lut_pt_oxide,lut_pt_major,lut_pt_minor,lut_pt_trace,lut_pt_ree)

  h <- sapply(h, \(s) {
    s <- tolower(s)
    s <- gsub(" ", "_", s)
    s <- gsub("[)(.]|[[:digit:]]", "", s)
    s
  })

  hh <- sapply(h, \(s) {
    s <- gsub("_m$", "", s)
    s <- gsub("hole", "bhid", s)
    s <- gsub("depth", "at", s)
    s
  })

  if ("bhid" %in% hh) {
    if (any(c("utm","x","y","xp","yp","xpt","ypt","easting","northing","lat",
              "lon","long","latitude","longitude") %in% hh)) {
      return("dh:collar")
    }
    if (all(c("bhid","from","to") %in% hh)) {
      ftype <- "dh:interval"
      if (any(c("lith","lithology","rocktype","rock") %in% hh)) {
        ftype <- paste(ftype, "lith", sep="+")
      }
      if (any(c("altn","alteration") %in% hh)) {
        ftype <- paste(ftype, "altn", sep="+")
      }
      if (any(c("bulkdensity","density","sg") %in% hh)) {
        ftype <- paste(ftype, "sg", sep="+")
      }
      if (sum(tolower(lut_assays) %in% hh) > 1) {
        ftype <- paste(ftype, "assay", sep="+")
      }
      if (any(c("dip","azm","azimuth") %in% hh)) {
        ftype <- paste(ftype, "surv", sep="+")
      }
      return(ftype)
    }
    if (any(c("at","depth") %in% hh)) {
      if (any(c("bulkdensity","density","sg") %in% hh)) {
        return("dh:dens")
      }
      if (any(c("dip","azm","azimuth") %in% hh)) {
        return("dh:surv")
      }
      return("dh:point")
    }
    return("dh:samp")
  }

  hh <- sapply(h, \(s) {
    s <- gsub("gpt|ppm|pct|kg", "", s)
    s <- gsub("_$", "", s)
    s
  })

  if (any(c("sample") %in% hh)) {
    if (any(tolower(lut_pt_oxide) %in% hh)) {
      return("lab:oxide")
    }
    if (any(tolower(lut_pt_major) %in% hh)) {
      return("lab:major")
    }
    if (any(tolower(lut_pt_minor) %in% hh)) {
      return("lab:minor")
    }
    if (any(tolower(lut_pt_trace) %in% hh)) {
      return("lab:trace")
    }
    if (any(tolower(lut_pt_ree) %in% hh)) {
      return("lab:ree")
    }
    return("lab:result")
  }

  hh <- sapply(h, \(s) {
    s <- gsub("id|dd|gpt|ppm|pct", "", s)
    s <- gsub("(.*_)(x|y)$", "\\2", s)
    s <- gsub("_$", "", s)
    s
  })

  if (any(c("utm","x","y","xp","yp","xpt","ypt","easting","northing","lat",
            "lon","long","latitude","longitude") %in% hh)) {
    if (all(c("pvalue","ptn") %in% hh)) {
      return("loc:polygon")
    }
    if ("photo" %in% hh) {
      return("loc:photo")
    }
    if (sum(tolower(lut_assays) %in% hh) > 1) {
      return("loc:assay")
    }
    return("loc:point")
  }

  return("unkwn")
}