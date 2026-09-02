#ang2cart(azm=45, dip=75)
#(0.1830127239227295, 0.1830127090215683, -0.9659258127212524)
ang2cart <- function(azm, dip) {
    DEG2RAD <- 3.141592654 / 180.0

    # convert degree to rad and correct sign of dip
    razm <- azm * DEG2RAD
    rdip <- -dip * DEG2RAD

    # do the conversion
    x <- sin(razm) * cos(rdip)
    y <- cos(razm) * cos(rdip)
    z <- sin(rdip)

    return(c(x,y,z))
}

#cart2ang(x=0.18301, y=0.18301, z=-0.96593)
#(45.0, 75.00092315673828)
cart2ang <- function(x, y, z) {
    EPSLON <- 1.0e-4

    if(abs(x)+EPSLON > 1.001 | abs(y)+EPSLON > 1.001 | abs(z)+EPSLON > 1.001) {
        warning("cart2ang: a coordinate x, y or z is outside the interval [-1,1]")
    }

    # check the values are in interval [-1,1] and truncate if necessary
    x <- max(-1, min(1, x))
    y <- max(-1, min(1, y))
    z <- max(-1, min(1, z))

    RAD2DEG <- 180.0 / 3.141592654

    azm <- atan2(x,y)
    azm <- ifelse(azm<0, azm + pi*2, azm)
    azm <- azm * RAD2DEG

    dip <- -asin(z) * RAD2DEG

    return(c(azm, dip))
}

#interp_ang1D(azm1=45, dip1=75, azm2=90, dip2=20, len12=10, d1=5)
# (80.74163055419922, 40.84182357788086)
interp_ang1D <- function(azm1, dip1, azm2, dip2, len12, d1) {
    # convert angles to coordinates
    u <- ang2cart(azm1, dip1)
    x1 <- u[1]
    y1 <- u[2]
    z1 <- u[3]
    u <- ang2cart(azm2, dip2)
    x2 <- u[1]
    y2 <- u[2]
    z2 <- u[3]

    # interpolate x,y,z
    x <- x2 * d1 / len12 + x1 * (len12 - d1) / len12
    y <- y2 * d1 / len12 + y1 * (len12 - d1) / len12
    z <- z2 * d1 / len12 + z1 * (len12 - d1) / len12

    # get back the results as angles
    u <- cart2ang(x, y, z)
    azm <- u[1]
    dip <- u[2]

    return(c(azm, dip))
}

#dsmincurb(len12=10, azm1=45, dip1=75, azm2=90, dip2=20)
#(7.207193374633789, 1.0084573030471802, 6.186459064483643)
dsmincurb <- function(len12, azm1, dip1, azm2, dip2) {
    # Desurveys one interval with minimum curvature
    # The equations were derived from the paper
    # http://www.cgg.com/data//1/rec_docs/2269_MinimumCurvatureWellPaths.pdf

    DEG2RAD <- 3.141592654 / 180.0

    i1 <- (90 - dip1) * DEG2RAD
    a1 <- azm1 * DEG2RAD

    i2 <- (90 - dip2) * DEG2RAD
    a2 <- azm2 * DEG2RAD

    # calculate the dog-leg (dl) and the Ratio Factor (rf)
    dl <- acos(cos(i2-i1) - sin(i1)*sin(i2)*(1 - cos(a2 - a1)))

    if (dl != 0) {
        rf <- 2*tan(dl/2) / dl  # minimum curvature
    } else {
        rf <- 1                 # balanced tangential
    }

    dz <- 0.5 * len12 * (cos(i1) + cos(i2)) * rf
    dn <- 0.5 * len12 * (sin(i1) * cos(a1) + sin(i2) * cos(a2)) * rf
    de <- 0.5 * len12 * (sin(i1) * sin(a1) + sin(i2) * sin(a2)) *rf

    return(c(dz, dn, de))
}

#dstangential(len12=10, azm1=45, dip1=75)
#(9.659257888793945, 1.8301270008087158, 1.8301271200180054)
dstangential <- function(len12, azm1, dip1) {
    # Desurveys one interval with tangential method
    # The equations were derived from the paper
    # http://www.cgg.com/data//1/rec_docs/2269_MinimumCurvatureWellPaths.pdf

    DEG2RAD <- 3.141592654 / 180.0

    i1 <- (90 - dip1) * DEG2RAD
    a1 <- azm1 * DEG2RAD

    dz <- len12 * cos(i1)
    dn <- len12 * sin(i1) * cos(a1)
    de <- len12 * sin(i1) * sin(a1)

    return(c(dz, dn, de))
}

angleson1dh <- function(indbs, indes, ats, azs, dips, lpt, warns=TRUE) {
    EPSLON <- 1.0e-4

    for (i in seq(indbs, indes)) {
        # get the segment [a-b] to test interval
        a <- ats[i]
        b <- ats[i+1]
        azm1 <- azs[i]
        dip1 <- dips[i]
        azm2 <- azs[i+1]
        dip2 <- dips[i+1]
        len12 <- ats[i+1] - ats[i]

        # test if we are in the interval, interpolate angles
        if(lpt>=a & lpt<b) {
            d1 <- lpt- a
            u <- interp_ang1D(azm1, dip1, azm2, dip2, len12, d1)
            azt <- u[1]
            dipt <- u[2]

            return(list(azt, dipt))
        }
    }

    # no interval detected, we must be beyond the last survey!
    a <- ats[indes]
    azt <- azs[indes]
    dipt <- dips[indes]
    # the point is beyond the last survey?
    if(lpt >= a) {
        if(warns) {
            warning(paste("\n point beyond the last survey point at", indes))
        }
        return(list(azt, dipt))
    } else {
        if(warns) {
            warning(paste("\n not interval found at survey, at", indes))
        }
        return(list(nan, nan))
    }
}

desurvey_survey <- function(survey, collar, method=1) {
    survey[["x"]] <- rep(NA, nrow(survey))
    survey[["y"]] <- rep(NA, nrow(survey))
    survey[["z"]] <- rep(NA, nrow(survey))

    o <- order(survey[["BHID"]], survey[["AT"]])
    survey <- survey[o, ]

    for (hole in collar[["BHID"]]) {
        # find collar data for this drillhole
        k <- collar[["BHID"]] == hole
        XC <- collar[k, "XCOLLAR"]
        YC <- collar[k, "YCOLLAR"]
        ZC <- collar[k, "ZCOLLAR"]

        # desurvey survey
        k <- survey[["BHID"]] == hole
        AT <- survey[k, "AT"]
        DIP <- survey[k, "DIP"]
        AZ <- survey[k, "AZ"]

        dz <- rep(NA, length(AT))
        dn <- rep(NA, length(AT))
        de <- rep(NA, length(AT))

        x <- rep(NA, length(AT))
        y <- rep(NA, length(AT))
        z <- rep(NA, length(AT))

        dz[1] <- 0
        dn[1] <- 0
        de[1] <- 0
        x[1] <- XC
        y[1] <- YC
        z[1] <- ZC

        for (i in seq(2, length(AT))) {
            if(method == 1) {
                u <- dsmincurb(len12=AT[i]-AT[i-1], azm1=AZ[i-1], dip1=DIP[i-1], azm2=AZ[i], dip2=DIP[i])
                dz[i] <- u[1]
                dn[i] <- u[2]
                de[i] <- u[3]
            } else {
                u <- dstangential(len12=AT[i]-AT[i-1], azm1=AZ[i-1], dip1=DIP[i-1])
                dz[i] <- u[1]
                dn[i] <- u[2]
                de[i] <- u[3]
            }

            x[i] <- x[i-1] + de[i]
            y[i] <- y[i-1] + dn[i]
            z[i] <- z[i-1] - dz[i]
        }

        k <- survey[["BHID"]] == hole
        survey[k, "x"] <- x
        survey[k, "y"] <- y
        survey[k, "z"] <- z
    }

    return(survey)
}

desurvey <- function(intv, collar, survey, endpoints=TRUE, warns=TRUE, method=1, at_interval=TRUE) {
    if (!all((c("x","y","z") %in% names(survey)))) {
        survey <- desurvey_survey(survey, collar, method)
    }

    bhid <- unique(intv$BHID)

    for (hole in bhid) {
        # get survey
        k <- survey[["BHID"]] == hole
        AT <-  survey[k, "AT"]
        DIP <- survey[k, "DIP"]
        AZ <-  survey[k, "AZ"]
        xs <-  survey[k, "x"]
        ys <-  survey[k, "y"]
        zs <-  survey[k, "z"]

        # get from, to, y mid interval
        k <- intv[["BHID"]] == hole
        db <- intv[k, "FROM"]
        de <- intv[k, "TO"]
        dm <- db + (de - db) / 2

        # add at the end of the survey if de< AT
        if (tail(de, 1) >= tail(AT, 1)) {
            AZ <- c(AZ, tail(AZ, 1))
            DIP <- c(DIP, tail(DIP, 1))
            AT <- c(AT, tail(de, 1) + 0.01)
        }

        #get the index where each interval is located
        jb <- findInterval(db, AT) + 1
        je <- findInterval(de, AT) + 1
        jm <- findInterval(dm, AT) + 1

        # outputs
        azmt <- rep(NA, length(jb))
        dipt <- rep(NA, length(jb))
        x <- rep(NA, length(jb))
        y <- rep(NA, length(jb))
        z <- rep(NA, length(jb))

        if (at_interval) {
            # the start
            intv[["xb"]] <- rep(NA, nrow(intv))
            intv[["yb"]] <- rep(NA, nrow(intv))
            intv[["zb"]] <- rep(NA, nrow(intv))
            intv[["azmb"]] <- rep(NA, nrow(intv))
            intv[["dipb"]] <- rep(NA, nrow(intv))
            for (i in seq(1, length(jb))) {
                d1 <- db[i] - AT[jb[i] - 1]
                lll1 <- AT[jb[i]]
                lll2 <- AT[jb[i] - 1]
                len12 <- lll1 - lll2
                azm1 <- AZ[jb[i] - 1]
                dip1 <- DIP[jb[i] - 1]
                azm2 <- AZ[jb[i]]
                dip2 <- DIP[jb[i]]
                u <- interp_ang1D(azm1, dip1, azm2, dip2, len12, d1)
                azmt[i] <- u[1]
                dipt[i] <- u[2]
                if (method==1) {
                    u <- dsmincurb(d1, azm1,  dip1, azmt[i], dipt[i])
                    dz <- u[1]
                    dy <- u[2]
                    dx <- u[3]
                } else {
                    u <- dstangential(d1, azm1,  dip1)
                    dz <- u[1]
                    dy <- u[2]
                    dx <- u[3]
                }

                x[i] = dx + xs[jb[i] - 1]
                y[i] = dy + ys[jb[i] - 1]
                z[i] = zs[jb[i] - 1] - dz
            }

            k <- intv[["BHID"]] == hole
            intv[k, "azmb"] <- azmt
            intv[k, "dipb"] <- dipt
            intv[k, "xb"] <- x
            intv[k, "yb"] <- y
            intv[k, "zb"] <- z

            # the end
            intv[["xe"]] <- rep(NA, nrow(intv))
            intv[["ye"]] <- rep(NA, nrow(intv))
            intv[["ze"]] <- rep(NA, nrow(intv))
            intv[["azme"]] <- rep(NA, nrow(intv))
            intv[["dipe"]] <- rep(NA, nrow(intv))
            for (i in seq(1, length(je))) {
                d1 <- de[i] - AT[je[i]- 1]
                len12 <- AT[je[i]] - AT[je[i] - 1]
                azm1 <- AZ[je[i] - 1]
                dip1 <- DIP[je[i] - 1]
                azm2 <- AZ[je[i]]
                dip2 <- DIP[je[i]]
                u <- interp_ang1D(azm1, dip1, azm2, dip2, len12, d1)
                azmt[i] <- u[1]
                dipt[i] <- u[2]
                if (method==1) {
                    u <- dsmincurb(d1, azm1,  dip1, azmt[i], dipt[i])
                    dz <- u[1]
                    dy <- u[2]
                    dx <- u[3]
                } else {
                    u <- dstangential(d1, azm1,  dip1)
                    dz <- u[1]
                    dy <- u[2]
                    dx <- u[3]
                }
                x[i] <- dx + xs[je[i] - 1]
                y[i] <- dy + ys[je[i] - 1]
                z[i] <- zs[je[i] - 1] - dz
            }

            k <- intv[["BHID"]] == hole
            intv[k, "azme"] <- azmt
            intv[k, "dipe"] <- dipt
            intv[k, "xe"] <- x
            intv[k, "ye"] <- y
            intv[k, "ze"] <- z
        }

        # the mean
        intv[["xm"]] <- rep(NA, nrow(intv))
        intv[["ym"]] <- rep(NA, nrow(intv))
        intv[["zm"]] <- rep(NA, nrow(intv))
        intv[["azmm"]] <- rep(NA, nrow(intv))
        intv[["dipm"]] <- rep(NA, nrow(intv))
        for (i in seq(1, length(jm))) {
            d1 <- dm[i] - AT[jm[i] - 1]
            len12 <- AT[jm[i]]-AT[jm[i] - 1]
            azm1 <- AZ[jm[i] - 1]
            dip1 <- DIP[jm[i] - 1]
            azm2 <- AZ[jm[i]]
            dip2 <- DIP[jm[i]]
            u <- interp_ang1D(azm1, dip1, azm2, dip2, len12, d1)
            azmt[i] <- u[1]
            dipt[i] <- u[2]
            if (method==1) {
                u <- dsmincurb(d1, azm1,  dip1, azmt[i], dipt[i])
                dz <- u[1]
                dy <- u[2]
                dx <- u[3]
            } else {
                u <- dstangential(d1, azm1,  dip1)
                dz <- u[1]
                dy <- u[2]
                dx <- u[3]
            }

            x[i] <- dx + xs[jm[i] - 1]
            y[i] <- dy + ys[jm[i] - 1]
            z[i] <- zs[jm[i] - 1] - dz
        }

        k <- intv[["BHID"]] == hole
        intv[k, "azmm"] <- azmt
        intv[k, "dipm"] <- dipt
        intv[k, "xm"] <- x
        intv[k, "ym"] <- y
        intv[k, "zm"] <- z
    }

    return(intv)
}

#df_int <- data.frame(BHID="DF01", FROM=c(0,3,12,31), TO=c(3,12,31,34))
#df_collar <- data.frame(BHID="DF01", XCOLLAR=0, YCOLLAR=0, ZCOLLAR=0)
#df_survey <- data.frame(BHID="DF01", AT=c(0,10,20,30), DIP=c(70,73,75,76), AZ=c(270,272,274,276))
#df_int <- desurvey(df_int, df_collar, df_survey, method=1, at_interval=FALSE)
