
K0 <- 0.9996

E <- 0.00669438
E2 <- E * E
E3 <- E2 * E
E_P2 <- E / (1.0 - E)
SQRT_E <- sqrt(1 - E)
E_b <- (1 - SQRT_E) / (1 + SQRT_E)
E2_b <- E_b * E_b
E3_b <- E2_b * E_b
E4_b <- E3_b * E_b
E5_b <- E4_b * E_b

M1 <- (1 - E/4 - 3* E2/64 - 5*E3/256)
M2 <- (3*E / 8 + 3*E2/32 + 45*E3/1024)
M3 <- (15*E2/256 + 45*E3/1024)
M4 <- (35*E3/3072)

P2 <- (3.0/2*E_b - 27.0/32*E3_b + 269.0/512*E5_b)
P3 <- (21.0/16*E2_b - 55.0/32*E4_b)
P4 <- (151.0/96*E3_b - 417.0/128*E5_b)
P5 <- (1097.0/512*E4_b)

R <- 6378137

ZONE_LETTERS <- list(c(84, NULL),c(72, 'X'),c(64, 'W'),c(56, 'V'),c(48, 'U'),
                  c(40, 'T'),c(32, 'S'),c(24, 'R'),c(16, 'Q'),c(8, 'P'),
                  c(0, 'N'),c(-8, 'M'),c(-16, 'L'),c(-24, 'K'),c(-32, 'J'),
                  c(-40, 'H'),c(-48, 'G'),c(-56, 'F'),c(-64, 'E'),
                  c(-72, 'D'),c(-80, 'C'))


to_latlon <- function(easting, northing, zone_number, zone_letter=NA, northern=NULL)
{
	#if (!zone_letter and northern is None:
        #	raise ValueError('either zone_letter or northern needs to be set')
	#elif zone_letter and northern is not None:
	#	raise ValueError('set either zone_letter or northern, but not both')

	#if not 100000 <= easting < 1000000:
	#	raise OutOfRangeError('easting out of range (must be between 100.000 m and 999.999 m)')
	#if not 0 <= northing <= 10000000:
	#	raise OutOfRangeError('northing out of range (must be between 0 m and 10.000.000 m)')
	#if not 1 <= zone_number <= 60:
	#	raise OutOfRangeError('zone number out of range (must be between 1 and 60)')

	#if zone_letter:
	#	zone_letter = zone_letter.upper()

	#if not 'C' <= zone_letter <= 'X' or zone_letter in ['I', 'O']:
	#	raise OutOfRangeError('zone letter out of range (must be between C and X)')

	if (!is.na(zone_letter))
		northern <- (zone_letter >= "N")
	x <- easting - 500000
	y <- northing

	if (!northern)
		y <- y - 10000000

	m <- y / K0
	mu <- m / (R * M1)

	p_rad <- (mu + P2*sin(2*mu) + P3*sin(4*mu) + P4*sin(6*mu) + P5*sin(8*mu))

	p_sin <- sin(p_rad)
	p_sin2 <- p_sin * p_sin

	p_cos = cos(p_rad)

	p_tan <- p_sin / p_cos
	p_tan2 <- p_tan * p_tan
	p_tan4 <- p_tan2 * p_tan2

	ep_sin <- 1 - E * p_sin2
	ep_sin_sqrt <- sqrt(1 - E*p_sin2)

	n <- R / ep_sin_sqrt
	r <- (1 - E) / ep_sin

	c <- E_b * p_cos^2
	c2 <- c * c

	d <- x / (n * K0)
	d2 <- d * d
	d3 <- d2 * d
	d4 <- d3 * d
	d5 <- d4 * d
	d6 <- d5 * d

	latitude <- (p_rad - (p_tan/r) * (d2/2 -
		d4/24 * (5 + 3*p_tan2 + 10*c - 4*c2 - 9*E_P2)) +
		d6/720 * (61 + 90*p_tan2 + 298*c + 45*p_tan4 - 252*E_P2 - 3*c2))
	latitude <- latitude * 180/pi

	longitude <- (d - d3/6 * (1 + 2*p_tan2 + c) +
		d5/120 * (5 - 2*c + 28*p_tan2 - 3*c2 + 8*E_P2 + 24*p_tan4)) / p_cos
	longitude <- longitude * 180/pi + zone_number_to_central_longitude(zone_number)

	return (c(latitude, longitude))
}


from_latlon <- function(latitude, longitude, force_zone_number=NULL)
{
	#if not -80.0 <= latitude <= 84.0:
	#	raise OutOfRangeError('latitude out of range (must be between 80 deg S and 84 deg N)')
	#if not -180.0 <= longitude <= 180.0:
        #	raise OutOfRangeError('northing out of range (must be between 180 deg W and 180 deg E)')

	lat_rad <- latitude * pi/180
	lat_sin <- sin(lat_rad)
	lat_cos <- cos(lat_rad)

	lat_tan <- lat_sin / lat_cos
	lat_tan2 <- lat_tan * lat_tan
	lat_tan4 <- lat_tan2 * lat_tan2

	if (is.null(force_zone_number))
		zone_number <- latlon_to_zone_number(latitude, longitude)
	else
		zone_number <- force_zone_number

	zone_letter <- latitude_to_zone_letter(latitude)

	lon_rad <- longitude * pi /180
	central_lon <- zone_number_to_central_longitude(zone_number)
	central_lon_rad <- central_lon * pi/180

	n <- R / sqrt(1 - E * lat_sin^2)
	c <- E_P2 * lat_cos^2

	a <- lat_cos * (lon_rad - central_lon_rad)
	a2 <- a * a
	a3 <- a2 * a
	a4 <- a3 * a
	a5 <- a4 * a
	a6 <- a5 * a

	m <- R * (M1*lat_rad - M2*sin(2*lat_rad) + M3*sin(4*lat_rad) - M4*sin(6*lat_rad))

	easting <- K0 * n * (a + a3/6 * (1 - lat_tan2 + c) +
		a5/120 * (5 - 18*lat_tan2 + lat_tan4 + 72*c - 58*E_P2)) + 500000

	northing <- K0 * (m + n*lat_tan*(a2 / 2 +
		a4/24 * (5 - lat_tan2 + 9*c + 4*c^2) +
		a6/720 * (61 - 58*lat_tan2 + lat_tan4 + 600*c - 330*E_P2)))

	if (latitude < 0)
		northing = northing + 10000000

	return (list(x=easting, y=northing, zonen=zone_number, zonel=zone_letter))
}

latitude_to_zone_letter <- function(latitude)
{
    for (i in ZONE_LETTERS)
        if (latitude >= i[[1]])
            return (i[[2]])

    return (NULL)
}

latlon_to_zone_number <- function(latitude, longitude)
{
	if (56 <= latitude & latitude <= 64
      & 3 <= longitude & latitude <= 12)
		return (32)

	if (72 <= latitude & latitude <= 84
      & longitude >= 0) {
		if (longitude <= 9)
			return (31)
		else if (longitude <= 21)
			return (33)
		else if(longitude <= 33)
			return (35)
		else if(longitude <= 42)
			return (37)
	}

	return (as.integer((longitude + 180) / 6) + 1)
}

zone_number_to_central_longitude <- function(zone_number)
{
    return ((zone_number - 1) * 6 - 180 + 3)
}

