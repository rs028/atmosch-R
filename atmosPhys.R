### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### Functions for atmospheric physics:
### - fSolar() : Earth-Sun angles
###
### version 1.9, Oct 2020
### author: RS
### ---------------------------------------------------------------- ###

fSolar <- function(lat, long, dt.chron) {
  ## Calculate Earth-Sun angles (in radians):
  ## * day angle = "THETA"
  ## * sun declination = "DEC"
  ##   angle between center of the Sun and Earth's equatorial plane
  ## * equation of time = "EQT"
  ## * local hour angle = "LHA"
  ##   angle between observer's meridian and Sun's meridian
  ## * solar zenith angle = "SZA"
  ##   angle between local vertical and center of the Sun
  ## * solar elevation angle = "SEA"
  ##   angle between local horizontal and center of the Sun
  ##
  ## [ from "The Atmosphere and UV-B Radiation at Ground Level" by
  ##   S. Madronich (Environmental UV Photobiology, 1993) ]
  ##
  ## input:
  ##     lat = latitude (degrees)
  ##     long = longitude (degrees)
  ##     dt.chron = chron variable ("d-m-y h:m:s", GMT/UTC)
  ## output:
  ##     df.out = data.frame ( GMT = fractional time,
  ##                           THETA = day angle,
  ##                           DEC = sun declination,
  ##                           EQT = equation of time,
  ##                           LHA = local hour angle,
  ##                           SZA = solar zenith angle,
  ##                           SEA = solar elevation angle,
  ##                           LAT = latitude,
  ##                           LONG = longitude )
  ## ---------------------------------------------------------------------
  if (is.data.frame(dt.chron)) {
    datet <- dt.chron[[1]]
  } else {
    datet <- dt.chron
  }
  ## latitude and longitude in radians
  lat.r <- fConvAngle(lat, "deg", "rad")
  long.r <- fConvAngle(long, "deg", "rad")
  ## day of year (1 Jan = 1) and fractional time
  jan1 <- chron(paste("01/01/", years(datet), sep=""))
  fracd <- as.numeric(datet - jan1 + 1)
  doy <- floor(fracd)
  gmt <- (fracd - doy) * 24
  ## day angle
  nday <- doy - 1
  theta <- 2 * pi * (nday / 365)
  ## sun declination
  b0 <-  0.006918
  b1 <- -0.399912
  b2 <-  0.070257
  b3 <- -0.006758
  b4 <-  0.000907
  b5 <- -0.002697
  b6 <-  0.001480
  dec <- b0 + b1 * cos(theta) + b2 * sin(theta) + b3 * cos(2 * theta) +
         b4 * sin(2 * theta) + b5 * cos(3 * theta) + b6 * sin(3 * theta)
  ## equation of time, local hour angle
  c0 <-  0.000075
  c1 <-  0.001868
  c2 <- -0.032077
  c3 <- -0.014615
  c4 <- -0.040849
  eqt <- c0 + c1 * cos(theta) + c2 * sin(theta) + c3 * cos(2 * theta) +
         c4 * sin(2 * theta)
  lha <- pi * (gmt / 12 - (1 + long / 180)) + eqt
  ## solar zenith angle, solar elevation angle
  sza <- acos(sin(dec) * sin(lat.r) + cos(dec) * cos(lat.r) * cos(lha))
  sea <- pi/2 - sza
  ## output data.frame
  df.out <- data.frame(datet, theta, dec, eqt, lha, sza, sea, lat.r, long.r)
  colnames(df.out) <- c("GMT", "THETA", "DEC", "EQT", "LHA", "SZA", "SEA", "LAT", "LONG")
  return(df.out)
}
