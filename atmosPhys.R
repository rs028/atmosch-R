### ---------------------------------------------------------------- ###
### functions for atmospheric physics:
### - fHumid() : humidity measurements
### - fSolar() : Earth-Sun angles
###
### version 1.5, Mar 2017
### author: RS
### ---------------------------------------------------------------- ###

fHumid <- function(data.in, meas.in, meas.out, temp, press=101325) {
  ## convert between measurements of humidity at given temperature:
  ## * absolute humidity = "AH"
  ##   mass of water vapour per volume air (g/m3)
  ## * specific humidity = "SH"
  ##   mass of water vapour per mass air (g/kg)
  ## * mixing ratio      = "MR"
  ##   mass of water vapour per mass dry air (g/kg)
  ## * relative humidity = "RH"
  ##   water vapour pressure to water vapour saturation pressure (%)
  ## * parts per million = "PPM"
  ##   volume of water vapour per volume of dry air (ppm)
  ##
  ## from "Humidity Conversion Formulas" published by Vaisala
  ## (http://www.vaisala.com/)
  ##
  ## NB: pressure is required only for conversions to/from "PPM"
  ##
  ## input:
  ##     data.in = original humidity data
  ##     meas.in = original measurement
  ##     meas.out = final measurement
  ##     temp = temperature (K)
  ##     press = pressure (Pa)     [ OPTIONAL, DEFAULT = 1 atm ]
  ## output:
  ##     data.out = final humidity data
  ## ------------------------------------------------------------
  ## convert temperature and pressure units
  temp.c <- fConvTemp(temp, "K", "C")
  press.h <- fConvPress(press, "Pa", "hPa")
  ## water vapour saturation pressure (hPa)
  pws <- 6.116441 * 10^( (7.591386 * temp.c) / (temp.c + 240.7263) )
  ## water vapour pressure (hPa) from original humidity data
  switch(meas.in,
         "AH" = {
           pw <- 1.0e-02 * (data.in * temp) / 2.16679
         },
         "SH" = {
           pw <- 1.0e-03 * (data.in * press.h) / 0.622
         },
         "MR" = {
           pw <- (data.in * press.h) / (621.9907 + data.in)
         },
         "RH" = {
           pw <- data.in * pws / 100
         },
         "PPM" = {
           pw <- (data.in * 1.0e-06 * press.h) / (1 + data.in * 1.0e-06)
         },
         stop("INPUT ERROR: unit not found")
         )
  ## final humidity data from water vapour pressure (hPa)
  switch(meas.out,
         "AH" = {
           data.out <- 1.0e+02 * (pw * 2.16679) / temp
         },
         "SH" = {
           data.out <- 1.0e+03 * (pw * 0.622) / press.h
         },
         "MR" = {
           data.out <- (621.9907 * pw) / (press.h - pw)
         },
         "RH" = {
           data.out <- 100 * pw / pws
         },
         "PPM" = {
           data.out <- 1.0e+06 * pw / (press.h - pw)
         },
         stop("INPUT ERROR: unit not found")
  )
  ## final humidity data
  return(data.out)
}

fSolar <- function(lat, long, dt.chron) {
  ## calculate Earth-Sun angles (in radians):
  ## * sun declination = "DEC"
  ##   angle between center of the Sun and Earth's equatorial plane
  ## * local hour angle = "LHA"
  ##   angle between observer's meridian and Sun's meridian
  ## * solar zenith angle = "SZA"
  ##   angle between local vertical and center of the Sun
  ## * solar elevation angle = "SEA"
  ##   angle between local horizontal and center of the Sun
  ##
  ## from "The Atmosphere and UV-B Radiation at Ground Level" by
  ## S. Madronich (Environmental UV Photobiology, 1993)
  ##
  ## input:
  ##     lat = latitude (degrees)
  ##     long = longitude (degrees)
  ##     dt.chron = chron variable ("d-m-y h:m:s", GMT/UTC)
  ## output:
  ##     df.out = data.frame ( DEC = sun declination,
  ##                           LHA = local hour angle,
  ##                           SZA = solar zenith angle,
  ##                           SEA = solar elevation angle )
  ## ---------------------------------------------------------------------
  ## latitude and longitude in radians
  lat.r <- fConvAngle(lat, "deg", "rad")
  long.r <- fConvAngle(long, "deg", "rad")
  ## day of year (1 Jan = 1) and fractional time
  jan1 <- chron(paste("01/01/", years(dt.chron), sep=""))
  fracd <- as.numeric(dt.chron - jan1 + 1)
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
  ## local hour angle
  c0 <-  0.000075
  c1 <-  0.001868
  c2 <- -0.032077
  c3 <- -0.014615
  c4 <- -0.040849
  eqt <- c0 + c1 * cos(theta) + c2 * sin(theta) + c3 * cos(2 * theta) +
         c4 * sin(2 * theta)
  lha <- pi * (gmt / 12 - (1 + long / 180)) + eqt
  ## solar zenith angle and solar elevation angle
  sza <- acos(sin(dec) * sin(lat.r) + cos(dec) * cos(lat.r) * cos(lha))
  sea <- pi/2 - sza
  ## output data.frame
  df.out <- data.frame(dt.chron, dec, lha, sza, sea)
  colnames(df.out) <- c("GMT", "DEC", "LHA", "SZA", "SEA")
  return(df.out)
}
