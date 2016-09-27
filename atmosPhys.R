### ---------------------------------------------------------------- ###
### functions for atmospheric physics:
### - fHumid() : humidity measurements
###
### version 1.0, Feb 2016
### author: RS
### ---------------------------------------------------------------- ###

fHumid <- function(data.in, meas.in, meas.out, temp, press) {
  ## convert between measurements of humidity at given temperature and
  ## pressure
  ## from "Humidity Conversion Formulas" published by Vaisala:
  ##    http://www.vaisala.com/
  ##
  ## - absolute humidity : ("AH")
  ##   mass of water vapour per volume air (g/m3)
  ## - specific humidity : ("SH")
  ##   mass of water vapour per mass air (g/kg)
  ## - mixing ratio      : ("MR")
  ##   mass of water vapour per mass dry air (g/kg)
  ## - relative humidity : ("RH")
  ##   water vapour pressure to water vapour saturation pressure (%)
  ## - parts per million : ("PPM")
  ##   volume of water vapour per volume of dry air (ppm)
  ##
  ## input:
  ##     data.in = original humidity data
  ##     meas.in = original measurement
  ##     meas.out = final measurement
  ##     temp = temperature (K)
  ##     press = pressure (Pa)
  ## output:
  ##     data.out = final humidity data
  ## ------------------------------------------------------------
  ## convert temperature and pressure units
  temp.c <- fConvTemp(temp, "K", "C")
  press.h <- fConvPress(press, "Pa", "hPa")
  ## water vapour saturation pressure (hPa)
  pws <- 6.116441 * 10 ^ ((7.591386 * temp.c) / (temp.c + 240.7263))
  ## calculate water vapour pressure (hPa) from original humidity data
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
         stop("unit not found")
         )
  ## calculate final humidity data from water vapour pressure (hPa)
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
         stop("unit not found")
  )
  ## final humidity data
  return(data.out)
}
