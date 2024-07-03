### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### Functions to convert between measurement units:
### - fConvTemp()  : temperature
### - fConvPress() : pressure
### - fConvAngle() : angle
### - fConvTime()  : time
### - fConcGas()   : concentration (gas-phase)
### - fConcAq()    : concentration (aqueous-phase)
### - fHumid()     : humidity/water
###
### Conversion factors from WolframAlpha:
###   https://www.wolframalpha.com/
###
### version 2.4, March 2024
### author: RS
### ---------------------------------------------------------------- ###

fConvTemp <- function(data.in, unit.in, unit.out) {
  ## Convert between units of temperature:
  ## * kelvin    = "K"
  ## * celsius   = "C"
  ## * fahreneit = "F"
  ##
  ## INPUT:
  ##     data.in = data in original unit
  ##     unit.in = original unit
  ##     unit.out = final unit
  ## OUTPUT:
  ##     data.out = data in final unit
  ## EXAMPLE:
  ##     xx <- fConvTemp(298, "K", "C")
  ## ------------------------------------------------------------
  ## from original unit to reference unit (K)
  convert_to_K <- function(data, unit) {
    switch(unit,
           "K" = data,
           "C" = data + 273.15,
           "F" = (data + 459.67) * 5/9,
           stop("INPUT ERROR: unit not found")
           )
  }
  ## from reference unit (K) to final unit
  convert_from_K <- function(data, unit) {
    switch(unit,
           "K" = data,
           "C" = data - 273.15,
           "F" = (data * 9/5) - 459.67,
           stop("INPUT ERROR: unit not found")
           )
  }
  ## convert data from original unit to final unit
  data.ref <- convert_to_K(data.in, unit.in)
  data.out <- convert_from_K(data.ref, unit.out)
  return(data.out)
}

fConvPress <- function(data.in, unit.in, unit.out) {
  ## Convert between units of pressure:
  ## * pascal      = "Pa"
  ## * hectopascal = "hPa"
  ## * atmosphere  = "atm"
  ## * torr = mmHg = "torr"
  ## * bar         = "bar"
  ## * millibar    = "mbar"
  ## * psi         = "psi"
  ##
  ## INPUT:
  ##     data.in = data in original unit
  ##     unit.in = original unit
  ##     unit.out = final unit
  ## OUTPUT:
  ##     data.out = data in final unit
  ## EXAMPLE:
  ##     xx <- fConvPress(1013, "mbar", "torr")
  ## ------------------------------------------------------------
  ## from original unit to reference unit (Pa)
  convert_to_Pa <- function(data, unit) {
    switch(unit,
           "Pa" = data,
           "hPa" = data * 1.0e+02,
           "atm" = data * 1.01325e+05,
           "torr" = data * (20265 / 152),
           "bar" = data * 1.0e+05,
           "mbar" = data * 1.0e+02,
           "psi" = data * (8896443230521 / 1290320000),
           stop("INPUT ERROR: unit not found")
           )
  }
  ## from reference unit (Pa) to final unit
  convert_from_Pa <- function(data, unit) {
    switch(unit,
           "Pa" = data,
           "hPa" = data / 1.0e+02,
           "atm" = data / 1.01325e+05,
           "torr" = data * (152 / 20265),
           "bar" = data / 1.0e+05,
           "mbar" = data / 1.0e+02,
           "psi" = data * (1290320000 / 8896443230521),
           stop("INPUT ERROR: unit not found")
           )
  }
  ## convert data from original unit to final unit
  data.ref <- convert_to_Pa(data.in, unit.in)
  data.out <- convert_from_Pa(data.ref, unit.out)
  return(data.out)
}

fConvAngle <- function(data.in, unit.in, unit.out) {
  ## Convert between units of angle:
  ## * radian = "rad"
  ## * degree = "deg"
  ##
  ## INPUT:
  ##     data.in = data in original unit
  ##     unit.in = original unit
  ##     unit.out = final unit
  ## OUTPUT:
  ##     data.out = data in final unit
  ## EXAMPLE:
  ##     xx <- fConvAngle(33, "deg", "rad")
  ## ------------------------------------------------------------
  ## from original unit to reference unit (rad)
  convert_to_rad <- function(data, unit) {
    switch(unit,
           "rad" = data,
           "deg" = data * (pi / 180),
           stop("INPUT ERROR: unit not found")
           )
  }
  ## from reference unit (rad) to final unit
  convert_from_rad <- function(data, unit) {
    switch(unit,
           "rad" = data,
           "deg" = data * (180 / pi),
           stop("INPUT ERROR: unit not found")
           )
  }
  ## convert data from original unit to final unit
  data.ref <- convert_to_rad(data.in, unit.in)
  data.out <- convert_from_rad(data.ref, unit.out)
  return(data.out)
}

fConvTime <- function(data.in, unit.in, unit.out) {
  ## Convert between units of time:
  ## * second = "sec"
  ## * minute = "min"
  ## * hour   = "hr"
  ## * day    = "day"
  ## * week   = "wk"
  ##
  ## INPUT:
  ##     data.in = data in original unit
  ##     unit.in = original unit
  ##     unit.out = final unit
  ## OUTPUT:
  ##     data.out = data in final unit
  ## EXAMPLE:
  ##     xx <- fConvTime(2, "day", "sec")
  ## ------------------------------------------------------------
  ## from original unit to reference unit (sec)
  convert_to_sec <- function(data, unit) {
    switch(unit,
           "sec" = data,
           "min" = data * 60,
           "hr" = data * 3600,
           "day" = data * 86400,
           "wk" = data * (86400 * 7),
           stop("INPUT ERROR: unit not found")
           )
  }
  ## from reference unit (sec) to final unit
  convert_from_sec <- function(data, unit) {
    switch(unit,
           "sec" = data,
           "min" = data / 60,
           "hr" = data / 3600,
           "day" = data / 86400,
           "wk" = data / (86400 * 7),
           stop("INPUT ERROR: unit not found")
           )
  }
  ## convert data from original unit to final unit
  data.ref <- convert_to_sec(data.in, unit.in)
  data.out <- convert_from_sec(data.ref, unit.out)
  return(data.out)
}

### ---------------------------------------------------------------- ###

fConcGas <- function(data.in, unit.in, unit.out, temp, press, m.mass=NULL) {
  ## Convert between units of concentration (gas-phase):
  ## * molecule cm-3 = "ND"
  ## * ppth          = "ppth"
  ## * ppm           = "ppm"
  ## * ppb           = "ppb"
  ## * ppt           = "ppt"
  ## * mole m-3      = "MD"
  ## * ug m-3        = "UG"
  ##
  ## NB: molar mass is required only for conversions to/from "UG".
  ##
  ## INPUT:
  ##     data.in = data in original concentration unit
  ##     unit.in = original concentration unit
  ##     unit.out = final concentration unit
  ##     temp = temperature (K)
  ##     press = pressure (Pa)
  ##     m.mass = molar mass (g/mole)     [ OPTIONAL ]
  ## OUTPUT:
  ##     data.out = data in final concentration unit
  ## EXAMPLE:
  ##     xx <- fConcGas(data_df$O3, "ppb", "MD", data_df$Temp, data_df$Press)
  ## ------------------------------------------------------------
  ## check molar mass input
  if (unit.in == "UG" || unit.out == "UG") {
    if (is.null(m.mass)) {
      stop("INPUT ERROR: molar mass needed")
    }
  }
  ## Avogadro number, number density of air
  n.avog <- fConstant("Na")$Value
  m.air <- fAirND(temp, press)$M
  ## from original unit to reference unit (ND)
  convert_to_ND <- function(data, unit) {
    switch(unit,
           "ND" = data,
           "ppth" = data * (m.air * 1.0e-03),
           "ppm" = data * (m.air * 1.0e-06),
           "ppb" = data * (m.air * 1.0e-09),
           "ppt" = data * (m.air * 1.0e-12),
           "MD" = data * (n.avog * 1.0e-06),
           "UG" = {
             data.tmp <- data * (n.avog * 1.0e-12)
             if (length(m.mass) == 1) {
               data.tmp / m.mass
             } else {
               t(apply(data.tmp, 1, function(x) x / m.mass))
             }
           },
           stop("INPUT ERROR: unit not found")
           )
  }
  ## from reference unit (ND) to final unit
  convert_from_ND <- function(data, unit) {
    switch(unit,
           "ND" = data,
           "ppth" = data / (m.air * 1.0e-03),
           "ppm" = data / (m.air * 1.0e-06),
           "ppb" = data / (m.air * 1.0e-09),
           "ppt" = data / (m.air * 1.0e-12),
           "MD" = data / (n.avog * 1.0e-06),
           "UG" = {
             data.tmp <- data / (n.avog * 1.0e-12)
             if (length(m.mass) == 1) {
               data.tmp * m.mass
             } else {
               t(apply(data.tmp, 1, function(x) x * m.mass))
             }
           },
             stop("INPUT ERROR: unit not found")
           )
  }
  ## convert concentration from original unit to final unit
  data.ref <- convert_to_ND(data.in, unit.in)
  data.out <- convert_from_ND(data.ref, unit.out)
  return(data.out)
}

fConcAq <- function(data.in, unit.in, unit.out, m.mass=NULL) {
  ## Convert between units of concentration (aqueous-phase):
  ## * mole/L (molarity) = "M"
  ## * mole m-3          = "MD"
  ## * ug m-3            = "UG"
  ##
  ## NB: molar mass is required only for conversions to/from "UG".
  ##
  ## INPUT:
  ##     data.in = data in original concentration unit
  ##     unit.in = original concentration unit
  ##     unit.out = final concentration unit
  ##     m.mass = molar mass (g/mole)     [ OPTIONAL ]
  ## OUTPUT:
  ##     data.out = data in final concentration unit
  ## EXAMPLE:
  ##     xx <- fConcAq(data_df$NO3, "M", "UG", 62)
  ## -------------------------------------------------------------
  ## check molar mass input
  if (unit.in == "UG" || unit.out == "UG") {
    if (is.null(m.mass)) {
      stop("INPUT ERROR: molar mass needed")
    }
  }
  ## from original unit to reference unit (M)
  convert_to_M <- function(data, unit) {
    switch(unit,
           "M" = data,
           "MD" = data * 1.0e-03,
           "UG" = {
             data.tmp <- data * 1.0e-09
             if (length(m.mass) == 1) {
               data.tmp / m.mass
             } else {
               t(apply(data.tmp, 1, function(x) x / m.mass))
             }
           },
           stop("INPUT ERROR: unit not found")
           )
  }
  ## from reference unit (M) to final unit
  convert_from_M <- function(data, unit) {
    switch(unit,
           "M" = data,
           "MD" = data / 1.0e-03,
           "UG" = {
             data.tmp <- data / 1.0e-09
             if (length(m.mass) == 1) {
               data.tmp * m.mass
             } else {
               t(apply(data.tmp, 1, function(x) x * m.mass))
             }
           },
           stop("INPUT ERROR: unit not found")
           )
  }
  ## convert concentration from original unit to final unit
  data.ref <- convert_to_M(data.in, unit.in)
  data.out <- convert_from_M(data.ref, unit.out)
  return(data.out)
}

fHumid <- function(data.in, unit.in, unit.out, temp, press=101325) {
  ## Convert between units of humidity at given temperature and
  ## pressure:
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
  ## [ from "Humidity Conversion Formulas", published by Vaisala:
  ##   https://www.vaisala.com/ ]
  ##
  ## NB: pressure is required only for conversions to/from "PPM".
  ##     If not specified, it is assumed to be 1 atm.
  ##
  ## INPUT:
  ##     data.in = data in original humidity unit
  ##     unit.in = original humidity unit
  ##     unit.out = final humidity unit
  ##     temp = temperature (K)
  ##     press = pressure (Pa)     [ OPTIONAL, default=101325 ]
  ## OUTPUT:
  ##     data.out = data in final humidity unit
  ## EXAMPLE:
  ##     xx <- fHumid(data_df, "AH", "RH", data_df$Temp)
  ## ------------------------------------------------------------
  ## convert temperature to C, pressure to hPa
  temp.c <- fConvTemp(temp, "K", "C")
  press.hpa <- fConvPress(press, "Pa", "hPa")
  ## water vapour saturation pressure (hPa)
  pws <- 6.116441 * 10^( (7.591386 * temp.c) / (temp.c + 240.7263) )
  ## from original humidity to water vapour pressure (hPa)
  convert_to_pw <- function(data, unit) {
    switch(unit,
           "AH" = 1.0e-02 * (data * temp.c) / 2.16679,
           "SH" = 1.0e-03 * (data * press.hpa) / 0.622,
           "MR" = (data * press.hpa) / (621.9907 + data),
           "RH" = data * pws / 100,
           "PPM" = (data * 1.0e-06 * press.hpa) / (1 + data * 1.0e-06),
           stop("INPUT ERROR: unit not found")
           )
  }
  ## from water vapour pressure (hPa) to final humidity
  convert_from_pw <- function(data, unit) {
    switch(unit,
           "AH" = 1.0e+02 * (data * 2.16679) / temp.c,
           "SH" = 1.0e+03 * (data * 0.622) / press.hpa,
           "MR" = (621.9907 * data) / (press.hpa - data),
           "RH" = 100 * data / pws,
           "PPM" = 1.0e+06 * data / (press.hpa - data),
           stop("INPUT ERROR: unit not found")
           )
  }
  ## convert humidity from original unit to final unit
  data.ref <- convert_to_pw(data.in, unit.in)
  data.out <- convert_from_pw(data.ref, unit.out)
  return(data.out)
}
