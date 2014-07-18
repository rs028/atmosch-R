### ---------------------------------------------------------------- ###
### functions to convert between measurement units:
###  1. temperature
###  2. pressure
###  3. angle
###  4. time
###  5. multiples of SI units
###  6. concentration (gas-phase)
###  7. concentration (aqueous-phase)
###
### conversion factors from WolframAlpha:
###    http://www.wolframalpha.com/
###
### input:
###       data.in = data in original unit
###       unit.in = original measurement unit
###       unit.out = final measurement unit
###       temp = temperature (K)  [if required]
###       press = pressure (Pa)   [if required]
###
### output:
###        data.out = data in final unit
###
### version 1.5, Dec 2013
### author: RS
### ---------------------------------------------------------------- ###

fConvTemp <- function(data.in, unit.in, unit.out) {
  ## 1. convert between units of temperature:
  ##   kelvin    -> "K"
  ##   celsius   -> "C"
  ##   fahreneit -> "F"
  ## ------------------------------------------------------------
  ## data in original unit to reference unit (K)
  switch(unit.in,
         "K" = {
           data.ref <- data.in
         },
         "C" = {
           data.ref <- data.in + 273.15
         },
         "F" = {
           data.ref <- (data.in + 459.67) * 5 / 9
         },
         stop("unit not found")
         )
  ## data in reference unit (K) to final unit
  switch(unit.out,
         "K" = {
           data.out <- data.ref
         },
         "C" = {
           data.out <- data.ref - 273.15
         },
         "F" = {
           data.out <- (data.ref * 9 / 5) - 459.67
         },
         stop("unit not found")
         )
  ## data in final unit
  return(data.out)
}

fConvPress <- function(data.in, unit.in, unit.out) {
  ## 2. convert between units of pressure:
  ##   pascal      -> "Pa"
  ##   hectopascal -> "hPa"
  ##   kilopascal  -> "kPa"
  ##   atmosphere  -> "atm"
  ##   torr = mmHg -> "torr"
  ##   bar         -> "bar"
  ##   millibar    -> "mbar"
  ##   psi         -> "psi"
  ## ------------------------------------------------------------
  ## data in original unit to reference unit (Pa)
  switch(unit.in,
         "Pa" = {
           data.ref <- data.in
         },
         "hPa" = {
           data.ref <- data.in * 1.0e+02
         },
         "kPa" = {
           data.ref <- data.in * 1.0e+03
         },
         "atm" = {
           data.ref <- data.in * 1.01325e+05
         },
         "torr" = {
           data.ref <- data.in * (20265 / 152)
         },
         "bar" = {
           data.ref <- data.in * 1.0e+05
         },
         "mbar" = {
           data.ref <- data.in * 1.0e+02
         },
         "psi" = {
           data.ref <- data.in * (8896443230521 / 1290320000)
         },
         stop("unit not found")
         )
  ## data in reference unit (Pa) to final unit
  switch(unit.out,
         "Pa" = {
           data.out <- data.ref
         },
         "hPa" = {
           data.out <- data.ref / 1.0e+02
         },
         "kPa" = {
           data.out <- data.ref / 1.0e+03
         },
         "atm" = {
           data.out <- data.ref / 1.01325e+05
         },
         "torr" = {
           data.out <- data.ref / (20265 / 152)
         },
         "bar" = {
           data.out <- data.ref / 1.0e+05
         },
         "mbar" = {
           data.out <- data.ref / 1.0e+02
         },
         "psi" = {
           data.out <- data.ref / (8896443230521 / 1290320000)
         },
         stop("unit not found")
         )
  ## data in final unit
  return(data.out)
}

fConvAngle <- function(data.in, unit.in, unit.out) {
  ## 3. convert between units of angle:
  ##   radian -> "rad"
  ##   degree -> "deg"
  ## ------------------------------------------------------------
  ## data in original unit to reference unit (rad)
  switch(unit.in,
         "rad" = {
           data.ref <- data.in
         },
         "deg" = {
           data.ref <- data.in * (pi / 180)
         },
         stop("unit not found")
         )
  ## data in reference unit (rad) to final unit
  switch(unit.out,
         "rad" = {
           data.out <- data.ref
         },
         "deg" = {
           data.out <- data.ref / (pi / 180)
         },
         stop("unit not found")
         )
  ## data in final unit
  return(data.out)
}

fConvTime <- function(data.in, unit.in, unit.out) {
  ## 4. convert between units of time:
  ##   second -> "sec"
  ##   minute -> "min"
  ##   hour   -> "hr"
  ##   day    -> "day"
  ##   week   -> "wk"
  ## ------------------------------------------------------------
  ## data in original unit to reference unit (sec)
  switch(unit.in,
         "sec" = {
           data.ref <- data.in
         },
         "min" = {
           data.ref <- data.in * 60
         },
         "hr" = {
           data.ref <- data.in * 3600
         },
         "day" = {
           data.ref <- data.in * 86400
         },
         "wk" = {
           data.ref <- data.in * (86400 * 7)
         },
         stop("unit not found")
         )
  ## data in reference unit (sec) to final unit
  switch(unit.out,
         "sec" = {
           data.out <- data.ref
         },
         "min" = {
           data.out <- data.ref / 60
         },
         "hr" = {
           data.out <- data.ref / 3600
         },
         "day" = {
           data.out <- data.ref / 86400
         },
         "wk" = {
           data.out <- data.ref / (86400 * 7)
         },
         stop("unit not found")
         )
  ## data in final unit
  return(data.out)
}

fConvSI <- function(data.in, unit.in, unit.out) {
  ## 5. convert between multiples of SI units:
  ##   exa       -> "Ex"     atto  -> "a"
  ##   peta      -> "P"     femto -> "f"
  ##   tera      -> "T"     pico  -> "p"
  ##   giga      -> "G"     nano  -> "n"
  ##   mega      -> "M"     micro -> "u"
  ##   kilo      -> "k"     milli -> "m"
  ##   hecto     -> "h"     centi -> "c"
  ##   deca      -> "da"    deci  -> "d"
  ##   base unit -> "-"
  ## ------------------------------------------------------------
  ## data in original unit to reference unit (base unit)
  switch(unit.in,
         "Ex" = {
           data.ref <- data.in * 1.0e+18
         },
         "P" = {
           data.ref <- data.in * 1.0e+15
         },
         "T" = {
           data.ref <- data.in * 1.0e+12
         },
         "G" = {
           data.ref <- data.in * 1.0e+09
         },
         "M" = {
           data.ref <- data.in * 1.0e+06
         },
         "k" = {
           data.ref <- data.in * 1.0e+03
         },
         "h" = {
           data.ref <- data.in * 1.0e+02
         },
         "da" = {
           data.ref <- data.in * 1.0e+01
         },
         "-" = {
           data.ref <- data.in
         },
         "d" = {
           data.ref <- data.in * 1.0e-01
         },
         "c" = {
           data.ref <- data.in * 1.0e-02
         },
         "m" = {
           data.ref <- data.in * 1.0e-03
         },
         "u" = {
           data.ref <- data.in * 1.0e-06
         },
         "n" = {
           data.ref <- data.in * 1.0e-09
         },
         "p" = {
           data.ref <- data.in * 1.0e-12
         },
         "f" = {
           data.ref <- data.in * 1.0e-15
         },
         "a" = {
           data.ref <- data.in * 1.0e-18
         },
         stop("unit not found")
         )
  ## data in reference unit (base unit) to final unit
  switch(unit.out,
         "Ex" = {
           data.out <- data.ref / 1.0e+18
         },
         "P" = {
           data.out <- data.ref / 1.0e+15
         },
         "T" = {
           data.out <- data.ref / 1.0e+12
         },
         "G" = {
           data.out <- data.ref / 1.0e+09
         },
         "M" = {
           data.out <- data.ref / 1.0e+06
         },
         "k" = {
           data.out <- data.ref / 1.0e+03
         },
         "h" = {
           data.out <- data.ref / 1.0e+02
         },
         "da" = {
           data.out <- data.ref / 1.0e+01
         },
         "-" = {
           data.out <- data.ref
         },
         "d" = {
           data.out <- data.ref / 1.0e-01
         },
         "c" = {
           data.out <- data.ref / 1.0e-02
         },
         "m" = {
           data.out <- data.ref / 1.0e-03
         },
         "u" = {
           data.out <- data.ref / 1.0e-06
         },
         "n" = {
           data.out <- data.ref / 1.0e-09
         },
         "p" = {
           data.out <- data.ref / 1.0e-12
         },
         "f" = {
           data.out <- data.ref / 1.0e-15
         },
         "a" = {
           data.out <- data.ref / 1.0e-18
         },
         stop("unit not found")
         )
  ## data in final unit
  return(data.out)
}

fConcGas <- function(data.in, unit.in, unit.out, temp, press) {
  ## 6. convert between units of concentration (gas-phase):
  ##   molecule cm-3 -> "ND"
  ##   ppth          -> "ppth"
  ##   ppm           -> "ppm"
  ##   ppb           -> "ppb"
  ##   ppt           -> "ppt"
  ##   mole m-3      -> "MD"
  ## ------------------------------------------------------------
  ## Avogadro number and air number density
  n.avog <- fConstant("Na")$Value
  m.air <- fAirND(temp, press)$M
  ## data in original unit to reference unit (ND)
  switch(unit.in,
         "ND" = {
           data.ref <- data.in
         },
         "ppth" = {
           data.ref <- data.in * (m.air * 1.0e-03)
         },
         "ppm" = {
           data.ref <- data.in * (m.air * 1.0e-06)
         },
         "ppb" = {
           data.ref <- data.in * (m.air * 1.0e-09)
         },
         "ppt" = {
           data.ref <- data.in * (m.air * 1.0e-12)
         },
         "MD" = {
           data.ref <- data.in * (n.avog * 1.0e-06)
         },
         stop("unit not found")
         )
  ## data in reference unit (ND) to final unit
  switch(unit.out,
         "ND" = {
           data.out <- data.ref
         },
         "ppth" = {
           data.out <- data.ref / (m.air * 1.0e-03)
         },
         "ppm" = {
           data.out <- data.ref / (m.air * 1.0e-06)
         },
         "ppb" = {
           data.out <- data.ref / (m.air * 1.0e-09)
         },
         "ppt" = {
           data.out <- data.ref / (m.air * 1.0e-12)
         },
         "MD" = {
           data.out <- data.ref / (n.avog * 1.0e-06)
         },
         stop("unit not found")
         )
  ## data in final unit
  return(data.out)
}

fConcAq <- function(data.in, unit.in, unit.out) {
  ## 7. convert between units of concentration (aqueous-phase):
  ##   molarity (mole/l, mole/dm3) -> "M"
  ##   mole m-3                    -> "MD"
  ## ------------------------------------------------------------
  ## Avogadro number
  n.avog <- fConstant("Na")$Value
  ## data in original unit to reference unit (M)
  switch(unit.in,
         "M" = {
           data.ref <- data.in
         },
         "MD" = {
           data.ref <- data.in * 1.0e-03
         },
         stop("unit not found")
         )
  ## data in reference unit (M) to final unit
  switch(unit.out,
         "M" = {
           data.out <- data.ref
         },
         "MD" = {
           data.out <- data.ref / 1.0e-03
         },
         stop("unit not found")
         )
  ## data in final unit
  return(data.out)
}
