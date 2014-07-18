### ---------------------------------------------------------------- ###
### functions for atmospheric chemistry:
###  1. number density of air, O2, N2
###  2. estimate OH from jO1D
###
### version 1.7, Feb 2014
### author: RS
### ---------------------------------------------------------------- ###

fAirND <- function(temp, press) {
  ## 1. calculate number density (molecule cm-3) of air, molecular
  ## oxygen and molecular nitrogen at given temperature and pressure
  ##
  ## input:
  ##       temp = temperature (K)
  ##       press = pressure (Pa)
  ## output:
  ##        data.frame ( M = number density of air,
  ##                     O2 = number density of O2,
  ##                     N2 = number density of N2 )
  ## ------------------------------------------------------------
  ## Avogadro number, gas constant
  n.avog <- fConstant("Na")$Value
  r.gas <- fConstant("R")$Value
  ## number density of air, O2, N2
  m.air <- 1.0e-06 * (n.avog * press) / (r.gas * temp)
  o2.air <- 0.21 * m.air
  n2.air <- 0.78 * m.air
  ## output data.frame
  df.out <- data.frame(M = m.air,
                       O2 = o2.air,
                       N2 = n2.air)
  return(df.out)
}

