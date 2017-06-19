### ---------------------------------------------------------------- ###
### functions for atmospheric chemistry:
### - fAirND() : number density of air
###
### version 2.0, Nov 2016
### author: RS
### ---------------------------------------------------------------- ###

fAirND <- function(temp, press) {
  ## calculate the number density (molecule cm-3) of air, oxygen and
  ## nitrogen in the atmosphere at given temperature and pressure
  ##
  ## input:
  ##     temp = temperature (K)
  ##     press = pressure (Pa)
  ## output:
  ##     df.out = data.frame ( M = number density of air,
  ##                           O2 = number density of O2,
  ##                           N2 = number density of N2 )
  ## ------------------------------------------------------------
  ## Avogadro number, gas constant
  n.avog <- fConstant("Na")$Value
  r.gas <- fConstant("R")$Value
  ## number density of air, oxygen, nitrogen
  m.air <- 1.0e-06 * (n.avog * press) / (r.gas * temp)
  o2.air <- 0.21 * m.air
  n2.air <- 0.78 * m.air
  ## output data.frame
  df.out <- data.frame(m.air, o2.air, n2.air)
  colnames(df.out) <- c("M", "O2", "N2")
  return(df.out)
}
