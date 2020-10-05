### ---------------------------------------------------------------- ###
### functions for atmospheric chemistry:
### - fAirND()    : number density of air, oxygen, nitrogen
### - fFractO1D() : fraction of O1D reacting with water
###
### version 2.1, Dec 2017
### author: RS
### ---------------------------------------------------------------- ###

fAirND <- function(temp, press) {
  ## Calculate the number density (molecule cm-3) of air, oxygen and
  ## nitrogen at given temperature and pressure.
  ##
  ## input:
  ##     temp = temperature (K)
  ##     press = pressure (Pa)
  ## output:
  ##     df.out = data.frame ( M = number density of air,
  ##                           O2 = number density of oxygen,
  ##                           N2 = number density of nitrogen,
  ##                           Temp = temperature,
  ##                           Press = pressure )
  ## ------------------------------------------------------------
  ## Avogadro number and molar gas constant
  n.avog <- fConstant("Na")$Value
  r.gas <- fConstant("R")$Value
  ## number density of air, oxygen, nitrogen
  m.nd <- 1.0e-06 * (n.avog * press) / (r.gas * temp)
  o2.nd <- 0.21 * m.nd
  n2.nd <- 0.78 * m.nd
  ## output data.frame
  df.out <- data.frame(m.nd, o2.nd, n2.nd, temp, press)
  colnames(df.out) <- c("M", "O2", "N2", "Temp", "Press")
  return(df.out)
}

fFractO1D <- function(h2o, temp, press) {
  ## Calculate the fraction of singlet oxygen atoms (O1D) which reacts
  ## with water vapour to form OH radicals -- instead of being
  ## quenched by collision with atmospheric oxygen and nitrogen.
  ##
  ## [ from Ravishankara et al., Geophys. Res. Lett., 2002 ]
  ##
  ## input:
  ##     h2o = water concentration (molecule cm-3)
  ##     temp = temperature (K)
  ##     press = pressure (Pa)
  ## output:
  ##     df.out = data.frame ( fO1D = fraction of O1D reacting with H2O,
  ##                           H2O = water concentration,
  ##                           Temp = temperature,
  ##                           Press = pressure )
  ## ------------------------------------------------------------
  ## number density of oxygen and nitrogen
  o2.nd <- fAirND(temp, press)$O2
  n2.nd <- fAirND(temp, press)$N2
  ## rate coefficients
  k.o1d_h2o <- 2.20e-10                      # O1D + H2O = OH + OH
  k.o1d_o2 <- fKBi(3.2e-11, 67.0, temp)$k1   # O1D + O2 = O3P + O2
  k.o1d_n2 <- fKBi(2.1e-11, 115.0, temp)$k1  # O1D + N2 = O3P + N2
  ## fraction of O1D reacting with H2O
  rate.water <- k.o1d_h2o * h2o
  rate.air <- (k.o1d_o2 * o2.nd) + (k.o1d_n2 * n2.nd)
  f.o1d <- rate.water / (rate.water + rate.air)
  ## output data.frame
  df.out <- data.frame(f.o1d, h2o, temp, press)
  colnames(df.out) <- c("fO1D", "H2O", "Temp", "Press")
  return(df.out)
}
