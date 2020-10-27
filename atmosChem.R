### ---------------------------------------------------------------- ###
### functions for atmospheric chemistry:
### - fAirND()    : number density of air, oxygen, nitrogen
### - fFractO1D() : fraction of O1D reacting with water
### - fParamOH()  : estimate OH concentration
###
### version 2.3, Oct 2020
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
  ## Avogadro number, molar gas constant
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
  ## [ Ravishankara et al., Geophys. Res. Lett., 2002 ]
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

fParamOH <- function(jo1d) {
  ## Estimate the concentration of OH radicals using the empirical
  ## relationship between OH and solar UV radiation developed by
  ## Ehhalt & Rohrer (J. Geophys. Res., 2000), Rohrer & Berresheim
  ## (Nature, 2006). Empirical parameters have been derived from
  ## ambient datasets in different regions and conditions:
  ##
  ##   POPCORN    =  Rural Germany
  ##   ALBATROSS  =  Remote Atlantic Ocean
  ##   BERLIOZ    =  Rural Germany
  ##   MOHp       =  Rural Germany
  ##   MINOS      =  Coastal Crete
  ##   NAMBLEX    =  Coastal Ireland
  ##   TORCH      =  Urban UK
  ##   CHABLIS    =  Antarctica
  ##   RHaMBLe    =  Coastal Cape Verde
  ##   OP3        =  Tropical forest Borneo
  ##   SOS        =  Coastal Cape Verde
  ##
  ## [ from Stone et al., Chem Soc. Rev., 2012 ]
  ##
  ## input:
  ##     jo1d = photolysis rate of O3 to O1D (s-1)
  ## output:
  ##     df.out = data.frame ( JO1D, OH_popcorn, OH_albatross, ... )
  ## ------------------------------------------------------------
  ## empirical parameters from ambient datasets
  p01 <- c("popcorn", 3.90, 0.95, 0.04)
  p02 <- c("albatross", 1.40, 1.30, 0.20)
  p03 <- c("berlioz", 2.00, 0.95, 0.43)
  p04 <- c("mohp", 2.40, 0.93, 0.13)
  p05 <- c("minos", 2.20, 0.68, 0.01)
  p06 <- c("namblex", 1.47, 0.84, 0.44)
  p07 <- c("torch", 1.07, 1.16, 0.62)
  p08 <- c("chablis", 0.25, 0.74, 0.11)
  p09 <- c("rhamble", 1.73, 0.90, 0.95)
  p10 <- c("op3", 0.94, 0.61, 0.20)
  p11 <- c("sos", 1.19, 0.98, 0.50)
  data.db <- rbind(p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11)
  ## OH estimates
  df.out <- data.frame(JO1D = jo1d)
  for (i in 1:nrow(data.db)) {
    aa <- as.numeric(data.db[i,2])
    bb <- as.numeric(data.db[i,3])
    cc <- as.numeric(data.db[i,4])
    oh <- (aa * 1e6 * (jo1d / 1.0e-5) ^ bb) + (cc * 1e6)
    df.out <- cbind(df.out, oh)
  }
  ## output data.frame
  colnames(df.out)[-1] <- paste("OH_", data.db[,1], sep="")
  return(df.out)
}
