### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### Functions for atmospheric chemistry:
### - fAirND()    : number density of air, oxygen, nitrogen
### - fFractO1D() : fraction of O1D reacting with water
### - fParamOH()  : estimate OH concentration
### - fPSS()      : photostationary state for O3-NOx
###
### version 2.6, June 2024
### author: RS
### credits: function fPSS() is based on code by LK (Uni Bham)
### ---------------------------------------------------------------- ###

fAirND <- function(temp, press) {
  ## Calculate the number density (molecule cm-3) of air, oxygen and
  ## nitrogen at given temperature and pressure.
  ##
  ## INPUT:
  ##     temp = temperature (K)
  ##     press = pressure (Pa)
  ## OUTPUT:
  ##     df.out = data.frame ( M = number density of air,
  ##                           O2 = number density of oxygen,
  ##                           N2 = number density of nitrogen,
  ##                           Temp = temperature,
  ##                           Press = pressure )
  ## EXAMPLE:
  ##     xx <- fAirND(data_df$Temp, data_df$Press)
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
  ## [ kinetic data from Ravishankara et al, Geophys Res Lett, 2002 ]
  ##
  ## INPUT:
  ##     h2o = water concentration (molecule cm-3)
  ##     temp = temperature (K)
  ##     press = pressure (Pa)
  ## OUTPUT:
  ##     df.out = data.frame ( fO1D = fraction of O1D reacting with H2O,
  ##                           H2O = water concentration,
  ##                           Temp = temperature,
  ##                           Press = pressure )
  ## EXAMPLE:
  ##     xx <- fFractO1D(data_df$Water, data_df$Temp, data_df$Press)
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
  ## Ehhalt & Rohrer [J Geophys Res, 2000], Rohrer & Berresheim
  ## [Nature, 2006].
  ##
  ## The empirical parameters have been derived from ambient datasets
  ## in different regions and conditions:
  ## * POPCORN    =  rural Germany
  ## * ALBATROSS  =  remote Atlantic Ocean
  ## * BERLIOZ    =  rural Germany
  ## * MOHp       =  rural Germany
  ## * MINOS      =  coastal Crete
  ## * NAMBLEX    =  coastal Ireland
  ## * TORCH      =  urban UK
  ## * CHABLIS    =  Antarctica
  ## * RHaMBLe    =  coastal Cape Verde
  ## * OP3        =  tropical forest Borneo
  ## * SOS        =  coastal Cape Verde
  ##
  ## [ empirical paramenters from Stone et al, Chem Soc Rev, 2012 ]
  ##
  ## INPUT:
  ##     jo1d = photolysis rate of O3 to O1D (s-1)
  ## OUTPUT:
  ##     df.out = data.frame ( JO1D = O3 photolysis rate,
  ##                           OH_popcorn = OH estimate for POPCORN,
  ##                           OH_albatross = OH estimate for ALBATROSS,
  ##                           OH_berlioz = OH estimate for BERLIOZ,
  ##                           ... )
  ## EXAMPLE:
  ##     xx <- fParamOH(data_df$jO1D)
  ## ------------------------------------------------------------
  if (!is.vector(jo1d)) {
    jo1d <- as.matrix(jo1d)
  }
  ## empirical parameters derived from ambient datasets
  param.db <- list(
    popcorn   = c(3.90, 0.95, 0.04),
    albatross = c(1.40, 1.30, 0.20),
    berlioz   = c(2.00, 0.95, 0.43),
    mohp      = c(2.40, 0.9313, 0.),
    minos     = c(2.20, 0.68, 0.01),
    namblex   = c(1.47, 0.84, 0.44),
    torch     = c(1.07, 1.16, 0.62),
    chablis   = c(0.25, 0.74, 0.11),
    rhamble   = c(1.73, 0.90, 0.95),
    op3       = c(0.94, 0.6120, 0.),
    sos       = c(1.19, 0.98, 0.50)
  )
  ## OH estimates for each ambient dataset
  oh.est <- sapply(param.db, function(param) {
    aa <- param[1]
    bb <- param[2]
    cc <- param[3]
    (aa * 1e6 * (jo1d / 1.0e-5)^bb) + (cc * 1e6)
  })
  ## ensure correct orientation of the matrix
  if (length(jo1d) == 1) {
    oh.est <- t(oh.est)
  }
  ## output data.frame
  df.out <- data.frame(JO1D = jo1d, oh.est)
  colnames(df.out)[-1] <- paste0("OH_", names(param.db))
  return(df.out)
}

fPSS <- function(sec, o3, no, no2, jno2, temp) {
  ## Calculate the concentrations of ozone (O3) and nitrogen oxides
  ## (NO, NO2) assuming photostationary state for O3-NOx chemistry:
  ##    O3 = (jNO2 * NO2) / (k{O3+NO} * NO)
  ##
  ## [ kinetic data from Atkinson et al, Atmos Chem Phys, 2004 ]
  ##
  ## INPUT:
  ##     sec = number of seconds
  ##     o3 = initial O3 concentration (molecule cm-3)
  ##     no = initial NO concentration (molecule cm-3)
  ##     no2 = initial NO2 concentration (molecule cm-3)
  ##     jno2 = photolysis rate of NO2 (s-1)
  ##     temp = temperature (K)
  ## OUTPUT:
  ##     df.out = data.frame ( SEC = seconds,
  ##                           O3 = O3 concentration,
  ##                           NO = NO concentration,
  ##                           NO2 = NO2 concentration,
  ##                           JNO2 = NO2 photolysis rate,
  ##                           Temp = temperature )
  ## EXAMPLE:
  ##     xx <- fPSS(120, 7.5e11, 2.5e12, 5e11, 1e-2, 298)
  ## ------------------------------------------------------------
  ## rate coefficient of O3+NO
  k.o3_no  <- fKBi(1.4e-12, -1310, temp)$k1
  ## initialize data.frame
  pss.df <- data.frame(SEC = seq(0, sec, 1),
                       O3 = rep(NA, sec+1),
                       NO = rep(NA, sec+1),
                       NO2 = rep(NA, sec+1))
  ## initial concentrations of O3, NO, NO2
  pss.df$O3[1] <- o3
  pss.df$NO[1] <- no
  pss.df$NO2[1] <- no2
  ## calculate O3, NO, NO2 concentrations
  for (i in 2:(sec+1)) {
    pss.df$O3[i] <- pss.df$O3[i-1] + (jno2 * pss.df$NO2[i-1]) - (k.o3_no * pss.df$NO[i-1] * pss.df$O3[i-1])
    pss.df$NO[i] <- pss.df$NO[i-1] + (jno2 * pss.df$NO2[i-1]) - (k.o3_no * pss.df$NO[i-1] * pss.df$O3[i-1])
    pss.df$NO2[i] <- pss.df$NO2[i-1] - (jno2 * pss.df$NO2[i-1]) + (k.o3_no * pss.df$NO[i-1] * pss.df$O3[i-1])
  }
  ## output data.frame
  df.out <- data.frame(pss.df, JNO2 = jno2, Temp = temp)
  return(df.out)
}
