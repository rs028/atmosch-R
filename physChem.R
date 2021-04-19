### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### Functions for physical chemistry:
### - fGasLaw() : ideal gas law
### - fKBi()    : rate coefficient of bimolecular reactions (standard)
### - fKBix()   : rate coefficient of bimolecular reactions (expanded)
### - fKTer()   : rate coefficient of termolecular reactions
### - fLifeT()  : chemical lifetime and half-life
###
### version 2.4, Oct 2020
### author: RS
### ---------------------------------------------------------------- ###

fGasLaw <- function(press, vol, mol, temp) {
  ## Solve the equation of state of a gas using the ideal gas law:
  ##    PV = nRT
  ##
  ## NB: use "?" to indicate the unknown variable.
  ##
  ## INPUT:
  ##     press = pressure (Pa)
  ##     vol = volume (m3; 1 m3 = 1000 L)
  ##     mol = number of moles
  ##     temp = temperature (K)
  ## OUTPUT:
  ##     df.out = data.frame( Press = pressure,
  ##                          Vol = volume,
  ##                          Mol = number of moles,
  ##                          Temp = temperature )
  ## EXAMPLE:
  ##     xx <- fGasLaw(data_df$Pressure, data_df$Volume, "?", data_df$Temperature)
  ## ------------------------------------------------------------
  ## molar gas constant
  r.gas <- fConstant("R")$Value
  ## calculate unknown variable
  if (all(press == "?")) {        # pressure
    press <- (mol * r.gas * temp) / vol
  } else if (all(vol == "?")) {   # volume
    vol <- (mol * r.gas * temp) / press
  } else if (all(mol == "?")) {   # n. of moles
    mol <- (press * vol) / (r.gas * temp)
  } else if (all(temp == "?")) {  # temperature
    temp <- (press * vol) / (mol * r.gas)
  }
  ## output data.frame
  df.out <- data.frame(press, vol, mol, temp)
  colnames(df.out) <- c("Press", "Vol", "Mol", "Temp")
  return(df.out)
}

fKBi <- function(aa, ea.r, temp) {
  ## Calculate the rate coefficient (cm3 molecule-1 s-1) of
  ## bimolecular reactions using the Arrhenius equation:
  ##    k = A * exp(-Ea/RT)
  ##
  ## INPUT:
  ##     aa = pre-exponential factor (cm3 molecule-1 s-1)
  ##     ea.r = -Ea/R (J mol-1 / J K-1 mol-1)
  ##     temp = temperature (K)
  ## OUTPUT:
  ##     df.out = data.frame ( rate coeff 1, rate coeff 2, ...,
  ##                           temperature )
  ## EXAMPLE:
  ##     xx <- fKBi(1.85e-12, -1690, data_df$Temperature)
  ## ------------------------------------------------------------
  aa <- as.matrix(aa)
  ea.r <- as.matrix(ea.r)
  temp <- as.matrix(temp)
  ## rate coefficient (standard Arrhenius)
  kt <- sapply(temp, function(x) aa * exp(ea.r / x))
  kt <- as.matrix(kt)
  ## output data.frame
  if (dim(aa)[1] != 1) {
    df.out <- data.frame(t(kt), temp)
  } else {
    df.out <- data.frame(kt, temp)
  }
  nr <- ncol(df.out) - 1
  colnames(df.out) <- c(paste("k", seq(1, nr, by=1), sep=""), "Temp")
  return(df.out)
}

fKBix <- function(aa, t0, nn, ea.r, temp) {
  ## Calculate the rate coefficient (cm3 molecule-1 s-1) of
  ## bimolecular reactions using the expanded Arrhenius equation:
  ##    k = A * (T/T0)^n * exp(-Ea/RT)
  ##
  ## INPUT:
  ##     aa = pre-exponential factor (cm3 molecule-1 s-1)
  ##     t0 = reference temperature (K)        [ 1 if not used ]
  ##     nn = reference temperature exponent   [ 0 if not used ]
  ##     ea.r = -Ea/R (J mol-1 / J K-1 mol-1)  [ 0 if not used ]
  ##     temp = temperature (K)
  ## OUTPUT:
  ##     df.out = data.frame ( rate coeff 1, rate coeff 2, ...,
  ##                           temperature )
  ## EXAMPLE:
  ##     xx <- fKBix(2.8e-14, 1, 0.667, -1575, data_df$Temperature)
  ## ------------------------------------------------------------
  aa <- as.matrix(aa)
  t0 <- as.matrix(t0)
  nn <- as.matrix(nn)
  ea.r <- as.matrix(ea.r)
  temp <- as.matrix(temp)
  ## rate coefficient (expanded Arrhenius)
  kt <- sapply(temp, function(x) aa * (x / t0)^nn * exp(ea.r / x))
  kt <- as.matrix(kt)
  ## output data.frame
  if (dim(aa)[1] != 1) {
    df.out <- data.frame(t(kt), temp)
  } else {
    df.out <- data.frame(kt, temp)
  }
  nr <- ncol(df.out) - 1
  colnames(df.out) <- c(paste("k", seq(1, nr, by=1), sep=""), "Temp")
  return(df.out)
}

fKTer <- function(k.zero, k.inf, fc, temp, press, ref.fc) {
  ## Calculate the rate coefficient (cm3 molecule-1 s-1) of a
  ## termolecular reaction at given temperature and pressure using the
  ## Troe parametrization:
  ##    k = F (k0 * ki) / (k0 + ki)
  ##
  ## NB: use fKBi() or fKBix() to calculate the rate coefficients
  ##     `k.zero` and `k.inf`.
  ##
  ## INPUT:
  ##     k.zero = low pressure limit rate coefficient (cm3 molecule-1 s-1)
  ##     k.inf = high pressure limit rate coefficient (cm3 molecule-1 s-1)
  ##     fc = falloff curve factor
  ##     temp = temperature (K)
  ##     press = pressure (Pa)
  ##     ref.fc = "iupac" OR "jpl" convention
  ## OUTPUT:
  ##     df.out = data.frame ( rate coeff, temperature, pressure )
  ## EXAMPLE:
  ##     xx <- fKTer(fKBix(3.6e-30, 300, -4.1, 0, data_df$Temperature)$k1,
  ##                 fKBix(1.9e-12, 300, 0.2, 0, data_df$Temperature)$k1,
  ##                 0.35, data_df$Temperature, data_df$Pressure, "iupac")
  ## ------------------------------------------------------------
  ## air number density
  mm <- fAirND(temp, press)$M
  ## falloff curve parameters
  if (ref.fc == "iupac") {       # IUPAC convention
    fc <- fc
    nn <- 0.75 - 1.27 * log10(fc)
  } else if (ref.fc == "jpl") {  # JPL convention
    fc <- 0.6
    nn <- 1.0
  } else {
    stop("INPUT ERROR: select 'iupac' or 'jpl' convention")
  }
  ## broadening factor F
  kr <-  (k.zero * mm) / k.inf
  ff <- 10^( log10(fc) / (1 + (log10(kr) / nn)^2) )
  ## rate coefficient (Troe parametrization)
  kt <- ff * (kr / (1 + kr)) * k.inf
  ## output data.frame
  df.out <- data.frame(kt, temp, press)
  colnames(df.out) <- c("kt", "Temp", "Press")
  return(df.out)
}

fLifeT <- function(k.gas, c.gas) {
  ## Calculate the chemical lifetime (s) and the half-life (s) of a
  ## gas with respect to a first or second order process.
  ##
  ## NB: use fKBi(), fKBix() or fKTer() to calculate the rate
  ##     coefficient `k.gas`.
  ##
  ## INPUT:
  ##     k.gas = rate coefficient (cm3 molecule-1 s-1 OR s-1)
  ##     c.gas = concentration of reactant gas (molecule cm-3) OR
  ##             1 (for a first order process, with `k.gas` in s-1)
  ## OUTPUT:
  ##     df.out = data.frame( kp = pseudo-1st order rate coeff,
  #                           tau = chemical lifetime,
  ##                          t1_2 = chemical half-life )
  ## EXAMPLE:
  ##     xx <- fLifeT(1e-10, data_df$OH)
  ## ------------------------------------------------------------
  ## pseudo-1st order rate coefficient
  kp <- k.gas * c.gas
  ## chemical lifetime and half-life
  tau <- 1 / kp
  t1_2 <- tau * log(2)
  ## output data.frame
  df.out <- data.frame(kp, tau, t1_2)
  colnames(df.out) <- c("kp", "tau", "t1_2")
  return(df.out)
}
