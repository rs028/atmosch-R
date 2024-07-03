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
### version 2.5, July 2024
### author: RS
### ---------------------------------------------------------------- ###

fGasLaw <- function(press, vol, mol, temp) {
  ## Solve the equation of state of a gas using the Ideal Gas Law:
  ##    PV = nRT
  ##
  ## INPUT:     [ "?" for the unknown variable ]
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
  ##     xx <- fGasLaw(data_df$Press, data_df$Vol, "?", data_df$Temp)
  ## ------------------------------------------------------------
  args <- list(press, vol, mol, temp)
  unknwn <- which(sapply(args, function(x) is.character(x) && x == "?"))
  if (length(unknwn) != 1) {
    stop("INPUT ERROR: only one unknown variable allowed")
  }
  ## molar gas constant
  r.gas <- fConstant("R")$Value
  ## calculate unknown variable
  if (unknwn == 1) {   # pressure
    press <- (mol * r.gas * temp) / vol
  } else if (unknwn == 2) {   # volume
    vol <- (mol * r.gas * temp) / press
  } else if (unknwn == 3) {   # number of moles
    mol <- (press * vol) / (r.gas * temp)
  } else if (unknwn == 4) {   # temperature
    temp <- (press * vol) / (mol * r.gas)
  }
  ## output data.frame
  df.out <- data.frame(press, vol, mol, temp)
  colnames(df.out) <- c("Press", "Vol", "Mol", "Temp")
  return(df.out)
}

fKBi <- function(aa, ea.r, temp) {
  ## Calculate the rate coefficient (cm3 molecule-1 s-1) of one or
  ## more bimolecular reactions over a range of temperatures, using
  ## the standard Arrhenius equation:
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
  ##     xx <- fKBi(1.85e-12, -1690, data_df$Temp)
  ## ------------------------------------------------------------
  aa <- as.data.frame(aa)
  ea.r <- as.data.frame(ea.r)
  temp <- as.data.frame(temp)
  nr <- nrow(aa)
  if (nr != nrow(ea.r)) {
    stop("INPUT ERROR: mismatched kinetic parameters")
  }
  ## rate coefficient (standard Arrhenius)
  df.coeff <- matrix(nrow = nrow(temp), ncol = nr)
  for (k in 1:ncol(df.coeff)) {
    for (t in 1:nrow(df.coeff)) {
      df.coeff[t,k] <- aa[k,1] * exp(ea.r[k,1] / temp[t,1])
    }
  }
  ## output data.frame
  df.out <- data.frame(df.coeff, temp)
  colnames(df.out) <- c(paste0("k", seq_len(nr)), "Temp")
  return(df.out)
}

fKBix <- function(aa, t0, nn, ea.r, temp) {
  ## Calculate the rate coefficient (cm3 molecule-1 s-1) of one or
  ## more bimolecular reactions over a range of temperatures, using
  ## the expanded Arrhenius equation:
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
  ##     xx <- fKBix(2.8e-14, 1, 0.667, -1575, data_df$Temp)
  ## ------------------------------------------------------------
  aa <- as.data.frame(aa)
  t0 <- as.data.frame(t0)
  nn <- as.data.frame(nn)
  ea.r <- as.data.frame(ea.r)
  temp <- as.data.frame(temp)
  nr <- nrow(aa)
  if (nr != nrow(t0) || nr != nrow(nn) || nr != nrow(ea.r)) {
    stop("INPUT ERROR: mismatched kinetic parameters")
  }
  ## rate coefficient (expanded Arrhenius)
  df.coeff <- matrix(nrow = nrow(temp), ncol = nr)
  for (k in 1:ncol(df.coeff)) {
    for (t in 1:nrow(df.coeff)) {
      df.coeff[t,k] <- aa[k,1] * (temp[t,1] / t0[k,1])^nn[k,1] * exp(ea.r[k,1] / temp[t,1])
    }
  }
  ## output data.frame
  df.out <- data.frame(df.coeff, temp)
  colnames(df.out) <- c(paste0("k", seq_len(nr)), "Temp")
  return(df.out)
}

fKTer <- function(k.zero, k.inf, fc, temp, press, ref.fc="iupac") {
  ## Calculate the rate coefficient (cm3 molecule-1 s-1) of a
  ## termolecular reaction over a range of temperatures and pressures,
  ## using the Troe parametrization:
  ##    k = F (k0 * ki) / (k0 + ki)
  ##
  ## NB: use fKBi() or fKBix() to calculate the low and high pressure
  ##     limit rate coefficients `k.zero` and `k.inf`.
  ##
  ## INPUT:
  ##     k.zero = low pressure limit rate coefficient (cm3 molecule-1 s-1)
  ##     k.inf = high pressure limit rate coefficient (cm3 molecule-1 s-1)
  ##     fc = falloff curve factor
  ##     temp = temperature (K)
  ##     press = pressure (Pa)
  ##     ref.fc = "iupac" OR "jpl" convention     [ OPTIONAL, default="iupac"]
  ## OUTPUT:
  ##     df.out = data.frame ( rate coeff, temperature, pressure )
  ## EXAMPLE:
  ##     xx <- fKTer(fKBix(3.6e-30, 300, -4.1, 0, data_df$Temp)$k1,
  ##                 fKBix(1.9e-12, 300, 0.2, 0, data_df$Temp)$k1,
  ##                 0.35, data_df$Temp, data_df$Press, "iupac")
  ## ------------------------------------------------------------
  k.zero <- as.data.frame(k.zero)
  k.inf <- as.data.frame(k.inf)
  fc <- as.data.frame(fc)
  temp <- as.data.frame(temp)
  press <- as.data.frame(press)
  if (nrow(k.zero) != nrow(k.inf) || nrow(k.zero) != nrow(fc)) {
    stop("INPUT ERROR: mismatched kinetic parameters")
  }
  if (nrow(k.zero) != nrow(temp)) {
    stop("INPUT ERROR: mismatched temperature and kinetic parameters")
  }
  ## number density of air
  m.air <- fAirND(temp[,1], press[,1])$M
  ## falloff curve parameters
  if (ref.fc == "iupac") {   # IUPAC convention
    fc <- fc
    nn <- 0.75 - 1.27 * log10(fc)
  } else if (ref.fc == "jpl") {   # JPL convention
    fc <- 0.6
    nn <- 1.0
  }
  ## broadening factor (F)
  kr <-  (k.zero * m.air) / k.inf
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
