### ---------------------------------------------------------------- ###
### functions for physical chemistry:
###  1. (gas kinetics)
###  2. ideal gas law
###  3. (van der Waals)
###  4. rate coefficient of bimolecular reaction (standard)
###  5. rate coefficient of bimolecular reaction (expanded)
###  6. rate coefficient of termolecular reaction
###  7. chemical lifetime and half-life
###
### version 1.8, Feb 2016
### author: RS
### ---------------------------------------------------------------- ###

fGasLaw <- function(press, volum, n.mol, temp) {
  ## 2. solve the equation of state of an ideal gas using the ideal
  ## gas law:
  ##    PV = nRT
  ##
  ## NB: use "?" for unknown variable
  ##
  ## input:
  ##     press = pressure (Pa)
  ##     volum = volume (m3 = 1000 L)
  ##     n.mol = number of moles
  ##     temp = temperature (K)
  ## output:
  ##     df.out = data.frame( unknown variable )
  ## ------------------------------------------------------------
  ## gas constant
  r.gas <- fConstant("R")$Value
  ## calculate unknown variable
  if (press == "?") {         # pressure
    var.x <- (n.mol * r.gas * temp) / volum
    str.x <- "Pressure"
  } else if (volum == "?") {  # volume
    var.x <- (n.mol * r.gas * temp) / press
    str.x <- "Volume"
  } else if (n.mol == "?") {  # moles
    var.x <- (press * volum) / (r.gas * temp)
    str.x <- "Mole"
  } else if (temp == "?") {   # temperature
    var.x <- (press * volum) / (n.mol * r.gas)
    str.x <- "Temperature"
  }
  ## output data.frame
  df.out <- data.frame(var.x)
  colnames(df.out) <- str.x
  return(df.out)
}

fKBi <- function(pfac, ea.r, temp) {
  ## 4. calculate the rate coefficient (cm3 molecule-1 s-1) of a
  ## bimolecular reaction at given and standard temperature using the
  ## standard Arrhenius equation:
  ##    k = A exp(-Ea/RT)
  ##
  ## input:
  ##     pfac = pre-exponential factor (cm3 molecule-1 s-1)
  ##     ea.r = -Ea/R (J mol-1 / J K-1 mol-1)
  ##     temp = temperature (K)
  ## output:
  ##     df.out = data.frame ( kt = rate coefficient,
  ##                           k298 = standard rate coefficient )
  ## ------------------------------------------------------------
  k.gas <- pfac * exp(ea.r / temp)
  k.std <- pfac * exp(ea.r / 298)
  ## output data.frame
  df.out <- data.frame(k.gas, k.std)
  colnames(df.out) <- c("kt", "k298")
  return(df.out)
}

fKBix <- function(pfac, temp0, nn, ea.r, temp) {
  ## 5. calculate the rate coefficient (cm3 molecule-1 s-1) of a
  ## bimolecular reaction at given and standard temperature using the
  ## expanded Arrhenius equation:
  ##    k = A(T/T0)^n exp(-Ea/RT)
  ##
  ## input:
  ##     pfac = pre-exponential factor (cm3 molecule-1 s-1)
  ##     temp0 = reference temperature (K)
  ##     nn = reference temperature exponent
  ##     ea.r = -Ea/R (J mol-1 / J K-1 mol-1)
  ##     temp = temperature (K)
  ## output:
  ##     df.out = data.frame ( kt = rate coefficient,
  ##                           k298 = standard rate coefficient )
  ## ------------------------------------------------------------
  k.gas <- (pfac * (temp / temp0) ^ nn) * exp(ea.r / temp)
  k.std <- (pfac * (298 / temp0) ^ nn) * exp(ea.r / 298)
  ## output data.frame
  df.out <- data.frame(k.gas, k.std)
  colnames(df.out) <- c("kt", "k298")
  return(df.out)
}

fKTer <- function(k.zero, k.inf, fc, temp, press, refp) {
  ## 6. calculate rate coefficient of a termolecular reaction using
  ## the Lindemann-Hinshelwood expression:
  ##    k = F (k0 * ki) / (k0 + ki)
  ##
  ## input:
  ##     k.zero = low pressure limit rate coefficient
  ##     k.inf = high pressure limit rate coefficient
  ##     fc = Fc factor
  ##     temp = temperature (K)
  ##     press = pressure (Pa)
  ##     refp = "iupac" OR "jpl" fitting
  ## output:
  ##     df.out = data.frame ( kt = rate coefficient,
  ##                           k298 = standard rate coefficient )
  ## ------------------------------------------------------------
  ## air number density
  m.tmp <- fAirND(temp, press)$M
  m.std <- fAirND(298, press)$M
  ## fall-off curve parameters
  if (refp == "iupac") {        # IUPAC fitting
    fc <- fc
    nn <- 0.75 - 1.27 * log10(fc)
  } else  if (refp == "jpl") {  # JPL fitting
    fc <- 0.6
    nn <- 1.0
  }
  ## broadening factor F
  kr <- (k.zero * m.air) / k.inf
  fr <- (log10(kr) / nn) ^ 2
  ff <- 10 ^ (log10(fc) / (1 + fr))
  ## effective second-order rate coefficient
  k.gas <- ff * k.inf * (kr / (1 + kr))
  print(k.gas)
  ## output data.frame
  ## df.out <- data.frame(k.gas, k.std)
  ## colnames(df.out) <- c("kt", "k298")
  ## return(df.out)
}

fLifeT <- function(k.gas, c.gas) {
  ## 7. calculate the chemical lifetime (s) and the half-life (s) of a
  ## gas with respect to a first or second order process
  ##
  ## input:
  ##     k.gas = rate coefficient (cm3 molecule-1 s-1 OR s-1)
  ##     c.gas = concentration of reactant gas (molecule cm-3) OR
  ##             1 (for a first-order process, where k.gas = s-1)
  ## output:
  ##     df.out = data.frame( LifeT = lifetime,
  ##                          HalfT = half-life )
  ## ------------------------------------------------------------
  ## pseudo first-order rate coefficient
  kp <- k.gas * c.gas
  ## lifetime and half-life (in seconds)
  tau.g <- 1 / kp
  half.g <- tau.g * log(2)
  ## output data.frame
  df.out <- data.frame(tau.g, half.g)
  colnames(df.out) <- c("LifeT", "HalfT")
  return(df.out)
}
