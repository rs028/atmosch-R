### ---------------------------------------------------------------- ###
### functions for physical chemistry:
###  2. ideal gas law
###  3. rate coefficient of bimolecular reaction (standard)
###  4. rate coefficient of bimolecular reaction (expanded)
###  5. rate coefficient of termolecular reaction
###  6. chemical lifetime and half-life
###
### version 1.7, Dec 2014
### author: RS
### ---------------------------------------------------------------- ###

fGasLaw <- function(press, volum, n.mol, temp) {
  ## 2. solve the equation of state of an ideal gas using the ideal
  ## gas law:
  ##   PV = nRT
  ##
  ## input ("?" for unknown variable):
  ##       press = pressure (Pa)
  ##       volum = volume (1 m3 = 1000 L)
  ##       n.mol = number of moles
  ##       temp = temperature (K)
  ## output:
  ##        datax = unknown variable
  ## ------------------------------------------------------------
  ## gas constant
  r.gas <- fConstant("R")$Value
  ## calculate unknown variable
  if (press == "?") {         # pressure
    datax <- (n.mol * r.gas * temp) / volum
    datan <- "Pressure"
  } else if (volum == "?") {  # volume
    datax <- (n.mol * r.gas * temp) / press
    datan <- "Volume"
  } else if (n.mol == "?") {  # moles
    datax <- (press * volum) / (r.gas * temp)
    datan <- "Moles"
  } else if (temp == "?") {   # temperature
    datax <- (press * volum) / (n.mol * r.gas)
    datan <- "Temperature"
  }
  ## unknown variable data.frame
  datax <- data.frame(datax)
  colnames(datax) <- datan
  return(datax)
}

fKBi <- function(pfac, ea.r, temp) {
  ## 3. calculate rate coefficient of a bimolecular reaction at given
  ## and standard temperature using the standard Arrhenius equation:
  ##  k = A exp(-Ea/RT)
  ##
  ## input:
  ##    pfac = pre-exponential factor (cm3 molecule-1 s-1)
  ##    ea.r = -Ea / R (J mol-1 / J K-1 mol-1)
  ##    temp = temperature (K)
  ## output:
  ##    df.out = data.frame ( kt = rate coefficient,
  ##                          k298 = standard rate coefficient )
  ## ------------------------------------------------------------
  k.gas <- pfac * exp(ea.r / temp)
  k.std <- pfac * exp(ea.r / 298)
  ## output data.frame
  df.out <- data.frame(kt = k.gas, k298 = k.std)
  return(df.out)
}

fKBix <- function(pfac, temp0, nn, ea.r, temp) {
  ## 4. calculate rate coefficient of a bimolecular reaction at given
  ## and standard temperature using the expanded Arrhenius equation:
  ##  k = A(T/T0)^n exp(-Ea/RT)
  ##
  ## input:
  ##    pfac = pre-exponential factor (cm3 molecule-1 s-1)
  ##    temp0 = reference temperature (K)
  ##    nn = reference temperature exponent (unitless)
  ##    ea.r = -Ea / R (J mol-1 / J K-1 mol-1)
  ##    temp = temperature (K)
  ## output:
  ##    df.out = data.frame ( kt = rate coefficient,
  ##                          k298 = standard rate coefficient )
  ## ------------------------------------------------------------
  k.gas <- (pfac * (temp / temp0) ^ nn) * exp(ea.r / temp)
  k.std <- (pfac * (298 / temp0) ^ nn) * exp(ea.r / 298)
  ## output data.frame
  df.out <- data.frame(kt = k.gas, k298 = k.std)
  return(df.out)
}

fKTer <- function(k.zero, k.inf, fc, temp, press, refp) {
  ## 5. calculate rate coefficient of termolecular reaction using the
  ## Lindemann-Hinshelwood expression:
  ##   k = F (k0 * ki) / (k0 + ki)
  ##
  ## input:
  ##       k.zero = low pressure limit rate coefficient
  ##       k.inf = high pressure limit rate coefficient
  ##       fc = Fc factor
  ##       temp = temperature (K)
  ##       press = pressure (Pa)
  ##       refp = "iupac" OR "jpl" parametrization
  ## output:
  ##        data.frame ( kt = rate coefficient,
  ##                     k298 = rate coefficient at 298 K )
  ## ------------------------------------------------------------
  ## air number density
  m.air <- fAirND(temp, press)$M
  ## falloff parameters
  if (refp == "iupac") {
    fc <- fc
    nn <- 0.75 - 1.27 * log10(fc)
  } else  if (refp == "jpl") {
    fc <- 0.6
    nn <- 1.0
  }
  ## broadening factor
  kr <- (k.zero * m.air) / k.inf
  fr <- (log10(kr) / nn) ^ 2
  ff <- 10 ^ (log10(fc) / (1 + fr))
  ## effective second-order rate coefficient
  k.gas <- ff * k.inf * (kr / (1 + kr))
  ## output data.frame
  df.out <- data.frame(kt = k.gas)
  return(df.out)
}

fLifeT <- function(k.gas, c.gas) {
  ## 6. calculate chemical lifetime (s) and half-life (s) of a gas of
  ## given concentration
  ##
  ## input:
  ##       k.gas = rate coefficient (cm3 molecule-1 s-1 OR s-1)
  ##       c.gas = concentration of gas (molecule cm-3) OR
  ##               0 (for first-order reactions)
  ## output:
  ##        data.frame( LifeT = lifetime,
  ##                    HalfL = half-life )
  ## ------------------------------------------------------------
  ## pseudo first-order rate coefficient
  if (c.gas == 0) {
    kp <- k.gas
  } else {
    kp <- k.gas * c.gas
  }
  ## lifetime and half-life (in seconds)
  tau.g <- 1 / kp
  half.g <- tau.g * log(2)
  ## output data.frame
  df.out <- data.frame(LifeT = tau.g,
                       HalfL = half.g)
  return(df.out)
}
