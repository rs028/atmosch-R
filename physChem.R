### ---------------------------------------------------------------- ###
### functions for physical chemistry:
### - fGasLaw() : ideal gas law
### - fKBi()    : rate coefficient of bimolecular reaction (standard)
### - fKBix()   : rate coefficient of bimolecular reaction (expanded)
### - fKTer()   : rate coefficient of termolecular reaction
### - fLifeT()  : chemical lifetime and half-life
###
### version 1.9, Nov 2016
### author: RS
### ---------------------------------------------------------------- ###

fGasLaw <- function(press, volum, n.mol, temp) {
  ## solve the equation of state of a gas using the ideal gas law:
  ##    PV = nRT
  ##
  ## input:     [ "?" for unknown variable ]
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
  if (all(press == "?")) {         # pressure
    var.x <- (n.mol * r.gas * temp) / volum
    str.x <- "Pressure"
  } else if (all(volum == "?")) {  # volume
    var.x <- (n.mol * r.gas * temp) / press
    str.x <- "Volume"
  } else if (all(n.mol == "?")) {  # moles
    var.x <- (press * volum) / (r.gas * temp)
    str.x <- "n.Mol"
  } else if (all(temp == "?")) {   # temperature
    var.x <- (press * volum) / (n.mol * r.gas)
    str.x <- "Temperature"
  }
  ## output data.frame
  df.out <- data.frame(var.x)
  colnames(df.out) <- str.x
  return(df.out)
}

fKBi <- function(aa, ea.r, temp) {
  ## calculate the rate coefficient (cm3 molecule-1 s-1) of a
  ## bimolecular reaction at given and standard temperature using the
  ## Arrhenius equation:
  ##    k = A exp(-Ea/RT)
  ##
  ## input:
  ##     aa = pre-exponential factor (cm3 molecule-1 s-1)
  ##     ea.r = -Ea/R (J mol-1 / J K-1 mol-1)
  ##     temp = temperature (K)
  ## output:
  ##     df.out = data.frame ( kt = rate coefficient,
  ##                           k298 = standard rate coefficient )
  ## ------------------------------------------------------------
  if (is.list(temp) | length(temp) > 1) {
    stop("input not valid")
  }
  ## standard Arrhenius
  k.gas <- aa * exp(ea.r / temp)
  k.std <- aa * exp(ea.r / 298)
  ## output data.frame
  df.out <- data.frame(k.gas, k.std)
  colnames(df.out) <- c("kt", "k298")
  return(df.out)
}

fKBix <- function(aa, t0, nn, ea.r, temp) {
  ## calculate the rate coefficient (cm3 molecule-1 s-1) of a
  ## bimolecular reaction at given and standard temperature using the
  ## expanded Arrhenius equation:
  ##    k = A(T/T0)^n exp(-Ea/RT)
  ##
  ## input:
  ##     aa = pre-exponential factor (cm3 molecule-1 s-1)
  ##     t0 = reference temperature (K)
  ##     nn = reference temperature exponent
  ##     ea.r = -Ea/R (J mol-1 / J K-1 mol-1)
  ##     temp = temperature (K)
  ## output:
  ##     df.out = data.frame ( kt = rate coefficient,
  ##                           k298 = standard rate coefficient )
  ## ------------------------------------------------------------
  if (is.list(temp) | length(temp) > 1) {
    stop("input not valid")
  }
  ## expanded Arrhenius
  k.gas <- (aa * (temp / t0) ^ nn) * exp(ea.r / temp)
  k.std <- (aa * (298 / t0) ^ nn) * exp(ea.r / 298)
  ## output data.frame
  df.out <- data.frame(k.gas, k.std)
  colnames(df.out) <- c("kt", "k298")
  return(df.out)
}

fKTer <- function(k.zero, k.inf, fc, m.air, refp) {
  ## calculate the rate coefficient of a termolecular reaction at
  ## given and standard temperature using the Lindemann-Hinshelwood
  ## expression:
  ##    k = F (k0 * ki) / (k0 + ki)
  ##
  ## NB: use fKBi() and fKBix() to calculate k.zero and k.inf
  ##     use fAirND() to calculate m.air
  ##
  ## input:
  ##     k.zero = low pressure limit rate coefficient
  ##     k.inf = high pressure limit rate coefficient
  ##     fc = Fc factor
  ##     temp = air density (molecule cm-3)
  ##     refp = "iupac" OR "jpl" fitting
  ## output:
  ##     df.out = data.frame ( kt = rate coefficient,
  ##                           k298 = standard rate coefficient )
  ## ------------------------------------------------------------
  if (is.list(m.air) | length(m.air) > 1) {
    stop("input not valid")
  }
  ## air number density
  m.temp <- m.air
  m.298 <- fAirND(298, 101325)$M
  m.df <- cbind(m.temp, m.298)
  ## fall-off curve parameters
  if (refp == "iupac") {        # IUPAC fitting
    fc <- fc
    nn <- 0.75 - 1.27 * log10(fc)
  } else  if (refp == "jpl") {  # JPL fitting
    fc <- 0.6
    nn <- 1.0
  }
  ## broadening factor F
  kr <- (k.zero * m.df) / k.inf
  fr <- (log10(kr) / nn) ^ 2
  ff <- 10 ^ (log10(fc) / (1 + fr))
  ## effective second-order rate coefficient
  k.gas <- ff * k.inf * (kr / (1 + kr))
  ## output data.frame
  df.out <- data.frame(k.gas)
  colnames(df.out) <- c("kt", "k298")
  return(df.out)
}

fLifeT <- function(k.gas, c.gas) {
  ## calculate the chemical lifetime (s) and the half-life (s) of a
  ## gas with respect to a first or second order process
  ##
  ## input:
  ##     k.gas = rate coefficient (cm3 molecule-1 s-1 OR s-1)
  ##     c.gas = concentration of reactant gas (molecule cm-3) OR
  ##             1 (for a first-order process, with k.gas in s-1)
  ## output:
  ##     df.out = data.frame( Life.T = lifetime,
  ##                          Half.L = half-life )
  ## ------------------------------------------------------------
  ## pseudo first-order rate coefficient
  kp <- k.gas * c.gas
  ## lifetime and half-life (in seconds)
  tau.g <- 1 / kp
  half.g <- tau.g * log(2)
  ## output data.frame
  df.out <- data.frame(tau.g, half.g)
  colnames(df.out) <- c("Life.T", "Half.L")
  return(df.out)
}
