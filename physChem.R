### ---------------------------------------------------------------- ###
### functions for physical chemistry:
### - fGasLaw() : ideal gas law
### - fKBi()    : rate coefficient of bimolecular reaction (standard)
### - fKBix()   : rate coefficient of bimolecular reaction (expanded)
### - fKTer()   : rate coefficient of termolecular reaction
### - fLifeT()  : chemical lifetime and half-life
###
### version 2.2, Dec 2017
### author: RS
### ---------------------------------------------------------------- ###

fGasLaw <- function(press, vol, mol, temp) {
  ## solve the equation of state of a gas using the ideal gas law:
  ##    PV = nRT
  ##
  ## input:     [ "?" for unknown variable ]
  ##     press = pressure (Pa)
  ##     vol = volume (m3 = 1000 L)
  ##     mol = number of moles
  ##     temp = temperature (K)
  ## output:
  ##     df.out = data.frame( unknown variable )
  ## ------------------------------------------------------------
  ## gas constant
  r.gas <- fConstant("R")$Value
  ## calculate unknown variable
  if (all(press == "?")) {       # pressure
    var.x <- (mol * r.gas * temp) / vol
    str.x <- "Press"
  } else if (all(vol == "?")) {  # volume
    var.x <- (mol * r.gas * temp) / press
    str.x <- "Vol"
  } else if (all(mol == "?")) {  # moles
    var.x <- (press * vol) / (r.gas * temp)
    str.x <- "Mol"
  } else if (all(temp == "?")) { # temperature
    var.x <- (press * vol) / (mol * r.gas)
    str.x <- "Temp"
  }
  ## output data.frame
  df.out <- data.frame(var.x)
  colnames(df.out) <- str.x
  return(df.out)
}

fKBi <- function(aa, ea.r, temp=298) {
  ## calculate the rate coefficient (cm3 molecule-1 s-1) of one or
  ## more bimolecular reactions at given temperature using the
  ## Arrhenius equation:
  ##    k = A * exp(-Ea/RT)
  ##
  ## input:
  ##     aa = pre-exponential factor (cm3 molecule-1 s-1)
  ##     ea.r = -Ea/R (J mol-1 / J K-1 mol-1)
  ##     temp = temperature (K)
  ## output:
  ##     df.out = data.frame ( rate coeff 1, rate coeff 2, ...,
  ##                           temperature )
  ## ------------------------------------------------------------
  aa <- as.matrix(aa)
  ea.r <- as.matrix(ea.r)
  temp <- as.matrix(temp)
  ## calculate rate coefficients (standard Arrhenius)
  kt <- sapply(temp, function(x) aa * exp(ea.r / x))
  kt <- as.matrix(kt)
  ## output data.frame
  if (dim(kt)[2] == dim(temp)[1]) {
    df.out <- data.frame(t(kt), temp)
  } else {
    df.out <- data.frame(kt, temp)
  }
  nr <- ncol(df.out) - 1
  colnames(df.out) <- c(paste("k", seq(1, nr, by=1), sep=""), "Temp")
  return(df.out)
}

fKBix <- function(aa, t0, nn, ea.r, temp=298) {
  ## calculate the rate coefficient (cm3 molecule-1 s-1) of one or
  ## more bimolecular reactions at given temperature using the
  ## expanded Arrhenius equation:
  ##    k = A * (T/T0)^n * exp(-Ea/RT)
  ##
  ## input:
  ##     aa = pre-exponential factor (cm3 molecule-1 s-1)
  ##     t0 = reference temperature (K)         -> 1 if not used
  ##     nn = reference temperature exponent    -> 0 if not used
  ##     ea.r = -Ea/R (J mol-1 / J K-1 mol-1)   -> 0 if not used
  ##     temp = temperature (K)
  ## output:
  ##     df.out = data.frame ( rate coeff 1, rate coeff 2, ...,
  ##                           temperature )
  ## ------------------------------------------------------------
  aa <- as.matrix(aa)
  t0 <- as.matrix(t0)
  nn <- as.matrix(nn)
  ea.r <- as.matrix(ea.r)
  temp <- as.matrix(temp)
  ## calculate rate coefficients (expanded Arrhenius)
  kt <- sapply(temp, function(x) aa * (x / t0)^nn * exp(ea.r / x))
  kt <- as.matrix(kt)
  ## output data.frame
  if (dim(kt)[2] == dim(temp)[1]) {
    df.out <- data.frame(t(kt), temp)
  } else {
    df.out <- data.frame(kt, temp)
  }
  nr <- ncol(df.out) - 1
  colnames(df.out) <- c(paste("k", seq(1, nr, by=1), sep=""), "Temp")
  return(df.out)
}

fKTer <- function(k.zero, k.inf, fc, temp, press, refp) {
  ## calculate the rate coefficient of a termolecular reaction at
  ## given and standard temperature and pressure using the
  ## Lindemann-Hinshelwood expression:
  ##    k = F (k0 * ki) / (k0 + ki)
  ##
  ## NB: use fKBi() and fKBix() to calculate k.zero and k.inf
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
  if (is.list(temp) | length(temp) > 1) {
    stop("INPUT ERROR: only one temperature value allowed")
  }
  if (is.list(press) | length(press) > 1) {
    stop("INPUT ERROR: only one pressure value allowed")
  }
  ## air number density
  m.temp <- fAirND(temp, press)$M
  m.298 <- fAirND(298, 101325)$M
  ## fall-off curve parameters
  if (refp == "iupac") {        # IUPAC fitting
    fc <- fc
    nn <- 0.75 - 1.27 * log10(fc)
  } else  if (refp == "jpl") {  # JPL fitting
    fc <- 0.6
    nn <- 1.0
  }
  ## broadening factor F
  kr1 <- (k.zero[1] * m.temp) / k.inf[1]
  kr2 <- (k.zero[2] * m.298) / k.inf[2]
  ff1 <- 10^( log10(fc) / (1 + (log10(kr1) / nn)^2) )
  ff2 <- 10^( log10(fc) / (1 + (log10(kr2) / nn)^2) )
  ## effective second-order rate coefficient
  k.gas <- ff1 * k.inf[1] * (kr1 / (1 + kr1))
  k.std <- ff2 * k.inf[2] * (kr2 / (1 + kr2))
  ## output data.frame
  df.out <- data.frame(k.gas, k.std)
  colnames(df.out) <- c("kt", "k298")
  return(df.out)
}

fLifeT <- function(k.gas, c.gas) {
  ## calculate the chemical lifetime (s) and the half-life (s) of a
  ## gas with respect to a first or second order process
  ##
  ## NB: see documentation of fKBi(), fKBix(), fKTer()
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
