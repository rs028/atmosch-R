### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### Reference data for physics and chemistry :
### - fConstant() : physical/chemical constants
### - fPeriodic() : periodic table
###
### Data from NIST Standard Reference Database:
###   https://www.nist.gov/pml/fundamental-physical-constants
###   https://www.nist.gov/pml/elemental-data-index
###
### version 3.6, Feb 2024
### author: RS
### ---------------------------------------------------------------- ###

fConstant <- function(symb.in) {
  ## Return name, symbol, value, unit of selected physical/chemical
  ## constants:
  ## * Avogadro number            = "Na"
  ## * Boltzmann constant         = "kB"
  ## * molar gas constant         = "R"
  ## * Planck constant            = "h"
  ## * Stefan-Boltzmann constant  = "kSB"
  ## * Wien displacement constant = "Wb"
  ## * standard gravity           = "g0"
  ## * speed of light             = "c0"
  ## * atomic mass constant       = "mu"
  ##
  ## INPUT:
  ##     symb.in = symbol of constant
  ## OUTPUT:
  ##     data.out = data.frame ( Name = name of constant,
  ##                             Symbol = symbol of constant,
  ##                             Value = value of constant,
  ##                             Unit = unit of constant )
  ## EXAMPLE:
  ##     xx <- fConstant("Na")
  ## ------------------------------------------------------------
  data.db <- list(
    Na = c("Avogadro number", 6.022140857e+23, "mol-1"),
    kB = c("Boltzmann constant", 1.38064852e-23, "J K-1"),
    R = c("molar gas constant", 8.3144598, "J mol-1 K-1"),
    h = c("Planck constant", 6.62607004e-34, "J s"),
    kSB = c("Stefan-Boltzmann constant", 5.670367e-08, "W m-2 K-4"),
    Wb = c("Wien displacement constant", 2.8977729e-03, "m K"),
    g0 = c("standard gravity", 9.80665, "m s-2"),
    c0 = c("speed of light", 299792458, "m s-1"),
    mu = c("atomic mass constant", 1.660539040e-24, "g")
  )
  ## select physical/chemical constant
  if (symb.in %in% names(data.db)) {
    data.out <- data.frame(Name = data.db[[symb.in]][1],
                           Symbol = symb.in,
                           Value = as.numeric(data.db[[symb.in]][2]),
                           Unit = data.db[[symb.in]][3])
  } else {
    data.out <- data.frame(Name = NA,
                           Symbol = symb.in,
                           Value = NA,
                           Unit = NA)
    stop("INPUT ERROR: symbol not found")
  }
  return(data.out)
}

fPeriodic <- function(symb.in) {
  ## Return name, symbol, atomic number, atomic weight of naturally
  ## occurring chemical elements (up to Uranium).
  ##
  ## INPUT:
  ##     symb.in = symbol of element
  ## OUTPUT:
  ##     data.out = data.frame ( Name = name of element,
  ##                             Symbol = symbol of element,
  ##                             Atomic.N = atomic number of element,
  ##                             Atomic.W = atomic weight of element )
  ## EXAMPLE:
  ##     xx <- fPeriodic("Cl")
  ## ------------------------------------------------------------
  data.db <- list(
  H  = c("Hydrogen", 1, 1.00794),
  He = c("Helium", 2, 4.002602),
  Li = c("Lithium", 3, 6.941),
  Be = c("Beryllium", 4, 9.012182),
  B  = c("Boron", 5, 10.811),
  C  = c("Carbon", 6, 12.0107),
  N  = c("Nitrogen", 7, 14.0067),
  O  = c("Oxygen", 8, 15.9994),
  F  = c("Fluorine", 9, 18.998403),
  Ne = c("Neon", 10, 20.1797),
  Na = c("Sodium", 11, 22.98977),
  Mg = c("Magnesium", 12, 24.3050),
  Al = c("Aluminium", 13, 26.98154),
  Si = c("Silicon", 14, 28.0855),
  P  = c("Phosphorus", 15, 30.97376),
  S  = c("Sulfur", 16, 32.065),
  Cl = c("Chlorine", 17, 35.453),
  Ar = c("Argon", 18, 39.948),
  K  = c("Potassium", 19, 39.0983),
  Ca = c("Calcium", 20, 40.078),
  Sc = c("Scandium", 21, 44.955912),
  Ti = c("Titanium", 22, 47.867),
  V  = c("Vanadium", 23, 50.9415),
  Cr = c("Chromium", 24, 51.9961),
  Mn = c("Manganese", 25, 54.938045),
  Fe = c("Iron", 26, 55.845),
  Co = c("Cobalt", 27, 58.933195),
  Ni = c("Nickel", 28, 58.6934),
  Cu = c("Copper", 29, 63.546),
  Zn = c("Zinc", 30, 65.38),
  Ga = c("Gallium", 31, 69.723),
  Ge = c("Germanium", 32, 72.64),
  As = c("Arsenic", 33, 74.9216),
  Se = c("Selenium", 34, 78.96),
  Br = c("Bromine", 35, 79.904),
  Kr = c("Krypton", 36, 83.798),
  Rb = c("Rubidium", 37, 85.4678),
  Sr = c("Strontium", 38, 87.62),
  Y  = c("Yttrium", 39, 88.90585),
  Zr = c("Zirconium", 40, 91.224),
  Nb = c("Niobium", 41, 92.90638),
  Mo = c("Molybdenum", 42, 95.96),
  Tc = c("Technetium", 43, 98),    # no stable isotopes
  Ru = c("Ruthenium", 44, 101.07),
  Rh = c("Rhodium", 45, 102.9055),
  Pd = c("Palladium", 46, 106.42),
  Ag = c("Silver", 47, 107.8682),
  Cd = c("Cadmium", 48, 112.411),
  In = c("Indium", 49, 114.818),
  Sn = c("Tin", 50, 118.710),
  Sb = c("Antimony", 51, 121.760),
  Te = c("Tellurium", 52, 127.60),
  I  = c("Iodine", 53, 126.90447),
  Xe = c("Xenon", 54, 131.293),
  Cs = c("Caesium", 55, 132.9054),
  Ba = c("Barium", 56, 137.327),
  La = c("Lanthanum", 57, 138.90547),
  Ce = c("Cerium", 58, 140.116),
  Pr = c("Praseodymium", 59, 140.90765),
  Nd = c("Neodymium", 60, 144.242),
  Pm = c("Promethium", 61, 145),   # no stable isotopes
  Sm = c("Samarium", 62, 150.36),
  Eu = c("Europium", 63, 151.964),
  Gd = c("Gadolinium", 64, 157.25),
  Tb = c("Terbium", 65, 158.92535),
  Dy = c("Dysprosium", 66, 162.50),
  Ho = c("Holmium", 67, 164.93032),
  Er = c("Erbium", 68, 167.259),
  Tm = c("Thulium", 69, 168.93421),
  Yb = c("Ytterbium", 70, 173.054),
  Lu = c("Lutetium", 71, 174.9668),
  Hf = c("Hafnium", 72, 178.49),
  Ta = c("Tantalum", 73, 180.94788),
  W  = c("Tungsten", 74, 183.84),
  Re = c("Rhenium", 75, 186.207),
  Os = c("Osmium", 76, 190.23),
  Ir = c("Iridium", 77, 192.217),
  Pt = c("Platinum", 78, 195.084),
  Au = c("Gold", 79, 196.966569),
  Hg = c("Mercury", 80, 200.59),
  Tl = c("Thallium", 81, 204.3833),
  Pb = c("Lead", 82, 207.2),
  Bi = c("Bismuth", 83, 208.9804),
  Po = c("Polonium", 84, 209),     # no stable isotopes
  At = c("Astatine", 85, 210),     # no stable isotopes
  Rn = c("Radon", 86, 222),        # no stable isotopes
  Fr = c("Francium", 87, 223),     # no stable isotopes
  Ra = c("Radium", 88, 226),       # no stable isotopes
  Ac = c("Actinium", 89, 227),     # no stable isotopes
  Th = c("Thorium", 90, 232.03806),
  Pa = c("Protactinium", 91, 231.0359),
  U  = c("Uranium", 92, 238.02891)
  )
  ## select chemical element
  if (symb.in %in% names(data.db)) {
    data.out <- data.frame(Name = data.db[[symb.in]][1],
                           Symbol = symb.in,
                           Atomic.N = as.numeric(data.db[[symb.in]][2]),
                           Atomic.W = as.numeric(data.db[[symb.in]][3]))
  } else {
    data.out <- data.frame(Name = NA,
                           Symbol = symb.in,
                           Atomic.N = NA,
                           Atomic.W = NA)
    stop("INPUT ERROR: symbol not found")
  }
  return(data.out)
}
