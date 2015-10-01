### ---------------------------------------------------------------- ###
### reference data for physics and chemistry :
###  1. physical/chemical constants
###  2. periodic table
###
### data from NIST Standard Reference Database:
###    http://www.nist.gov/pml/data/physicalconst.cfm
###    http://www.nist.gov/pml/data/edi.cfm
###
### version 3.2, Oct 2015
### author: RS
### ---------------------------------------------------------------- ###

fConstant <- function(data.in) {
  ## 1. return name, symbol, value, unit of a physical/chemical
  ## constant
  ## - Avogadro number           : "Na"
  ## - Boltzmann constant        : "kB"
  ## - gas constant              : "R"
  ## - Planck constant           : "h"
  ## - Stefan-Boltzmann constant : "kSB"
  ## - standard gravity          : "g0"
  ## - speed of light            : "c0"
  ##
  ## input:
  ##     data.in = symbol of physical/chemical constant
  ## output:
  ##     data.out = data.frame ( Name = name of constant,
  ##                             Symbol = symbol of constant,
  ##                             Value = value of constant,
  ##                             Unit = unit of constant )
  ## ------------------------------------------------------------
  k01 <- c("Avogadro number", "Na", 6.02214129e+23, "mol-1")
  k02 <- c("Boltzmann constant", "kB", 1.3806488e-23, "J K-1")
  k03 <- c("gas constant", "R", 8.3144621, "J mol-1 K-1")
  k04 <- c("Planck constant", "h", 6.62606957e-34, "J s")
  k05 <- c("Stefan-Boltzmann constant", "kSB", 5.670373e-08, "W m-2 K-4")
  k06 <- c("standard gravity", "g0", 9.80665, "m s-2")
  k07 <- c("speed of light", "c0", 299792458, "m s-1")
  k.df <- rbind(k01, k02, k03, k04, k05, k06, k07)
  for (i in 1:nrow(k.df)) {
    if (data.in == k.df[i,2]) {
      nn <- as.character(k.df[i,1])  # name
      ss <- as.character(k.df[i,2])  # symbol
      vv <- as.numeric(k.df[i,3])    # value
      uu <- as.character(k.df[i,4])  # unit
    }
  }
  data.out <- data.frame(Name=nn, Symbol=ss, Value=vv, Unit=uu)
  return(data.out)
}

fPeriodic <- function(data.in) {
  ## 2. returns name, symbol, atomic number, atomic weight of a
  ## chemical element (trans-uranium elements not included)
  ## - "H" ... "U"
  ##
  ## input:
  ##     data.in = symbol of chemical element
  ## output:
  ##     data.out = data.frame ( Name = name of element,
  ##                             Symbol = symbol of element,
  ##                             Atomic.N = atomic number of element,
  ##                             Atomic.W = atomic weight of element )
  ## ------------------------------------------------------------
  el01 <- c("Hydrogen", "H", 1, 1.00794)
  el02 <- c("Helium", "He", 2, 4.002602)
  el03 <- c("Lithium", "Li", 3, 6.941)
  el04 <- c("Beryllium", "Be", 4, 9.012182)
  el05 <- c("Boron", "B", 5, 10.811)
  el06 <- c("Carbon", "C", 6, 12.0107)
  el07 <- c("Nitrogen", "N", 7, 14.0067)
  el08 <- c("Oxygen", "O", 8, 15.9994)
  el09 <- c("Fluorine", "F", 9, 18.9984032)
  el10 <- c("Neon", "Ne", 10, 20.1797)
  el11 <- c("Sodium", "Na", 11, 22.98977)
  el12 <- c("Magnesium", "Mg", 12, 24.305)
  el13 <- c("Aluminium", "Al", 13, 26.981538)
  el14 <- c("Silicon", "Si", 14, 28.0855)
  el15 <- c("Phosphorus", "P", 15, 30.973761)
  el16 <- c("Sulfur", "S", 16, 32.065)
  el17 <- c("Chlorine", "Cl", 17, 35.453)
  el18 <- c("Argon", "Ar", 18, 39.948)
  el19 <- c("Potassium", "K", 19, 39.0983)
  el20 <- c("Calcium", "Ca", 20, 40.078)
  el21 <- c("Scandium", "Sc", 21, 44.955912)
  el22 <- c("Titanium", "Ti", 22, 47.867)
  el23 <- c("Vanadium", "V", 23, 50.9415)
  el24 <- c("Chromium", "Cr", 24, 51.9961)
  el25 <- c("Manganese", "Mn", 25, 54.938045)
  el26 <- c("Iron", "Fe", 26, 55.845)
  el27 <- c("Cobalt", "Co", 27, 58.933195)
  el28 <- c("Nickel", "Ni", 28, 58.6934)
  el29 <- c("Copper", "Cu", 29, 63.546)
  el30 <- c("Zinc", "Zn", 30, 65.38)
  el31 <- c("Gallium", "Ga", 31, 69.723)
  el32 <- c("Germanium", "Ge", 32, 72.64)
  el33 <- c("Arsenic", "As", 33, 74.9216)
  el34 <- c("Selenium", "Se", 34, 78.96)
  el35 <- c("Bromine", "Br", 35, 79.904)
  el36 <- c("Krypton", "Kr", 36, 83.798)
  el37 <- c("Rubidium", "Rb", 37, 85.4678)
  el38 <- c("Strontium", "Sr", 38, 87.62)
  el39 <- c("Yttrium", "Y", 39, 88.90585)
  el40 <- c("Zirconium", "Zr", 40, 91.224)
  el41 <- c("Niobium", "Nb", 41, 92.90638)
  el42 <- c("Molybdenum", "Mo", 42, 95.96)
  el43 <- c("Technetium", "Tc", 43, 98)
  el44 <- c("Ruthenium", "Ru", 44, 101.07)
  el45 <- c("Rhodium", "Rh", 45, 102.9055)
  el46 <- c("Palladium", "Pd", 46, 106.42)
  el47 <- c("Silver", "Ag", 47, 107.8682)
  el48 <- c("Cadmium", "Cd", 48, 112.411)
  el49 <- c("Indium", "In", 49, 114.818)
  el50 <- c("Tin", "Sn", 50, 118.71)
  el51 <- c("Antimony", "Sb", 51, 121.76)
  el52 <- c("Tellurium", "Te", 52, 127.6)
  el53 <- c("Iodine", "I", 53, 126.90447)
  el54 <- c("Xenon", "Xe", 54, 131.293)
  el55 <- c("Caesium", "Cs", 55, 132.9054519)
  el56 <- c("Barium", "Ba", 56, 137.327)
  el57 <- c("Lanthanum", "La", 57, 138.90547)
  el58 <- c("Cerium", "Ce", 58, 140.116)
  el59 <- c("Praseodymium", "Pr", 59, 140.90765)
  el60 <- c("Neodymium", "Nd", 60, 144.242)
  el61 <- c("Promethium", "Pm", 61, 145)
  el62 <- c("Samarium", "Sm", 62, 150.36)
  el63 <- c("Europium", "Eu", 63, 151.964)
  el64 <- c("Gadolinium", "Gd", 64, 157.25)
  el65 <- c("Terbium", "Tb", 65, 158.92535)
  el66 <- c("Dysprosium", "Dy", 66, 162.5)
  el67 <- c("Holmium", "Ho", 67, 164.93032)
  el68 <- c("Erbium", "Er", 68, 167.259)
  el69 <- c("Thulium", "Tm", 69, 168.93421)
  el70 <- c("Ytterbium", "Yb", 70, 173.054)
  el71 <- c("Lutetium", "Lu", 71, 174.9668)
  el72 <- c("Hafnium", "Hf", 72, 178.49)
  el73 <- c("Tantalum", "Ta", 73, 180.94788)
  el74 <- c("Tungsten", "W", 74, 183.84)
  el75 <- c("Rhenium", "Re", 75, 186.207)
  el76 <- c("Osmium", "Os", 76, 190.23)
  el77 <- c("Iridium", "Ir", 77, 192.217)
  el78 <- c("Platinum", "Pt", 78, 195.084)
  el79 <- c("Gold", "Au", 79, 196.966569)
  el80 <- c("Mercury", "Hg", 80, 200.59)
  el81 <- c("Thallium", "Tl", 81, 204.3833)
  el82 <- c("Lead", "Pb", 82, 207.2)
  el83 <- c("Bismuth", "Bi", 83, 208.9804)
  el84 <- c("Polonium", "Po", 84, 209)
  el85 <- c("Astatine", "At", 85, 210)
  el86 <- c("Radon", "Rn", 86, 222)
  el87 <- c("Francium", "Fr", 87, 223)
  el88 <- c("Radium", "Ra", 88, 226)
  el89 <- c("Actinium", "Ac", 89, 227)
  el90 <- c("Thorium", "Th", 90, 232.03806)
  el91 <- c("Protactinium", "Pa", 91, 231.03588)
  el92 <- c("Uranium", "U", 92, 238.02891)
  el.df <- rbind(el01, el02, el03, el04, el05, el06, el07, el08, el09,
                 el10, el11, el12, el13, el14, el15, el16, el17, el18,
                 el19, el20, el21, el22, el23, el24, el25, el26, el27,
                 el28, el29, el30, el31, el32, el33, el34, el35, el36,
                 el37, el38, el39, el40, el41, el42, el43, el44, el45,
                 el46, el47, el48, el49, el50, el51, el52, el53, el54,
                 el55, el56, el57, el58, el59, el60, el61, el62, el63,
                 el64, el65, el66, el67, el68, el69, el70, el71, el72,
                 el73, el74, el75, el76, el77, el78, el79, el80, el81,
                 el82, el83, el84, el85, el86, el87, el88, el89, el90,
                 el91, el92)
  for (i in 1:nrow(el.df)) {
    if (data.in == el.df[i,2]) {
      nn <- as.character(el.df[i,1])  # name
      ss <- as.character(el.df[i,2])  # symbol
      an <- as.numeric(el.df[i,3])    # atomic number
      aw <- as.numeric(el.df[i,4])    # atomic weight
    }
  }
  data.out <- data.frame(Name=nn, Symbol=ss, Atomic.N=an,  Atomic.W=aw)
  return(data.out)
}
