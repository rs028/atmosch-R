### ---------------------------------------------------------------- ###
### database of physical and chemical reference data:
###  1. physical/chemical constants
###  2. periodic table
###
### data from NIST Standard Reference Database:
###    http://www.nist.gov/pml/data/physicalconst.cfm
###    http://www.nist.gov/pml/data/edi.cfm
###
### input:
###       data.in = symbol of data to retrieve (string)
###
### output:
###        data.out = retrieved data (data.frame)
###
### version 2.4, Jan 2014
### author: RS
### ---------------------------------------------------------------- ###

fConstant <- function(data.in) {
  ## 1. retrieve name, symbol, value, unit of a physical/chemical
  ## constant:
  ##   Avogadro number           -> "Na"
  ##   Boltzmann constant        -> "kB"
  ##   ideal gas constant        -> "R"
  ##   Planck constant           -> "h"
  ##   Stefan-Boltzmann constant -> "kSB"
  ##   standard gravity          -> "g0"
  ## ------------------------------------------------------------
  k01 <- c("Avogadro number", "Na", 6.02214129e+23, "mol-1")
  k02 <- c("Boltzmann constant", "kB", 1.3806488e-23, "J K-1")
  k03 <- c("ideal gas constant", "R", 8.3144621, "J mol-1 K-1")
  k04 <- c("Planck constant", "h", 6.62606957e-34, "J s")
  k05 <- c("Stefan-Boltzmann constant", "kSB", 5.670373e-08, "W m-2 K-4")
  k06 <- c("standard gravity", "g0", 9.80665, "m s-2")
  k.df <- rbind(k01, k02, k03, k04, k05, k06)
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

fPeriodic <- function(data_in) {
  ## 2. retrieve name, atomic number, atomic weight of a chemical
  ## element (trans-uranium elements not included):
  ##   IUPAC element symbol -> "H" to "U"
  ## ------------------------------------------------------------
  elem_name <- c("Hydrogen", "Helium", "Lithium", "Beryllium", "Boron",
                 "Carbon", "Nitrogen", "Oxygen", "Fluorine", "Neon",
                 "Sodium", "Magnesium", "Aluminium", "Silicon", "Phosphorus",
                 "Sulfur", "Chlorine", "Argon", "Potassium", "Calcium",
                 "Scandium", "Titanium", "Vanadium", "Chromium", "Manganese",
                 "Iron", "Cobalt", "Nickel", "Copper", "Zinc",
                 "Gallium", "Germanium", "Arsenic", "Selenium", "Bromine",
                 "Krypton", "Rubidium", "Strontium", "Yttrium", "Zirconium",
                 "Niobium", "Molybdenum", "Technetium", "Ruthenium", "Rhodium",
                 "Palladium", "Silver", "Cadmium", "Indium", "Tin",
                 "Antimony", "Tellurium", "Iodine", "Xenon", "Caesium",
                 "Barium", "Lanthanum", "Cerium", "Praseodymium", "Neodymium",
                 "Promethium", "Samarium", "Europium", "Gadolinium", "Terbium",
                 "Dysprosium", "Holmium", "Erbium", "Thulium", "Ytterbium",
                 "Lutetium", "Hafnium", "Tantalum", "Tungsten", "Rhenium",
                 "Osmium", "Iridium", "Platinum", "Gold", "Mercury",
                 "Thallium", "Lead", "Bismuth", "Polonium", "Astatine",
                 "Radon", "Francium", "Radium", "Actinium", "Thorium",
                 "Protactinium", "Uranium")
  elem_symb <- c("H", "He", "Li", "Be", "B",
                 "C", "N", "O", "F", "Ne",
                 "Na", "Mg", "Al", "Si", "P",
                 "S", "Cl", "Ar", "K", "Ca",
                 "Sc", "Ti", "V", "Cr", "Mn",
                 "Fe", "Co", "Ni", "Cu", "Zn",
                 "Ga", "Ge", "As", "Se", "Br",
                 "Kr", "Rb", "Sr", "Y", "Zr",
                 "Nb", "Mo", "Tc", "Ru", "Rh",
                 "Pd", "Ag", "Cd", "In", "Sn",
                 "Sb", "Te", "I", "Xe", "Cs",
                 "Ba", "La", "Ce", "Pr", "Nd",
                 "Pm", "Sm", "Eu", "Gd", "Tb",
                 "Dy", "Ho", "Er", "Tm", "Yb",
                 "Lu", "Hf", "Ta", "W", "Re",
                 "Os", "Ir", "Pt", "Au", "Hg",
                 "Tl", "Pb", "Bi", "Po", "At",
                 "Rn", "Fr", "Ra", "Ac", "Th",
                 "Pa", "U")
  elem_numb <- c(1, 2, 3, 4, 5,
                 6, 7, 8, 9, 10,
                 11, 12, 13, 14, 15,
                 16, 17, 18, 19, 20,
                 21, 22, 23, 24, 25,
                 26, 27, 28, 29, 30,
                 31, 32, 33, 34, 35,
                 36, 37, 38, 39, 40,
                 41, 42, 43, 44, 45,
                 46, 47, 48, 49, 50,
                 51, 52, 53, 54, 55,
                 56, 57, 58, 59, 60,
                 61, 62, 63, 64, 65,
                 66, 67, 68, 69, 70,
                 71, 72, 73, 74, 75,
                 76, 77, 78, 79, 80,
                 81, 82, 83, 84, 85,
                 86, 87, 88, 89, 90,
                 91, 92)
  elem_weig <- c(1.00794, 4.002602, 6.941, 9.012182, 10.811,
                 12.0107, 14.0067, 15.9994, 18.9984032, 20.1797,
                 22.989770, 24.3050, 26.981538, 28.0855, 30.973761,
                 32.065, 35.453, 39.948, 39.0983, 40.078,
                 44.955912, 47.867, 50.9415, 51.9961, 54.938045,
                 55.845, 58.933195, 58.6934, 63.546, 65.38,
                 69.723, 72.64, 74.92160, 78.96, 79.904,
                 83.798, 85.4678, 87.62, 88.90585, 91.224,
                 92.90638, 95.96, 98, 101.07, 102.90550,
                 106.42, 107.8682, 112.411, 114.818, 118.710,
                 121.760, 127.60, 126.90447, 131.293, 132.9054519,
                 137.327, 138.90547, 140.116, 140.90765, 144.242,
                 145, 150.36, 151.964, 157.25, 158.92535,
                 162.500, 164.93032, 167.259, 168.93421, 173.054,
                 174.9668, 178.49, 180.94788, 183.84, 186.207,
                 190.23, 192.217, 195.084, 196.966569, 200.59,
                 204.3833, 207.2, 208.98040, 209, 210,
                 222, 223, 226, 227, 232.03806,
                 231.03588, 238.02891)
  for (i in 1:length(elem_symb)) {
    if (data_in == elem_symb[i]) {
      Name <- elem_name[i]
      Atomic.N <- elem_numb[i]
      Atomic.W <- elem_weig[i]
    }
  }
  data_out <- data.frame(Name, Atomic.N, Atomic.W)
  return(data_out)
}
