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
### version 3.5, Oct 2020
### author: RS
### ---------------------------------------------------------------- ###

fConstant <- function(symb.in) {
  ## Return name, symbol, value, unit of physical/chemical constants:
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
  ## input:
  ##     symb.in = symbol of constant
  ## output:
  ##     data.out = data.frame ( Name = name of constant,
  ##                             Symbol = symbol of constant,
  ##                             Value = value of constant,
  ##                             Unit = unit of constant )
  ## ------------------------------------------------------------
  n01 <- c("Avogadro number", "Na", 6.022140857e+23, "mol-1")
  n02 <- c("Boltzmann constant", "kB", 1.38064852e-23, "J K-1")
  n03 <- c("molar gas constant", "R", 8.3144598, "J mol-1 K-1")
  n04 <- c("Planck constant", "h", 6.62607004e-34, "J s")
  n05 <- c("Stefan-Boltzmann constant", "kSB", 5.670367e-08, "W m-2 K-4")
  n06 <- c("Wien displacement constant", "Wb", 2.8977729e-03, "m K")
  n07 <- c("standard gravity", "g0", 9.80665, "m s-2")
  n08 <- c("speed of light", "c0", 299792458, "m s-1")
  n09 <- c("atomic mass constant", "mu", 1.660539040e-24, "g")
  data.db <- rbind(n01, n02, n03, n04, n05, n06, n07, n08, n09)
  ## select and output constant
  for (i in 1:nrow(data.db)) {
    if (symb.in == data.db[i,2]) {
      nn <- as.character(data.db[i,1])  # name
      ss <- as.character(data.db[i,2])  # symbol
      vv <- as.numeric(data.db[i,3])    # value
      uu <- as.character(data.db[i,4])  # unit
    }
  }
  data.out <- data.frame(Name=nn, Symbol=ss, Value=vv, Unit=uu)
  return(data.out)
}

fPeriodic <- function(symb.in) {
  ## Return name, symbol, atomic number, atomic weight of naturally
  ## occuring elements (up to Uranium).
  ##
  ## input:
  ##     symb.in = symbol of element
  ## output:
  ##     data.out = data.frame ( Name = name of element,
  ##                             Symbol = symbol of element,
  ##                             Atomic.N = atomic number of element,
  ##                             Atomic.W = atomic weight of element )
  ## ------------------------------------------------------------
  n01 <- c("Hydrogen", "H", 1, 1.00794)
  n02 <- c("Helium", "He", 2, 4.002602)
  n03 <- c("Lithium", "Li", 3, 6.941)
  n04 <- c("Beryllium", "Be", 4, 9.012182)
  n05 <- c("Boron", "B", 5, 10.811)
  n06 <- c("Carbon", "C", 6, 12.0107)
  n07 <- c("Nitrogen", "N", 7, 14.0067)
  n08 <- c("Oxygen", "O", 8, 15.9994)
  n09 <- c("Fluorine", "F", 9, 18.998403)
  n10 <- c("Neon", "Ne", 10, 20.1797)
  n11 <- c("Sodium", "Na", 11, 22.98977)
  n12 <- c("Magnesium", "Mg", 12, 24.3050)
  n13 <- c("Aluminium", "Al", 13, 26.98154)
  n14 <- c("Silicon", "Si", 14, 28.0855)
  n15 <- c("Phosphorus", "P", 15, 30.97376)
  n16 <- c("Sulfur", "S", 16, 32.065)
  n17 <- c("Chlorine", "Cl", 17, 35.453)
  n18 <- c("Argon", "Ar", 18, 39.948)
  n19 <- c("Potassium", "K", 19, 39.0983)
  n20 <- c("Calcium", "Ca", 20, 40.078)
  n21 <- c("Scandium", "Sc", 21, 44.955912)
  n22 <- c("Titanium", "Ti", 22, 47.867)
  n23 <- c("Vanadium", "V", 23, 50.9415)
  n24 <- c("Chromium", "Cr", 24, 51.9961)
  n25 <- c("Manganese", "Mn", 25, 54.938045)
  n26 <- c("Iron", "Fe", 26, 55.845)
  n27 <- c("Cobalt", "Co", 27, 58.933195)
  n28 <- c("Nickel", "Ni", 28, 58.6934)
  n29 <- c("Copper", "Cu", 29, 63.546)
  n30 <- c("Zinc", "Zn", 30, 65.38)
  n31 <- c("Gallium", "Ga", 31, 69.723)
  n32 <- c("Germanium", "Ge", 32, 72.64)
  n33 <- c("Arsenic", "As", 33, 74.9216)
  n34 <- c("Selenium", "Se", 34, 78.96)
  n35 <- c("Bromine", "Br", 35, 79.904)
  n36 <- c("Krypton", "Kr", 36, 83.798)
  n37 <- c("Rubidium", "Rb", 37, 85.4678)
  n38 <- c("Strontium", "Sr", 38, 87.62)
  n39 <- c("Yttrium", "Y", 39, 88.90585)
  n40 <- c("Zirconium", "Zr", 40, 91.224)
  n41 <- c("Niobium", "Nb", 41, 92.90638)
  n42 <- c("Molybdenum", "Mo", 42, 95.96)
  n43 <- c("Technetium", "Tc", 43, 98)   # no stable isotopes
  n44 <- c("Ruthenium", "Ru", 44, 101.07)
  n45 <- c("Rhodium", "Rh", 45, 102.9055)
  n46 <- c("Palladium", "Pd", 46, 106.42)
  n47 <- c("Silver", "Ag", 47, 107.8682)
  n48 <- c("Cadmium", "Cd", 48, 112.411)
  n49 <- c("Indium", "In", 49, 114.818)
  n50 <- c("Tin", "Sn", 50, 118.710)
  n51 <- c("Antimony", "Sb", 51, 121.760)
  n52 <- c("Tellurium", "Te", 52, 127.60)
  n53 <- c("Iodine", "I", 53, 126.90447)
  n54 <- c("Xenon", "Xe", 54, 131.293)
  n55 <- c("Caesium", "Cs", 55, 132.9054)
  n56 <- c("Barium", "Ba", 56, 137.327)
  n57 <- c("Lanthanum", "La", 57, 138.90547)
  n58 <- c("Cerium", "Ce", 58, 140.116)
  n59 <- c("Praseodymium", "Pr", 59, 140.90765)
  n60 <- c("Neodymium", "Nd", 60, 144.242)
  n61 <- c("Promethium", "Pm", 61, 145)  # no stable isotopes
  n62 <- c("Samarium", "Sm", 62, 150.36)
  n63 <- c("Europium", "Eu", 63, 151.964)
  n64 <- c("Gadolinium", "Gd", 64, 157.25)
  n65 <- c("Terbium", "Tb", 65, 158.92535)
  n66 <- c("Dysprosium", "Dy", 66, 162.50)
  n67 <- c("Holmium", "Ho", 67, 164.93032)
  n68 <- c("Erbium", "Er", 68, 167.259)
  n69 <- c("Thulium", "Tm", 69, 168.93421)
  n70 <- c("Ytterbium", "Yb", 70, 173.054)
  n71 <- c("Lutetium", "Lu", 71, 174.9668)
  n72 <- c("Hafnium", "Hf", 72, 178.49)
  n73 <- c("Tantalum", "Ta", 73, 180.94788)
  n74 <- c("Tungsten", "W", 74, 183.84)
  n75 <- c("Rhenium", "Re", 75, 186.207)
  n76 <- c("Osmium", "Os", 76, 190.23)
  n77 <- c("Iridium", "Ir", 77, 192.217)
  n78 <- c("Platinum", "Pt", 78, 195.084)
  n79 <- c("Gold", "Au", 79, 196.966569)
  n80 <- c("Mercury", "Hg", 80, 200.59)
  n81 <- c("Thallium", "Tl", 81, 204.3833)
  n82 <- c("Lead", "Pb", 82, 207.2)
  n83 <- c("Bismuth", "Bi", 83, 208.9804)
  n84 <- c("Polonium", "Po", 84, 209)    # no stable isotopes
  n85 <- c("Astatine", "At", 85, 210)    # no stable isotopes
  n86 <- c("Radon", "Rn", 86, 222)       # no stable isotopes
  n87 <- c("Francium", "Fr", 87, 223)    # no stable isotopes
  n88 <- c("Radium", "Ra", 88, 226)      # no stable isotopes
  n89 <- c("Actinium", "Ac", 89, 227)    # no stable isotopes
  n90 <- c("Thorium", "Th", 90, 232.03806)
  n91 <- c("Protactinium", "Pa", 91, 231.0359)
  n92 <- c("Uranium", "U", 92, 238.02891)
  data.db <- rbind(n01, n02, n03, n04, n05, n06, n07, n08, n09,
                   n10, n11, n12, n13, n14, n15, n16, n17, n18,
                   n19, n20, n21, n22, n23, n24, n25, n26, n27,
                   n28, n29, n30, n31, n32, n33, n34, n35, n36,
                   n37, n38, n39, n40, n41, n42, n43, n44, n45,
                   n46, n47, n48, n49, n50, n51, n52, n53, n54,
                   n55, n56, n57, n58, n59, n60, n61, n62, n63,
                   n64, n65, n66, n67, n68, n69, n70, n71, n72,
                   n73, n74, n75, n76, n77, n78, n79, n80, n81,
                   n82, n83, n84, n85, n86, n87, n88, n89, n90,
                   n91, n92)
  ## select and output element
  for (i in 1:nrow(data.db)) {
    if (symb.in == data.db[i,2]) {
      nn <- as.character(data.db[i,1])  # name
      ss <- as.character(data.db[i,2])  # symbol
      an <- as.numeric(data.db[i,3])    # atomic number
      aw <- as.numeric(data.db[i,4])    # atomic weight
    }
  }
  data.out <- data.frame(Name=nn, Symbol=ss, Atomic.N=an,  Atomic.W=aw)
  return(data.out)
}
