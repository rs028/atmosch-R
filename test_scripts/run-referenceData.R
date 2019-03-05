### --------------------------------------------------------- ###
### script to test the functions in referenceData.R
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## input and output format

assert("fConstant() input",
       fConstant("Wb")$Value == fConstant("Wb")["Value"]
)

assert("fPeriodic() input",
       fPeriodic("Ti")$Atomic.W == fPeriodic("Ti")["Atomic.W"]
)

x0 <- fConstant("Wb")
assert("fConstant() output",
       is.data.frame(x0),
       nrow(x0) == 1,
       ncol(x0) == 4
)

x0 <- fPeriodic("Ti")
assert("fPeriodic() output",
       is.data.frame(x0),
       nrow(x0) == 1,
       ncol(x0) == 4
       )

## -----------------------------------------------
## return values

assert("fConstant() values",
       fConstant("Na")$Name == "Avogadro number",
       fConstant("Na")$Value == 6.02214179e+23,
       fConstant("Na")$Unit == "mol-1",
       fConstant("R")$Name == "gas constant",
       fConstant("R")$Value == 8.314472,
       fConstant("R")$Unit == "J mol-1 K-1"
       )

assert("fPeriodic() values",
       fPeriodic("Cl")$Name == "Chlorine",
       fPeriodic("Cl")$Atomic.N == 17,
       fPeriodic("Cl")$Atomic.W == 35.453,
       fPeriodic("Br")$Name == "Bromine",
       fPeriodic("Br")$Atomic.N == 35,
       fPeriodic("Br")$Atomic.W == 79.904
       )
