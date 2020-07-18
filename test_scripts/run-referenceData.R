### --------------------------------------------------------- ###
### script to test the functions in referenceData.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## fConstant()

x0 <- fConstant("Wb")

assert("=> fConstant() input",
       fConstant("Wb")$Value == fConstant("Wb")["Value"]
       )

assert("=> fConstant() output",
       is.data.frame(x0),
       nrow(x0) == 1,
       ncol(x0) == 4
       )

assert("=> fConstant() values",
       fConstant("Na")$Name == "Avogadro number",
       fConstant("Na")$Value == 6.022140857e+23,
       fConstant("Na")$Unit == "mol-1"
       )

## -----------------------------------------------
## fPeriodic()

x0 <- fPeriodic("Ti")

assert("=> fPeriodic() input",
       fPeriodic("Ti")$Atomic.W == fPeriodic("Ti")["Atomic.W"]
       )

assert("=> fPeriodic() output",
       is.data.frame(x0),
       nrow(x0) == 1,
       ncol(x0) == 4
       )

assert("=> fPeriodic() values",
       fPeriodic("Cl")$Name == "Chlorine",
       fPeriodic("Cl")$Atomic.N == 17,
       fPeriodic("Cl")$Atomic.W == 35.453
       )
