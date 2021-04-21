### --------------------------------------------------------- ###
### atmosch-R  //  TESTING                                    ###
### --------------------------------------------------------- ###
### Script to test the functions in referenceData.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## fConstant()

x0 <- fConstant("R")

assert("=> fConstant() input",
       fConstant("R")$Value == fConstant("R")["Value"]
       )

assert("=> fConstant() output",
       is.data.frame(x0),
       nrow(x0) == 1,
       ncol(x0) == 4
       )

assert("=> fConstant() values",
       fConstant("kB")$Name == "Boltzmann constant",
       fConstant("kB")$Value == 1.38064852e-23,
       fConstant("kB")$Unit == "J K-1"
       )

## -----------------------------------------------
## fPeriodic()

x0 <- fPeriodic("Na")

assert("=> fPeriodic() input",
       fPeriodic("Na")$Atomic.W == fPeriodic("Na")["Atomic.W"]
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
