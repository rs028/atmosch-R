### --------------------------------------------------------- ###
### referenceData.R
### --------------------------------------------------------- ###
require(testit)


##
assert("fConstant, fPeriodic input",
       fConstant("Wb")$Value == fConstant("Wb")["Value"],
       fPeriodic("Ti")$Atomic.W == fPeriodic("Ti")["Atomic.W"]
)

df1 <- fConstant("Wb")
assert("fConstant output",
       is.data.frame(df1),
       nrow(df1) == 1,
       ncol(df1) == 4
)

df1 <- fPeriodic("Ti")
assert("fPeriodic output",
       is.data.frame(df1),
       nrow(df1) == 1,
       ncol(df1) == 4
       )

##
assert("fConstant values",
       fConstant("Na")$Name == "Avogadro number",
       fConstant("Na")$Value == 6.02214179e+23,
       fConstant("Na")$Unit == "mol-1",
       fConstant("R")$Name == "gas constant",
       fConstant("R")$Value == 8.314472,
       fConstant("R")$Unit == "J mol-1 K-1"
       )

assert("fPeriodic values",
       fPeriodic("Cl")$Name == "Chlorine",
       fPeriodic("Cl")$Atomic.N == 17,
       fPeriodic("Cl")$Atomic.W == 35.453,
       fPeriodic("Br")$Name == "Bromine",
       fPeriodic("Br")$Atomic.N == 35,
       fPeriodic("Br")$Atomic.W == 79.904
       )
