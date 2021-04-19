### --------------------------------------------------------- ###
### atmosch-R  //  TESTING                                    ###
### --------------------------------------------------------- ###
### Script to test the functions in atmosChem.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## fAirND()

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101350, 101400))

x0 <- fAirND(df1$Temp, df1$Press)

assert("=> fAirND() input",
       x0 == fAirND(df1["Temp"], df1["Press"]),
       x0 == fAirND(df1["Temp"], df1$Press),
       x0 == fAirND(df1$Temp, df1["Press"])
       )

assert("=> fAirND() output",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 5
       )

assert("=> fAirND() values",
       fAirND(300, 101350) == fAirND(df1$Temp, df1$Press)[2,],
       fAirND(300, 101350) == fAirND(df1$Temp, 101350)[2,],
       fAirND(300, 101350) == fAirND(300, df1$Press)[2,]
       )

## -----------------------------------------------
## fFractO1D()

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101350, 101400),
                  H2O = c(1e19, 2e19, 3e19))

x0 <- fFractO1D(df1$H2O, df1$Temp, df1$Press)

assert("=> fFractO1D() input",
       x0 == fFractO1D(df1["H2O"], df1["Temp"], df1["Press"]),
       x0 == fFractO1D(df1$H2O, df1["Temp"], df1$Press),
       x0 == fFractO1D(df1["H2O"], df1$Temp, df1["Press"])
       )

assert("=> fFractO1D() output",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 4
       )

assert("=> fFractO1D() values",
       fFractO1D(2e19, 300, 101350) == fFractO1D(df1$H2O, df1$Temp, df1$Press)[2,],
       fFractO1D(2e19, 300, 101350) == fFractO1D(df1$H2O, df1$Temp, 101350)[2,],
       fFractO1D(2e19, 300, 101350) == fFractO1D(df1$H2O, 300, df1$Press)[2,],
       fFractO1D(2e19, 300, 101350) == fFractO1D(2e19, df1$Temp, df1$Press)[2,]
       )

## -----------------------------------------------
## fParamOH()

df1 <- data.frame(Time = c(8, 9, 10, 11, 12, 13, 14, 15, 16),
                  jO1D = c(1.2e-06, 2.7e-06, 4.5e-06, 7.9e-06, 9.4e-06, 9.2e-06, 7.9e-06, 4.9e-06, 2.0e-06))

x0 <- fParamOH(df1$jO1D)

assert("=> fParamOH() input",
       x0 == fParamOH(df1["jO1D"])
       )

assert("=> fParamOH() output",
       is.data.frame(x0),
       nrow(x0) == 9,
       ncol(x0) == 12
       )

assert("=> fParamOH() values",
       fParamOH(4.5E-06) == fParamOH(df1$jO1D)[3,]
       )

## -----------------------------------------------
## fPSS()

x0 <- fPSS(100, 7.5e11, 2.5e12, 5.0e11, 1.0e-2, 298.15)

assert("=> fPSS() output",
       is.data.frame(x0),
       nrow(x0) == 101,
       ncol(x0) == 6
       )
