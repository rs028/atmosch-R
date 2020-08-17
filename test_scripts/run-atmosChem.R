### --------------------------------------------------------- ###
### script to test the functions in atmosChem.R
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
