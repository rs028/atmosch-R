### --------------------------------------------------------- ###
### script to test the functions in atmosChem.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## input data

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101400, 101500),
                  H2O = c(1e19, 2e19, 3e19))

## -----------------------------------------------
## fAirND()

x0 <- fAirND(df1$Temp, df1$Press)

assert("=> fAirND() input/output format",
       x0 == fAirND(df1["Temp"], df1["Press"]),
       x0 == fAirND(df1["Temp"], df1$Press),
       x0 == fAirND(df1$Temp, df1["Press"]),
       x0$M == fAirND(df1["Temp"], df1$Press)["M"])


assert("=> fAirND() output data.frame",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 5
       )

assert("=> fAirND() calculated values",
       fAirND(300, 101400) == fAirND(df1$Temp, df1$Press)[2,]
       )

## -----------------------------------------------
## fFractO1D()

x0 <- fFractO1D(df1$H2O, df1$Temp, df1$Press)

assert("=> fFractO1D() input/output format",
       x0 == fFractO1D(df1["H2O"], df1["Temp"], df1["Press"]),
       x0 == fFractO1D(df1$H2O, df1["Temp"], df1$Press),
       x0 == fFractO1D(df1["H2O"], df1$Temp, df1["Press"]),
       x0$fO1D == fFractO1D(df1["H2O"], df1$Temp, df1$Press)["fO1D"]
       )

assert("=> fFractO1D() output data.frame",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 4
       )

assert("=> fFractO1D() calculated values",
       fFractO1D(2e19, 300, 101400) == fFractO1D(df1$H2O, df1$Temp, df1$Press)[2,]
       )
