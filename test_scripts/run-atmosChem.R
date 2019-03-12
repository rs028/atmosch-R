### --------------------------------------------------------- ###
### script to test the functions in atmosChem.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## input data
df1 <- data.frame(Temp = c(298,300,302),
                  Press = c(101300,101400,101500),
                  H2O = c(1e19,2e19,3e19))

## -----------------------------------------------
## check input and output format

assert("=> fAirND() input/output",
       fAirND(df1$Temp, df1$Press) == fAirND(df1["Temp"], df1["Press"]),
       fAirND(df1["Temp"], df1$Press) == fAirND(df1$Temp, df1["Press"]),
       fAirND(df1$Temp, df1$Press)$O2 == fAirND(df1$Temp, df1$Press)["O2"]
       )

assert("=> fFractO1D() input/output",
       fFractO1D(df1$H2O, df1$Temp, df1$Press) == fFractO1D(df1["H2O"], df1["Temp"], df1["Press"]),
       fFractO1D(df1$H2O, df1["Temp"], df1$Press) == fFractO1D(df1["H2O"], df1$Temp, df1["Press"]),
       fFractO1D(df1$H2O, df1$Temp, df1$Press)$fO1D == fFractO1D(df1$H2O, df1$Temp, df1$Press)["fO1D"]
       )

## -----------------------------------------------
## check output data.frame

x0 <- fAirND(df1$Temp, df1$Press)
assert("=> fAirND() output data.frame",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 5
       )

x0 <- fFractO1D(df1$H2O, df1$Temp, df1$Press)
assert("=> fFractO1D() output data.frame",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 4
       )

## -----------------------------------------------
## check calculated values

assert("=> fAirND() values",
       fAirND(300, 101400) == fAirND(df1$Temp, df1$Press)[2,]
       )

assert("=> fFractO1D() values",
       fFractO1D(2e19, 300, 101400) == fFractO1D(df1$H2O, df1$Temp, df1$Press)[2,]
       )
