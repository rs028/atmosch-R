### --------------------------------------------------------- ###
### script to test the functions in atmosChem.R
### --------------------------------------------------------- ###
require(testit)

## 
df1 <- data.frame(Temp = c(298,300,302),
                  Press = c(101300,101400,101500),
                  H2O = c(1e19,2e19,3e19))


fAirND(df1$Temp, df1$Press) == fAirND(df1["Temp"], df1["Press"])
fAirND(df1["Temp"], df1$Press) == fAirND(df1$Temp, df1["Press"])

## 
x0 <- fAirND(df1$Temp, df1$Press)
assert("",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 5
       )

x0 <- fFractO1D(df1$H2O, df1$Temp, df1$Press)
assert("",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 4
       )
