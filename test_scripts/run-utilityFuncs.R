### --------------------------------------------------------- ###
### script to test the functions in utilityFuncs.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## input data

df1 <- data.frame(Temp = c(298,300,302),
                  Press = c(101300,101400,101500),
                  H2O = c(1e19,2e19,3e19))

## -----------------------------------------------
## ## check input and output

assert("=> fVarName() input/output",
       fVarName(df1$Temp) == fVarName(df1["Temp"]),
       fVarName(df1$Temp) == fVarName(df1[1])
       fVarName(df1[c("Temp","Press")]) == fVarName(df1[1:2])
       )
