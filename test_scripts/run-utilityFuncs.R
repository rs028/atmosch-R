### --------------------------------------------------------- ###
### atmosch-R  //  TESTING                                    ###
### --------------------------------------------------------- ###
### Script to test the functions in utilityFuncs.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## fMergeDF()

## -----------------------------------------------
## fFindIdx()

## -----------------------------------------------
## fVarName()

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101350, 101400))

x0 <- "Temp"

assert("=> fVarName() input",
       x0 == fVarName(df1$Temp),
       x0 == fVarName(df1["Temp"]),
       x0 == fVarName(df1[1])#,
       #x0 == fVarName(df1[,1])
       )

assert("=> fVarName() output",
       is.character(x0)
       )

assert("=> fVarName() values",
       fVarName(df1[c("Temp","Press")]) == fVarName(df1[1:2])
       )
