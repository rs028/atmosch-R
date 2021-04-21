### --------------------------------------------------------- ###
### atmosch-R  //  TESTING                                    ###
### --------------------------------------------------------- ###
### Script to test the functions in utilityFuncs.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## fMergeDF()

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101350, 101400))

df2 <- data.frame(Temp = c(298, 299, 300, 301, 302),
                  Press = c(101300, 101375, NaN, 101325, 101400),
                  H2O = c(1e19, 3e19, 2e19, 4e19, 3e19))

df3 <- data.frame(Temp = c(298, 300, 302),
                  NO = c(2.5e10, 2.3e10, 2.7e10))

x0 <- fMergeDF(list(df1, df2, df3), "Temp", "ALL", list("_a", "_b", "_c"))
x1 <- fMergeDF(list(df1, df2, df3), "Temp", "NOTALL", list("_a", "_b", "_c"))

assert("=> fMergeDF() output",
       is.data.frame(x0),
       nrow(x0) == 5,
       ncol(x0) == 5
       )

assert("=> fMergeDF() output",
       is.data.frame(x1),
       nrow(x1) == 3,
       ncol(x1) == 5
       )

assert("=> fMergeDF() values",
       x0["H2O_b"] == df2["H2O"],
       x0[5,] == x1[3,]
       )

## -----------------------------------------------
## fFindIdx()

df1 <- data.frame(Time = c(8, 9, 10, 11, 12, 13, 14, 15, 16),
                  jO1D = c(1.2e-06, 2.7e-06, 4.5e-06, 7.9e-06, 9.4e-06, 9.2e-06, 7.9e-06, 4.9e-06, 2.0e-06))

x0 <- fFindIdx(df1$Time, "G", 12)

assert("=> fFindIdx() input",
       x0 == fFindIdx(df1["Time"], "G", 12)
       )

assert("=> fFindIdx() output",
       is.numeric(x0)
       )

assert("=> fFindIdx() values",
       fFindIdx(df1$Time, "GE", 11) == fFindIdx(df1$Time, "LE", 11),
       fFindIdx(df1$Time, "G", 10) == fFindIdx(df1$Time, "L", 12),
       fFindIdx(df1$Time, "G", 18) == 9,
       fFindIdx(df1$Time, "L", 6) == 1
       )

## -----------------------------------------------
## fVarName()

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101350, 101400))

x0 <- fVarName(df1$Temp)

assert("=> fVarName() input",
       x0 == fVarName(df1["Temp"]),
       x0 == fVarName(df1[1])
       )

assert("=> fVarName() output",
       is.character(x0)
       )

assert("=> fVarName() values",
       x0 == "Temp"
       )
