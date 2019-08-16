### --------------------------------------------------------- ###
### script to test the functions in physChem.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## input data

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101400, 101500),
                  Mol = c(20, 30, 40))

df2 <- data.frame(REAC = c("NO+O3", "NO+HO2"),
                  AA = c(1.4e-12, 3.6e-12),
                  EA = c(-1310, 270))

## -----------------------------------------------
## fGasLaw()

## x1 <- fGasLaw(p0, "?", m0, t0)
## x2 <- fGasLaw(p0, x1, "?", t0)
## x3 <- fGasLaw(p0, x1, x2, "?")
## x4 <- fGasLaw("?", x1, x2, x3)

## assert("=> fGasLaw",
##        all.equal(x4, p0)
##        )

## x0 <- fGasLaw(df1$Press, "?", df1$Mol, df1$Temp)
## assert("=> fGasLaw() output data.frame",
##        is.data.frame(x0),
##        nrow(x0) == 3,
##        ncol(x0) == 4
##        )

## -----------------------------------------------
## fKBi()

x0 <- fKBi(df2$AA, df2$EA, df1$Temp)

assert("=> fKBi() input/output format",
       x0 == fKBi(df2["AA"], df2["EA"], df1["Temp"]),
       x0 == fKBi(df2$AA, df2["EA"], df1$Temp),
       x0 == fKBi(df2["AA"], df2$EA, df1["Temp"]),
       x0$k2 == fKBi(df2["AA"], df2$EA, df1$Temp)["k2"]
       )

assert("=> fKBi() output data.frame",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 3
       )

assert("=> fKBi() calculated values",
       fKBi(3.6e-12, 270, 300)$k1 == fKBi(df2$AA, df2$EA, df1$Temp)[2,2],
       fKBi(3.6e-12, 270, df1$Temp)$k1 == fKBi(df2$AA, df2$EA, df1$Temp)[,-1],
       fKBi(df2$AA, df2$EA, 300)$k2 == fKBi(df2$AA, df2$EA, df1$Temp)[2,2]
       )
