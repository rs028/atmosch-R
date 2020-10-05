### --------------------------------------------------------- ###
### script to test the functions in physChem.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## fGasLaw()

## df1 <- data.frame(Temp = c(298, 300, 302),
##                   Press = c(101300, 101400, 101500),
##                   Mol = c(20, 30, 40)
##                   )

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

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101350, 101400))

df2 <- data.frame(REAC = c("NO+O3", "NO+HO2"),
                  AA = c(1.4e-12, 3.6e-12),
                  EA = c(-1310, 270))

x0 <- fKBi(df2$AA, df2$EA, df1$Temp)

assert("=> fKBi() input",
       x0 == fKBi(df2["AA"], df2["EA"], df1["Temp"]),
       x0 == fKBi(df2$AA, df2["EA"], df1$Temp),
       x0 == fKBi(df2["AA"], df2$EA, df1["Temp"])
       )

assert("=> fKBi() output",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 3
       )

assert("=> fKBi() values",
       fKBi(3.6e-12, 270, 300) == fKBi(df2$AA, df2$EA, df1$Temp)[2,2:3]
       )

## -----------------------------------------------
## fKBix()

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101350, 101400))

df2 <- data.frame(REAC = c("OH+OH", "HO2+O3"),
                  AA = c(6.2e-14, 2.03e-16),
                  T0 = c(298, 300),
                  NN = c(2.6, 4.57),
                  EA = c(945, 693))

x0 <- fKBix(df2$AA, df2$T0, df2$NN, df2$EA, df1$Temp)

assert("=> fKBix() input",
       x0 == fKBix(df2["AA"], df2["T0"], df2["NN"], df2["EA"], df1["Temp"]),
       x0 == fKBix(df2$AA, df2["T0"], df2$NN, df2["EA"], df1$Temp),
       x0 == fKBix(df2["AA"], df2$T0, df2["NN"], df2$EA, df1["Temp"])
       )

assert("=> fKBix() output",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 3
       )

assert("=> fKBix() values",
       fKBix(2.03e-16, 300, 4.57, 693, 300) == fKBix(df2$AA, df2$T0, df2$NN, df2$EA, df1$Temp)[2,2:3]
       )

## -----------------------------------------------
## fKTer()

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101400, 101500))

df2 <- data.frame(REAC = c("OH+C2H4 (zero)", "OH+C2H4 (inf)"),
                  AA = c(8.6e-29, 9.0e-12),
                  T0 = c(300, 300),
                  NN = c(-3.1, -0.85),
                  EA = c(0, 0))

x1 <- fKBix(df2$AA, df2$T0, df2$NN, df2$EA, df1$Temp)

df3 <- data.frame(REAC = "OH+C2H4",
                  K0 = x1$k1,
                  KI = x1$k2,
                  FC = 0.48)

x0 <- fKTer(df3$K0, df3$KI, df3$FC, df1$Temp, df1$Press, "iupac")

assert("=> fKTer() input",
       x0 == fKTer(df3["K0"], df3["KI"], df3["FC"], df1["Temp"], df1["Press"], "iupac"),
       x0 == fKTer(df3["K0"], df3$KI, df3["FC"], df1$Temp, df1["Press"], "iupac"),
       x0 == fKTer(df3$K0, df3["KI"], df3$FC, df1["Temp"], df1$Press, "iupac")
       )

assert("=> fKTer() output",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 3
       )

assert("=> fKTer() values",
       fKTer(8.6e-29, 9.0e-12, 0.48, 300, 101400, "iupac") == fKTer(df3$K0, df3$KI, df3$FC, df1$Temp, df1$Press, "iupac")[2,]
       )

## -----------------------------------------------
## fLifeT()

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101400, 101500))

df2 <- data.frame(NO = c(2.5e10, 2.3e10, 2.7e10),
                  k.O3 = c(1.73e-14, 1.78e-14, 1.83e-14))

x0 <- fLifeT(df2$k.O3, df2$NO)

assert("=> fLifeT() input",
       x0 == fLifeT(df2["k.O3"], df2["NO"]),
       x0 == fLifeT(df2["k.O3"], df2$NO),
       x0 == fLifeT(df2$k.O3, df2["NO"])
       )

assert("=> fLifeT() output",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 4
       )

assert("=> fLifeT() values",
       fLifeT(1.78e-14, 2.3e10) == fLifeT(df2$k.O3, df2$NO)[2,]
       )
