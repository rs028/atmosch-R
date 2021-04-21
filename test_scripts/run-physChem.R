### --------------------------------------------------------- ###
### atmosch-R  //  TESTING                                    ###
### --------------------------------------------------------- ###
### Script to test the functions in physChem.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## fGasLaw()

df1 <- data.frame(Temp = runif(10, 173, 373),
                  Press = runif(10, 0, 150000),
                  Vol = runif(10, 0, 100))

x1 <- fGasLaw(df1$Press, df1$Vol, "?", df1$Temp)
x2 <- fGasLaw("?", x1$Vol, x1$Mol, x1$Temp)
x3 <- fGasLaw(x2$Press, "?", x2$Mol, x2$Temp)
x4 <- fGasLaw(x3$Press, x3$Vol, x3$Mol, "?")
x5 <- fGasLaw(x4$Press, x4$Vol, "?", x4$Temp)
assert("=> fGasLaw() calculations",
       all.equal(x5, x1, tolerance=1e-7)
       )

assert("=> fGasLaw() input",
       x1 == fGasLaw(df1["Press"], df1["Vol"], "?", df1["Temp"]),
       x1 == fGasLaw(df1$Press, df1["Vol"], "?", df1$Temp),
       x1 == fGasLaw(df1["Press"], df1$Vol, "?", df1["Temp"])
       )

assert("=> fGasLaw() output",
       is.data.frame(x1),
       nrow(x1) == 10,
       ncol(x1) == 4
       )

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
       fKBi(3.6e-12, 270, 300) == fKBi(df2$AA, df2$EA, df1$Temp)[2,2:3],
       fKBi(3.6e-12, 270, 300) == fKBi(3.6e-12, 270, df1$Temp)[2,],
       fKBi(3.6e-12, 270, 300) == fKBi(df2$AA, df2$EA, 300)[,2:3]
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
       fKBix(2.03e-16, 300, 4.57, 693, 300) == fKBix(df2$AA, df2$T0, df2$NN, df2$EA, df1$Temp)[2,2:3],
       fKBix(2.03e-16, 300, 4.57, 693, 300) == fKBix(2.03e-16, 300, 4.57, 693, df1$Temp)[2,],
       fKBix(2.03e-16, 300, 4.57, 693, 300) == fKBix(df2$AA, df2$T0, df2$NN, df2$EA, 300)[,2:3]
       )

## -----------------------------------------------
## fKTer()

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101350, 101400))

df2 <- data.frame(REAC = c("OH+C2H4 (zero)", "OH+C2H4 (inf)"),
                  AA = c(8.6e-29, 9.0e-12),
                  T0 = c(300, 300),
                  NN = c(-3.1, -0.85),
                  EA = c(0, 0))

df3 <- data.frame(REAC = "OH+C2H4",
                  K0 = fKBix(df2$AA, df2$T0, df2$NN, df2$EA, df1$Temp)$k1,
                  KI = fKBix(df2$AA, df2$T0, df2$NN, df2$EA, df1$Temp)$k2,
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
       fKTer(8.6e-29, 9.0e-12, 0.48, 300, 101350, "iupac") == fKTer(df3$K0, df3$KI, df3$FC, df1$Temp, df1$Press, "iupac")[2,],
       fKTer(8.6e-29, 9.0e-12, 0.48, 300, 101350, "iupac") == fKTer(8.6e-29, 9.0e-12, 0.48, df1$Temp, df1$Press, "iupac")[2,],
       fKTer(8.6e-29, 9.0e-12, 0.48, 300, 101350, "iupac") == fKTer(df3$K0, df3$KI, df3$FC, 300, df1$Press, "iupac")[2,],
       fKTer(8.6e-29, 9.0e-12, 0.48, 300, 101350, "iupac") == fKTer(df3$K0, df3$KI, df3$FC, df1$Temp, 101350, "iupac")[2,],
       fKTer(8.6e-29, 9.0e-12, 0.48, 300, 101350, "iupac") == fKTer(df3$K0, df3$KI, df3$FC, 300, 101350, "iupac")[2,]
)

## -----------------------------------------------
## fLifeT()

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101350, 101400))

df2 <- data.frame(NO = c(2.5e10, 7.4e10, 1.23e11),
                  kO3 = c(1.73e-14, 1.78e-14, 1.83e-14))

x0 <- fLifeT(df2$kO3, df2$NO)

assert("=> fLifeT() input",
       x0 == fLifeT(df2["kO3"], df2["NO"]),
       x0 == fLifeT(df2["kO3"], df2$NO),
       x0 == fLifeT(df2$kO3, df2["NO"])
       )

assert("=> fLifeT() output",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 3
       )

assert("=> fLifeT() values",
       fLifeT(1.78e-14, 7.4e10) == fLifeT(df2$kO3, df2$NO)[2,],
       fLifeT(1.78e-14, 7.4e10) == fLifeT(1.78e-14, df2$NO)[2,],
       fLifeT(1.78e-14, 7.4e10) == fLifeT(df2$kO3, 7.4e10)[2,]
       )
