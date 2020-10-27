### --------------------------------------------------------- ###
### script to test the functions in convertUnits.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## conversion of physical units

x0 <- runif(10, -100, 100)
x1 <- fConvTemp(x0, "C", "K")
x2 <- fConvTemp(x1, "K", "F")
x3 <- fConvTemp(x2, "F", "F")
x4 <- fConvTemp(x3, "F", "C")
assert("=> fConvTemp() calculations",
       all.equal(x4, x0, tolerance=1e-7),
       all.equal(fConvTemp(25, "C", "K"), 298.15, tolerance=1e-7)
       )

x0 <- runif(10, 0, 1500)
x1 <- fConvPress(x0, "mbar", "hPa")
x2 <- fConvPress(x1, "hPa", "atm")
x3 <- fConvPress(x2, "atm", "bar")
x4 <- fConvPress(x3, "bar", "torr")
x5 <- fConvPress(x4, "torr", "torr")
x6 <- fConvPress(x5, "torr", "Pa")
x7 <- fConvPress(x6, "Pa", "psi")
x8 <- fConvPress(x7, "psi", "mbar")
assert("=> fConvPress() calculations",
       all.equal(x8, x0, tolerance=1e-7),
       all.equal(fConvPress(760, "torr", "hPa"), 1013.25, tolerance=1e-7)
       )

x0 <- runif(10, 0, 360)
x1 <- fConvAngle(x0, "deg", "deg")
x2 <- fConvAngle(x1, "deg", "rad")
x3 <- fConvAngle(x2, "rad", "deg")
assert("=> fConvAngle() calculations",
       all.equal(x3, x0, tolerance=1e-7),
       all.equal(fConvAngle(30, "deg", "rad"), 0.5235988, tolerance=1e-7)
       )

x0 <- runif(10, 0, 1e+03)
x1 <- fConvTime(x0, "hr", "wk")
x2 <- fConvTime(x1, "wk", "sec")
x3 <- fConvTime(x2, "sec", "day")
x4 <- fConvTime(x3, "day", "min")
x5 <- fConvTime(x4, "min", "min")
x6 <- fConvTime(x5, "min", "hr")
assert("=> fConvTime() calculations",
       all.equal(x6, x0, tolerance=1e-7),
       all.equal(fConvTime(8.25, "hr", "sec"), 29700, tolerance=1e-7)
       )

x0 <- runif(10, 0, 1e+05)
x1 <- fConvSI(x0, "d", "H")
x2 <- fConvSI(x1, "H", "m")
x3 <- fConvSI(x2, "m", "M")
x4 <- fConvSI(x3, "M", "n")
x5 <- fConvSI(x4, "n", "T")
x6 <- fConvSI(x5, "T", "f")
x7 <- fConvSI(x6, "f", "D")
x8 <- fConvSI(x7, "D", "c")
x9 <- fConvSI(x8, "c", "c")
x10 <- fConvSI(x9, "c", "K")
x11 <- fConvSI(x10, "K", "u")
x12 <- fConvSI(x11, "u", "G")
x13 <- fConvSI(x12, "G", "p")
x14 <- fConvSI(x13, "p", "P")
x15 <- fConvSI(x14, "P", "-")
x16 <- fConvSI(x15, "-", "d")
assert("=> fConvSI() calculations",
       all.equal(x16, x0, tolerance=1e-7),
       all.equal(fConvSI(101325, "-", "H"), 1013.25, tolerance=1e-7)
       )

## -----------------------------------------------
## fConcGas()

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101350, 101400),
                  O3 = c(20, 40, 60),
                  NO = c(1, 3, 5))

x0 <- fConcGas(df1$O3, "ppb", "ND", df1$Temp, df1$Press)

assert("=> fConcGas() input",
       x0 == fConcGas(df1["O3"], "ppb", "ND", df1["Temp"], df1["Press"]),
       x0 == fConcGas(df1$O3, "ppb", "ND", df1["Temp"], df1$Press),
       x0 == fConcGas(df1["O3"], "ppb", "ND", df1$Temp, df1["Press"])
       )

assert("=> fConcGas() values",
      fConcGas(3, "ppb", "ND", 300, 101350) == fConcGas(df1[3:4], "ppb", "ND", df1$Temp, df1$Press)[2,2],
      fConcGas(3, "ppb", "ND", 300, 101350) == fConcGas(df1[3:4], "ppb", "ND", df1$Temp, 101350)[2,2],
      fConcGas(3, "ppb", "ND", 300, 101350) == fConcGas(df1[3:4], "ppb", "ND", 300, df1$Press)[2,2],
      fConcGas(3, "ppb", "ND", 300, 101350) == fConcGas(3, "ppb", "ND", df1$Temp, df1$Press)[2],
      fConcGas(3, "ppb", "UG", 300, 101350, 30) == fConcGas(df1[3:4], "ppb", "UG", df1$Temp, df1$Press, c(48, 30))[2,2],
      fConcGas(3, "ppb", "UG", 300, 101350, 30) == fConcGas(df1[3:4], "ppb", "UG", 300, 101350, c(48, 30))[2,2]
      )

x1 <- fConcGas(x0, "ND", "ppth", df1$Temp, df1$Press)
x2 <- fConcGas(x1, "ppth", "MD", df1$Temp, df1$Press)
x3 <- fConcGas(x2, "MD", "ppm", df1$Temp, df1$Press)
x4 <- fConcGas(x3, "ppm", "UG", df1$Temp, df1$Press, 48)
x5 <- fConcGas(x4, "UG", "ppt", df1$Temp, df1$Press, 48)
x6 <- fConcGas(x5, "ppt", "ppb", df1$Temp, df1$Press)
assert("=> fConcGas() calculations",
      all.equal(x6, df1$O3, tolerance=1e-7)
      )

## -----------------------------------------------
## fConcAq()

df1 <- data.frame(SO4 = c(2, 4, 6),
                  NO3 = c(0.1, 0.3, 0.5))

x0 <- fConcAq(df1$SO4, "M", "UG", 96)

assert("=> fConcAq() input",
       x0 == fConcAq(df1["SO4"], "M", "UG", 96)
       )

assert("=> fConcAq() values",
       fConcAq(0.3, "M", "UG", 62) == fConcAq(df1, "M", "UG", c(96, 62))[2,2]
       )

x1 <- fConcAq(x0, "UG", "MD", 96)
x2 <- fConcAq(x1, "MD", "M", 96)
assert("=> fConcAq() calculations",
      all.equal(x2, df1$SO4, tolerance=1e-7)
      )

## -----------------------------------------------
## fHumid()
