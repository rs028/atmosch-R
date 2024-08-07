### --------------------------------------------------------- ###
### atmosch-R  //  TESTING                                    ###
### --------------------------------------------------------- ###
### Script to test the functions in convertUnits.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## conversion of physical units

x0 <- runif(10, -100, 200)
x1 <- fConvTemp(x0, "C", "K")
x2 <- fConvTemp(x1, "K", "F")
x3 <- fConvTemp(x2, "F", "F")
x4 <- fConvTemp(x3, "F", "C")
assert("=> fConvTemp() calculations",
       all.equal(x4, x0, tolerance=1e-7),
       all.equal(fConvTemp(25, "C", "K"), 298.15, tolerance=1e-7)
       )

x0 <- runif(10, 0, 2000)
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

x0 <- runif(10, 0, 500)
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

## -----------------------------------------------
## fConcGas()

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101350, 101400),
                  O3 = c(20, 40, 60),
                  NO = c(1, 3, 5))

x0 <- fConcGas(df1$O3, "ppb", "ND", df1$Temp, df1$Press)
x1 <- fConcGas(x0, "ND", "ppth", df1$Temp, df1$Press)
x2 <- fConcGas(x1, "ppth", "MD", df1$Temp, df1$Press)
x3 <- fConcGas(x2, "MD", "ppm", df1$Temp, df1$Press)
x4 <- fConcGas(x3, "ppm", "UG", df1$Temp, df1$Press, 48)
x5 <- fConcGas(x4, "UG", "UG", df1$Temp, df1$Press, 48)
x6 <- fConcGas(x5, "UG", "ppt", df1$Temp, df1$Press, 48)
x7 <- fConcGas(x6, "ppt", "ppb", df1$Temp, df1$Press)
assert("=> fConcGas() calculations",
       all.equal(x7, df1$O3, tolerance=1e-7)
       )

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
       fConcGas(3, "ppb", "UG", 300, 101350, 30) == fConcGas(df1[c("O3","NO")], "ppb", "UG", df1$Temp, df1$Press, c(48, 30))[2,2],
       fConcGas(3, "ppb", "UG", 300, 101350, 30) == fConcGas(df1[3:4], "ppb", "UG", df1$Temp, df1$Press, c(48, 30))[2,2],
       fConcGas(3, "ppb", "UG", 300, 101350, 30) == fConcGas(df1[3:4], "ppb", "UG", 300, 101350, c(48, 30))[2,2]
       )

## -----------------------------------------------
## fConcAq()

df1 <- data.frame(SO4 = c(2, 4, 6),
                  NO3 = c(0.1, 0.3, 0.5))

x0 <- fConcAq(df1$NO3, "M", "UG", 62)
x1 <- fConcAq(x0, "UG", "MD", 62)
x2 <- fConcAq(x1, "MD", "MD")
x3 <- fConcAq(x2, "MD", "M")
assert("=> fConcAq() calculations",
       all.equal(x3, df1$NO3, tolerance=1e-7)
       )

assert("=> fConcAq() input",
       x0 == fConcAq(df1["NO3"], "M", "UG", 62)
       )

assert("=> fConcAq() values",
       fConcAq(0.3, "M", "UG", 62) == fConcAq(df1, "M", "UG", c(96, 62))[2,2]
       )

## -----------------------------------------------
## fHumid()

df1 <- data.frame(Temp = c(298, 300, 302),
                  Press = c(101300, 101350, 101400),
                  RH = c(20, 40, 60))

x0 <- fHumid(df1$RH, "RH", "PPM", df1$Temp, df1$Press)
x1 <- fHumid(x0, "PPM", "AH", df1$Temp, df1$Press)
x2 <- fHumid(x1, "AH", "MR", df1$Temp)
x3 <- fHumid(x2, "MR", "MR", df1$Temp)
x4 <- fHumid(x3, "MR", "SH", df1$Temp)
x5 <- fHumid(x4, "SH", "RH", df1$Temp)
assert("=> fHumid() calculations",
      all.equal(x5, df1$RH, tolerance=1e-7)
      )

assert("=> fHumid() input",
       x0 == fHumid(df1["RH"], "RH", "PPM", df1["Temp"], df1["Press"]),
       x0 == fHumid(df1$RH, "RH", "PPM", df1["Temp"], df1$Press),
       x0 == fHumid(df1["RH"], "RH", "PPM", df1$Temp, df1["Press"])
       )

assert("=> fHumid() values",
       fHumid(40, "RH", "PPM", 300, 101350) == fHumid(df1$RH, "RH", "PPM", df1$Temp, df1$Press)[2],
       fHumid(40, "RH", "PPM", 300, 101350) == fHumid(df1$RH, "RH", "PPM", 300, df1$Press)[2],
       fHumid(40, "RH", "PPM", 300, 101350) == fHumid(df1$RH, "RH", "PPM", df1$Temp, 101350)[2],
       fHumid(40, "RH", "PPM", 300, 101350) == fHumid(40, "RH", "PPM", df1$Temp, df1$Press)[2],
       fHumid(55, "RH", "PPM", 296) == fHumid(55, "RH", "PPM", 296, 101325)
       )
