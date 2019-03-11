### --------------------------------------------------------- ###
### script to test the functions in convertUnits.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## check conversion of physical units

x0 <- runif(10, -100, 100)
x1 <- fConvTemp(x0, "C", "K")
x2 <- fConvTemp(x1, "K", "F")
x3 <- fConvTemp(x2, "F", "F")
x4 <- fConvTemp(x3, "F", "C")
assert("=> fConvTemp() calculation",
       all.equal(x4, x0, tolerance=1e-10),
       all.equal(fConvTemp(25, "C", "K"), 298.15, tolerance=1e-10)
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
assert("=> fConvPress() calculation",
       all.equal(x8, x0, tolerance=1e-10),
       all.equal(fConvPress(755, "torr", "hPa"), 1006.584, tolerance=1e-10)
       )

x0 <- runif(10, 0, 360)
x1 <- fConvAngle(x0, "deg", "deg")
x2 <- fConvAngle(x1, "deg", "rad")
x3 <- fConvAngle(x2, "rad", "deg")
assert("=> fConvAngle() calculation",
       all.equal(x3, x0, tolerance=1e-10),
       all.equal(fConvAngle(33, "deg", "rad"), 0.5759587, tolerance=1e-10)
       )

x0 <- runif(10, 0, 1e+03)
x1 <- fConvTime(x0, "hr", "wk")
x2 <- fConvTime(x1, "wk", "sec")
x3 <- fConvTime(x2, "sec", "day")
x4 <- fConvTime(x3, "day", "min")
x5 <- fConvTime(x4, "min", "min")
x6 <- fConvTime(x5, "min", "hr")
assert("=> fConvTime() calculation",
       all.equal(x6, x0, tolerance=1e-10),
       all.equal(fConvTime(8.25, "hr", "sec"), 29700, tolerance=1e-10)
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
assert("=> fConvSI() calculation",
       all.equal(x16, x0, tolerance=1e-10),
       all.equal(fConvSI(101325, "-", "H"), 1013.25, tolerance=1e-10)
       )

## -----------------------------------------------
## check conversion of concentration units

## TODO: fConcGas(), fConcAq(), fHumid()
