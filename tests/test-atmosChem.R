### --------------------------------------------------------- ###
###
### --------------------------------------------------------- ###
require(testit)

assert(
  is.data.frame(fAirND(298, 101325))
  all.equal(fAirND(298, 101325)$M, 2.462729e+19)
  fAirND(298, 101325) == c(2.462729e+19, 5.171731e+18, 1.920929e+19, 298, 101325)
)


# all.equal(fAirND(temp, press), )
# all.equal(fFractO1D(h2o, temp, press), ) p
