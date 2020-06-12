### --------------------------------------------------------- ###
### script to test the functions in instrumentFiles.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## fRead_Thermo()

df1 <- fRead_Thermo("../test_files/", "iport_42c.dat", "42c")

df2 <- fRead_Thermo("../test_files/", "iport_42iTL.dat", "iport")

df3 <- fRead_Thermo("../test_files/", "thermo_49i.dat", "49i")
