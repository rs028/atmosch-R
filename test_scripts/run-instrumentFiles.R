### --------------------------------------------------------- ###
### atmosch-R  //  TESTING                                    ###
### --------------------------------------------------------- ###
### Script to test the functions in instrumentFiles.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## fRead_Thermo()

df1 <- fRead_Thermo("../test_files/", "iport_42c.dat", "42c")
assert("=> fRead_Thermo() output - monitor 42c (iPort)",
       is.data.frame(df1),
       nrow(df1) == 9,
       ncol(df1) == 14
       )

df2 <- fRead_Thermo("../test_files/", "iport_42iTL.dat", "iport")
assert("=> fRead_Thermo() output - monitor 42iTL (iPort)",
       is.data.frame(df2),
       nrow(df2) == 9,
       ncol(df2) == 17
       )

df3 <- fRead_Thermo("../test_files/", "thermo_49i.dat", "49i")
assert("=> fRead_Thermo() output - monitor 49i (stream)",
       is.data.frame(df3),
       nrow(df3) == 15,
       ncol(df3) == 12
       )
