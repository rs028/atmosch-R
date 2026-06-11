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
df2 <- fRead_Thermo("../test_files/", "iport_42iTL.dat", "iport")
df3 <- fRead_Thermo("../test_files/", "thermo_49i.dat", "49i")

assert("=> fRead_Thermo() output - monitor 42c (iPort)",
       is.data.frame(df1),
       nrow(df1) == 9,
       ncol(df1) == 14
       )

assert("=> fRead_Thermo() output - monitor 42iTL (iPort)",
       is.data.frame(df2),
       nrow(df2) == 9,
       ncol(df2) == 17
       )

assert("=> fRead_Thermo() output - monitor 49i (stream)",
       is.data.frame(df3),
       nrow(df3) == 15,
       ncol(df3) == 12
       )

## -----------------------------------------------
## fRead_QCL()

df1 <- fRead_QCL("../test_files/", "200917_000002", "no")
df2 <- fRead_QCL("../test_files/", "200917_000002", "yes")

assert("=> fRead_QCL() output - without diagnostics",
       is.data.frame(df1),
       nrow(df1) == 10,
       ncol(df1) == 11
       )

assert("=> fRead_QCL() output - with diagnostics",
       is.data.frame(df2),
       nrow(df2) == 10,
       ncol(df2) == 56
       )
