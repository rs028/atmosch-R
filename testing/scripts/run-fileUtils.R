### --------------------------------------------------------- ###
### atmosch-R  //  TESTING                                    ###
### --------------------------------------------------------- ###
### Script to test the functions in fileUtils.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## fImportTXT()

df1 <- fImportTXT("data/", "chlorine.csv", ",", "-9999", "d-m-y h:m:s")
df2 <- fImportTXT("data/", "chlorine.dat", " ", NaN, "d-m-y", "h:m:s")
df3 <- fImportTXT("data/", "chlorine.txt", "", "", "FD")

assert("=> fImportTXT() output - comma delimited file",
       is.data.frame(df1),
       nrow(df1) == 14,
       ncol(df1) == 2
       )

assert("=> fImportTXT() output - space delimited file",
       is.data.frame(df2),
       nrow(df2) == 14,
       ncol(df2) == 3
       )

assert("=> fImportTXT() output - tab/space delimited file",
       is.data.frame(df3),
       nrow(df3) == 14,
       ncol(df3) == 2
       )

assert("=> fImportTXT() values",
       all.equal(df1[,2], df2[,3]),
       all.equal(df1[,2], df3[,2])
       )
