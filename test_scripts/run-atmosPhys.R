### --------------------------------------------------------- ###
### atmosch-R  //  TESTING                                    ###
### --------------------------------------------------------- ###
### Script to test the functions in atmosPhys.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## fSolar()

df1 <- data.frame(GMT = c(chron("03/15/10", "22:00:00"),
                          chron("03/16/10", "08:00:00"),
                          chron("03/16/10", "18:00:00")),
                  Lat = c(45.3, 45.9, 46.5),
                  Long = c(-15.2, -15.4, -15.6))

x0 <- fSolar(df1$Lat, df1$Long, df1$GMT)

assert("=> fSolar() input",
       x0 == fSolar(df1["Lat"], df1["Long"], df1["GMT"]),
       x0 == fSolar(df1$Lat, df1["Long"], df1$GMT),
       x0 == fSolar(df1["Lat"], df1$Long, df1["GMT"])
       )

assert("=> fSolar() output",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 9
       )

assert("=> fSolar() values",
       fSolar(45.9, -15.4, chron("03/16/10","08:00:00")) == fSolar(df1$Lat, df1$Long, df1$GMT)[2,],
       fSolar(45.9, -15.4, chron("03/16/10","08:00:00")) == fSolar(45.9, df1$Long, df1$GMT)[2,],
       fSolar(45.9, -15.4, chron("03/16/10","08:00:00")) == fSolar(df1$Lat, -15.4, df1$GMT)[2,]
       )
