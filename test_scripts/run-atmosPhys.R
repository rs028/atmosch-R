### --------------------------------------------------------- ###
### script to test the functions in atmosPhys.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## input data

df1 <- data.frame(GMT = c(chron("03/15/10","12:00:00"),
                          chron("03/16/10","08:00:00"),
                          chron("03/16/10","15:00:00")),
                  Lat = c(45.3, 45.9, 46.5),
                  Long = c(-15.2, -15.4, -15.6))


## -----------------------------------------------
## check input and output format

assert("=> fSolar() input/output",
       fSolar(df1$Lat, df1$Long, df1$GMT) == fSolar(df1["Lat"], df1["Long"], df1["GMT"]),
       fSolar(df1$Lat, df1["Long"], df1$GMT) == fSolar(df1["Lat"], df1$Long, df1["GMT"]),
       fSolar(df1$Lat, df1$Long, df1$GMT)$DEC == fSolar(df1["Lat"], df1["Long"], df1["GMT"])["DEC"]
       )

## -----------------------------------------------
## check output data.frame

x0 <- fSolar(df1$Lat, df1$Long, df1$GMT)
assert("=> fSolar() output data.frame",
       is.data.frame(x0),
       nrow(x0) == 3,
       ncol(x0) == 7
       )

## -----------------------------------------------
## check calculated values

assert("=> fSolar() values",
       fSolar(45.9, -15.4, chron("03/16/10","08:00:00")) == fSolar(df1$Lat, df1$Long, df1$GMT)[2,]
       )
