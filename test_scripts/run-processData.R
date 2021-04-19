### --------------------------------------------------------- ###
### atmosch-R  //  TESTING                                    ###
### --------------------------------------------------------- ###
### Script to test the functions in processData.R
### author: RS
### --------------------------------------------------------- ###
require(testit)

## -----------------------------------------------
## fOpenair()

df1  <- data.frame(Time=c("02-06-15 10:05:00", "02-06-15 10:05:30",
                          "02-06-15 10:06:00", "02-06-15 10:06:30"),
                  NO2 = c(12, 24, 36, 48),
                  WindSpeed = c(3, 6, 9, 12),
                  WindDir = c(32, 64, 128, 256))

x0 <- fOpenair(df1, "Time", "WindSpeed", "WindDir")

assert("=> fOpenair() output",
       is.data.frame(x0),
       nrow(x0) == 4,
       ncol(x0) == 4
       )

## -----------------------------------------------
## fMakeStartStop(), fAvgStartStop(), fAvgStartStopDF()

df1 <- data.frame(Time = seq(chron("06/02/15","10:00:00"), chron("06/02/15","11:00:00"), 300/86400),
                  NO2 = runif(12, 6, 60),
                  O3 = runif(12, 30, 120))

x0 <- fMakeStartStop("02-06-2015 09:20:00", "02-06-2015 11:50:00", 15, 10)

assert("=> fMakeStartStop() output",
       is.data.frame(x0),
       nrow(x0) == 10,
       ncol(x0) == 3
       )

x1 <- fAvgStartStop(df1["Time"], df1["NO2"], x0, "no")

assert("=> fAvgStartStop() output",
       is.data.frame(x1),
       nrow(x1) == 10,
       ncol(x1) == 8
       )

x2 <- fAvgStartStopDF(df1, x0, "")
x2a <- x2[[1]]
x2b <- x2[[2]]
x2c <- x2[[3]]
x2d <- x2[[4]]
x2e <- x2[[5]]

assert("=> fAvgStartStopDF() output",
       is.list(x2),
       is.chron(x2a),
       is.chron(x2b),
       is.chron(x2c),
       is.data.frame(x2d),
       is.data.frame(x2e),
       length(x2) == 5,
       length(x2a) == 10,
       length(x2b) == 10,
       length(x2c) == 10,
       nrow(x2d) == 10,
       nrow(x2e) == 10,
       ncol(x2d) == 5,
       ncol(x2e) == 5
      )

assert("=> fAvgStartStop(), fAvgStartStopDF() values",
       all.equal(x1, fAvgStartStop(df1[1], df1[2], x0, "no"), all.names=TRUE),
       all.equal(x0[[2]], x1$MidTime, all.names=TRUE),
       all.equal(x0[[2]], x2$MidTime, all.names=TRUE),
       all.equal(x1[4:8], x2$NO2.avg, all.names=TRUE)
       )

## -----------------------------------------------
## fChronStr()

df1  <- data.frame(Time=c("02-06-15 10:05:00", "02-06-15 10:05:30",
                          "02-06-15 10:06:00", "02-06-15 10:06:30"),
                   NO=c(1, 3, 5, 7))

x0 <- fChronStr(df1$Time, "d-m-y h:m:s")

assert("=> fChronStr() input",
       x0 == fChronStr(df1["Time"], "d-m-y h:m:s")
       )

assert("=> fChronStr() output",
       is.chron(x0),
       length(x0) == 4
       )

assert("=> fChronStr() values",
       fChronStr("02.06.15 10:06:00", "d.m.y h:m:s") == fChronStr(df1$Time, "d-m-y h:m:s")[3],
       fChronStr("02.06.15 10:06", "d.m.y h:m") == fChronStr(df1$Time, "d-m-y h:m:s")[3]
       )

## -----------------------------------------------
## fSwitchFlag()

## -----------------------------------------------
## fBkgdSignal()
