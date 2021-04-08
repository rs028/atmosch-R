### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### Functions for data processing and analysis:
### - fOpenair()        : convert data.frame to openair format
### - fMakeStartStop()  : make start/mid/stop chron variables
### - fAvgStartStop()   : average one variable using start/stop
### - fAvgStartStopDF() : average data.frame using start/stop
### - fChronStr()       : convert date/time string to chron
### - fSwitchFlag()     : flag points before/after switch
### - fBkgdSignal()     : average background signals
###
### version 2.6, Apr 2021
### author: RS
### credits: functions fMakeStartStop() and fAvgStartStop() are based
###          on code written by DS (NOAA Aeronomy Lab).
### ---------------------------------------------------------------- ###

fOpenair <- function(data.df, time.str, ws.str, wd.str, tz.str="GMT") {
  ## Convert a data.frame for use with the openair package
  ## (https://davidcarslaw.github.io/openair/):
  ## * use openair naming convention for date, time, wind speed and
  ##   direction variables
  ## * convert datetime from chron to POSIX format
  ##
  ## input:
  ##     data.df = input data.frame
  ##     time.str = name of datetime variable
  ##     ws.str = name of wind speed variable (m/s)
  ##     wd.str = name of wind direction variable (deg N)
  ##     tz.str = 
  ## output:
  ##     df.out = data.frame ( datetime, variables )
  ## ------------------------------------------------------------
  if (is.data.frame(data.df)) {
    df.out <- data.df
    ## change name of variables to openair standard
    df.vars <- colnames(data.df)
    df.vars[which(df.vars == time.str)] <- "date"
    if (ws.str != "") {
      df.vars[which(df.vars == ws.str)] <- "ws"
      cat("> wind speed data [", ws.str,"]: m/s\n")
    }
    if (wd.str != "") {
      df.vars[which(df.vars == wd.str)] <- "wd"
      cat("> wind direction data [", wd.str,"]: deg N\n")
    }
    colnames(df.out) <- df.vars
    ## convert datetime to POSIX format
    time.x <- as.POSIXlt(df.out$date, tz=tz.str)
    time.x <- round.POSIXt(time.x)
    df.out$date <- as.POSIXct(time.x)
    ## output data.frame
    return(df.out)
  } else {
    stop("input must be a data.frame")
  }
}

fMakeStartStop <- function(start.str, stop.str, step.str, interv.str) {
  ## Generate start, mid, stop datetime chron variables with given
  ## step and interval (in minutes).
  ##
  ## For example, 30 minutes step with 5 minutes interval:
  ##
  ##     start       mid         stop
  ##   12:00:00    12:02:30    12:04:59
  ##   12:30:00    12:32:30    12:34:59
  ##   01:00:00    01:02:30    01:04:59
  ##   ...         ...         ...
  ##
  ## input:
  ##     start.str = start datetime string ("d-m-y h:m:s")
  ##     stop.str = stop datetime string ("d-m-y h:m:s")
  ##     step.str = step between start (minutes)
  ##     interv.str = interval between start and stop (minutes)
  ## output:
  ##     df.out = data.frame ( StartTime = start chron,
  ##                           MidTime = mid chron,
  ##                           StopTime = stop chron )
  ## ------------------------------------------------------------
  ## datetime chron variable
  begin.start <- fChronStr(start.str, "d-m-y h:m:s")
  end.start <- fChronStr(stop.str, "d-m-y h:m:s")
  ## step and interval in fraction of day
  step.fd <- fConvTime(as.numeric(step.str), "min", "day")
  interv.fd <- fConvTime(as.numeric(interv.str), "min", "day")
  ## start and mid chron variables
  start.dt <- seq(begin.start, end.start, by=step.fd)
  mid.dt <- start.dt + (interv.fd / 2)
  ## stop chron variable
  begin.stop <- begin.start + interv.fd
  end.stop <- end.start + interv.fd
  stop.dt <- seq(begin.stop, end.stop, by=step.fd)
  stop.dt <- stop.dt - fConvTime(1, "sec", "day")
  ## output data.frame
  df.out <- data.frame(StartTime = start.dt,
                       MidTime = mid.dt,
                       StopTime = stop.dt)
  return(df.out)
}

fAvgStartStop <- function(tst.orig, dat.orig, tst.df, pl) {
  ## Calculate statistics (mean, median, standard deviation, etc...)
  ## of one variable between the time intervals defined by start/stop
  ## chron variables.
  ##
  ## Optional: show plots of averaged data on screen.
  ##
  ## NB: use fMakeStartStop() to generate the start/mid/stop datetime
  ## chron variable `tst.df`.
  ##
  ## input:
  ##     tst.orig = original chron variable ("d-m-y h:m:s")
  ##     dat.orig = original data variable
  ##     tst.df = start/mid/stop chron variable ("d-m-y h:m:s")
  ##     pl = show plot of averaged data ("yes" OR "no")
  ## output:
  ##     df.out = data.frame ( start chron, mid chron, stop chron,
  ##                           mean, median, standard deviation,
  ##                           n. averaged points, n. NA points )
  ##     --> plot averaged data (if pl = "yes")
  ## ------------------------------------------------------------
  if (!is.data.frame(tst.orig) | !is.data.frame(dat.orig) |
      !is.data.frame(tst.df)) {
    stop("input data must be in a data.frame")
  }
  ## start/stop chron variables
  tst.start <- tst.df$StartTime
  tst.stop <- tst.df$StopTime
  n.tst <- nrow(tst.df)
  ## chron and data variables must have same size
  if (length(tst.orig) == length(dat.orig)) {
    ## initialize variables
    vect.avg <- rep(NA, n.tst)
    vect.med <- rep(NA, n.tst)
    vect.std <- rep(NA, n.tst)
    vect.npt <- rep(NA, n.tst)
    vect.nan <- rep(NA, n.tst)
    ## define time intervals and average data
    start.pt <- 1; stop.pt <- 1
    for (i in 1:n.tst) {
      start.pt <- fFindIdx(tst.orig, "GE", tst.start[i])
      stop.pt <- fFindIdx(tst.orig, "LE", tst.stop[i])
      ## printout for debugging
        ## cat("------------------------------\n")
        ## cat("start:"); print(tst.start[i])
        ## cat("\t"); print(tst.orig[start.pt])
        ## cat("\t"); print(tst.orig[stop.pt])
        ## cat("stop:"); print(tst.stop[i])
      ## average data between time intervals
      if ((tst.orig[start.pt,1] >= tst.start[i]) &
          (tst.orig[stop.pt,1] <= tst.stop[i])) {
          if ((stop.pt - start.pt) >= 1) {         # multiple data points
            vect.avg[i] <- mean(dat.orig[start.pt:stop.pt,1], na.rm=TRUE)
            vect.med[i] <- median(dat.orig[start.pt:stop.pt,1], na.rm=TRUE)
            vect.std[i] <- sd(dat.orig[start.pt:stop.pt,1], na.rm=TRUE)
            vect.npt[i] <- sum(!is.na(dat.orig[start.pt:stop.pt,1]))
            vect.nan[i] <- sum(is.na(dat.orig[start.pt:stop.pt,1]))
          } else if ((stop.pt - start.pt) == 0) {  # one data point
            vect.avg[i] <- dat.orig[start.pt,1]
            vect.med[i] <- dat.orig[start.pt,1]
            vect.std[i] <- 0
            vect.npt[i] <- 1
            vect.nan[i] <- as.numeric(is.na(dat.orig[start.pt,1]))
          }
      }
    }
    ## plot original and averaged data [optional]
    if (pl == "yes") {
      vect.name <- fVarName(dat.orig)
      plot(tst.orig[,1], dat.orig[,1], type="l", col="red", lwd=2,
           xlab="Time", ylab=vect.name)
      lines(tst.df$StartTime, vect.avg, col="blue", lwd=1)
      grid()
    }
    ## output data.frame
    vect.df <- cbind(vect.avg, vect.med, vect.std,
                     vect.npt, vect.nan)
    vect.str <- c("Mean", "Median", "StdDev", "Pnts", "NaNs")
    colnames(vect.df) <- vect.str
    df.out <- data.frame(tst.df, vect.df)
    return(df.out)
  } else {
    stop("time and data variables not compatible")
  }
}

fAvgStartStopDF <- function(df.orig, tst.df, fn.str) {
  ## Calculate statistics (mean, median, standard deviation, etc...)
  ## and make plots of all variables in a data.frame between the time
  ## intervals defined by start/stop chron variables.
  ##
  ## Optional: save plots of averaged data to pdf file.
  ##
  ## NB: see documentation of fMakeStartStop() and fAvgStartStop().
  ##
  ## input:
  ##     df.orig = original data.frame (first column must be a
  ##               chron variable in "d-m-y h:m:s" format)
  ##     tst.df = start/mid/stop chron variable ("d-m-y h:m:s")
  ##     fn.str = name of pdf file to save plots OR ""
  ## output:
  ##     lst.out = list ( start chron, mid chron, stop chron,
  ##                      data.frame ( mean, median, standard deviation,
  ##                                   n. averaged points, n. NA points ),
  ##                      data.frame ( mean, median, standard deviation,
  ##                                   n. averaged points, n. NA points ),
  ##                     ... )
  ##     --> pdf file : `fn.str`.pdf
  ## ------------------------------------------------------------
  if (!is.data.frame(df.orig) | !is.data.frame(tst.df)) {
    stop("input must be a data.frame")
  }
  ## add start/mid/stop chron variables to output list
  lst.out <- tst.df
  ## open pdf file to save plots [optional]
  if (fn.str != "") {
    pdf(paste(fn.str, ".pdf", sep=""), paper="a4r", width=0, height=0)
  }
  ## average variables in data.frame using fAvgStartStop()
  tst.orig <- df.orig[1]
  for (i in 2:ncol(df.orig)) {
    dat.orig <- df.orig[i]
    dat.str <- colnames(df.orig)[i]
    cat("averaging:", dat.str, "\n")
    avg.df <- fAvgStartStop(tst.orig, dat.orig, tst.df, "yes")
    ## add data.frame of averaged data to output list
    avg.str <- paste(dat.str, "avg", sep=".")
    lst.out[[avg.str]] <- avg.df[-1:-ncol(tst.df)]
  }
  ## close pdf file
  if (fn.str != "") {
    dev.off()
  }
  ## output list: 
  return(lst.out)
}

fChronStr <- function(dt.str, dt.fmt) {
  ## Convert date, time, datetime string vector to chron vector with
  ## format "d-m-y h:m:s".
  ##
  ## input:
  ##     dt.str = date/time string vector
  ##     dt.fmt = format of date/time string ("d/m/y h:m:s" OR
  ##              "d/m/y" OR "h:m:s")
  ## output:
  ##     dt.chron = chron ( d-m-y h:m:s )
  ## ------------------------------------------------------------
  dt.str <- unlist(dt.str, use.names=FALSE)
  ## date/time format flag
  if (grepl("d", dt.fmt)) {
    dt.flag <- "date"
  }
  if (grepl("h", dt.fmt)) {
    dt.flag <- "time"
  }
  if (grepl("d", dt.fmt) & grepl("h", dt.fmt)) {
    dt.flag <- "datetime"
  }
  ## add seconds if missing
  if (dt.flag != "date" & grepl("s", dt.fmt) == FALSE) {
    dt.str <- paste(dt.str, "00", sep=":")
    dt.fmt <- paste(dt.fmt, "s", sep=":")
  }
  ## convert date/time string to chron
  switch(dt.flag,
         "date" = {      # date only
           dt.chron <- chron(dates=as.character(dt.str),
                             format=dt.fmt, out.format="d-m-y")
         },
         "time" = {      # time only
           dt.chron <- chron(times=as.character(dt.str),
                             format=dt.fmt, out.format="h:m:s")
         },
         "datetime" = {  # date and time
           dt.lst <- sapply(as.character(dt.str),
                            function(x) unlist(strsplit(x, " ")))
           colnames(dt.lst) <- NULL
           dt.chron <- chron(dates=dt.lst[1,], times=dt.lst[2,],
                             format=unlist(strsplit(dt.fmt, " ")),
                             out.format=c("d-m-y","h:m:s"))
         },              # invalid date/time format
         stop("date/time format not valid")
         )
  ## output chron vector
  return(dt.chron)
}

fSwitchFlag <- function(data.df, sw.var, sw.ref, skip.fore, skip.aft) {
  ## Find and flag the data points before/after an instrument switch
  ## for later removal. The switch flag that is added to the
  ## data.frame has the values:
  ##
  ##    0 = switch is OFF
  ##   -1 = before/after switch
  ##    1 = switch is ON
  ##
  ## The data.frame of instrument data must contain a numeric switch variable
  ## indicating the status of the switch (ON/OFF, 1/0, etc...).
  ##
  ## input:
  ##     data.df = data.frame of instrument data
  ##     sw.var = name of switch variable
  ##     sw.ref = value of switch variable (e.g., 1 OR "ON")
  ##     skip.fore = points to skip before the switch
  ##     skip.aft = points to skip after the switch
  ## output:
  ##     data.out = data.frame of instrument data with switch flag
  ## ------------------------------------------------------------
  if (!is.data.frame(data.df)) {
    df.name <- deparse(substitute(data.df))
    stop(paste(df.name, "must be a data.frame", sep=" "))
  }
  ## find data points before/after switch
  nf <- which(colnames(data.df) == sw.var)
  fl1 <- data.df[,nf]
  fl2 <- data.df[,nf]
  fl1[which(data.df[,nf] == sw.ref) - skip.aft] <- 9999
  fl2[which(data.df[,nf] == sw.ref) + skip.fore] <- 9999
  ## remove extra data points
  n.data <- nrow(data.df)
  if ((length(fl1) - n.data) != 0) {
    fl1 <- fl1[-1:-(length(fl1)-n.data)]
  }
  if ((length(fl2) - n.data) != 0) {
    fl2 <- fl2[-(n.data+1):-length(fl2)]
  }
  ## create switch flag
  data.out <- data.df
  data.out$Flag <- ifelse((fl1 == fl2 & fl1 == 9999), 1,
                   ifelse((fl1 == fl2 & fl1 != 9999), 0, -1))
  ## output variables for debugging
    ## data.out$fl1 <- fl1
    ## data.out$fl2 <- fl2
  ## output data.frame
  return(data.out)
}

fBkgdSignal <- function(data.df) {
  ## Average the background signals of an instrument over each
  ## background period.
  ##
  ## The data.frame of background signals must have one datetime chron
  ## variable (as first column), and must contain only the background
  ## periods. The instrument background is usually determined at
  ## regular intervals, so the datetime variable is expected to be
  ## discontinuous.
  ##
  ## For example, 10 minutes background period at the end of each
  ## hour:
  ##
  ##         datetime         variable 1    variable 2
  ##   12/01/2009 11:00:00       100           250
  ##   12/01/2009 11:50:00       125           200
  ##   12/01/2009 11:51:00       140           220
  ##   ...                       ...           ...
  ##   12/01/2009 11:59:00       130           210
  ##   12/01/2009 12:00:00       115           205
  ##   12/01/2009 12:50:00       120           225
  ##
  ## input:
  ##     data.df = data.frame of background signals
  ## output:
  ##     data.out = data.frame of averaged background signals
  ## ------------------------------------------------------------
  if (!is.data.frame(data.df)) {
    df.name <- deparse(substitute(data.df))
    stop(paste(df.name, "must be a data.frame", sep=" "))
  }
  ## datetime chron variable (column 1)
  data.dt <- data.df[,1]
  ## average signals over each background period
  i <- 1; j <- 1
  data.bgd <- rep(NA, ncol(data.df))
  for (k in 2:nrow(data.df)) {
    if ((data.dt[k] - data.dt[k-1]) > times("00:01:00")) {
      data.bgd <- rbind(data.bgd, colMeans(data.df[i:k-1,], na.rm=T))
      i <- k + 1
      j <- j + 1
    } else if (k == nrow(data.df)) {
      i <- i -1
      data.bgd <- rbind(data.bgd, colMeans(data.df[i:k,], na.rm=T))
    }
  }
  ## output data.frame
  data.out <- data.frame(data.bgd[-1,])
  data.out[,1] <- as.chron(data.out[,1])
  colnames(data.out) <-  paste(colnames(data.df), "bgd", sep="_")
  return(data.out)
}
