### ---------------------------------------------------------------- ###
### functions for data processing and analysis:
###  1. start/mid/stop chron vectors
###  2. average data using start/stop
###  3. average data.frame using start/stop
###
### version 1.1, Apr 2014
### author: RS - based on code by DS (NOAA Aeronomy Lab)
### ---------------------------------------------------------------- ###

fMakeStartStop <- function(start.str, stop.str, tstep.str, inter.str) {
  ## 1. make start, mid, stop datetime chron vectors with given
  ## time step and interval (format: "d-m-y h:m:s")
  ##
  ## e.g., 30 minutes time step with 5 minutes interval:
  ##            start     mid       stop
  ##          12:00:00  12:02:30  12:04:59
  ##          12:30:00  12:32:30  12:34:59
  ##          01:00:00  01:02:30  01:04:59
  ##
  ## input:
  ##       start.str = start datetime ("d-m-y h:m:s")
  ##       stop.str = stop datetime ("d-m-y h:m:s")
  ##       tstep.str = time step between start (minutes)
  ##       inter.str = interval between start and stop (minutes)
  ## output:
  ##        data.frame ( start chron, mid chron, stop chron )
  ## ------------------------------------------------------------
  ## datetime chron vector
  begin.start <- fChronStr(start.str, "d-m-y", "h:m:s")
  end.start <- fChronStr(stop.str, "d-m-y", "h:m:s")
  ## time step and interval in fraction of day
  tstep.fd <- fConvTime(as.numeric(tstep.str), "min", "day")
  inter.fd <- fConvTime(as.numeric(inter.str), "min", "day")
  ## start and mid chron vectors
  start.dt <- seq(begin.start, end.start, by=tstep.fd)
  mid.dt <- start.dt + (inter.fd / 2)
  ## stop chron vector
  begin.stop <- begin.start + inter.fd
  end.stop <- end.start + inter.fd
  stop.dt <- seq(begin.stop, end.stop, by=tstep.fd)
  stop.dt <- stop.dt - fConvTime(1, "sec", "day")
  ## output data.frame
  df.out <- data.frame(StartTime = start.dt,
                       MidTime = mid.dt,
                       StopTime = stop.dt)
  return(df.out)
}

fAvgStartStop <- function(tst.orig, dat.orig, dat.str, tst.df, pl) {
  ## 2. calculate mean, median, standard deviation, number of averaged
  ## points and number of NaN points between time intervals defined by
  ## start/stop chron vectors
  ##
  ## NB: see documentation of fMakeStartStop() function
  ##
  ## input:
  ##       tst.orig = original chron vector ("d-m-y h:m:s")
  ##       dat.orig = original data vector
  ##       dat.str = name of data variable
  ##       tst.df = start/mid/stop chron vector ("d-m-y h:m:s")
  ##       pl = "yes" or "no"
  ## output:
  ##        data.frame ( start chron, mid chron, stop chron,
  ##                     mean, median, standard deviation,
  ##                     n. averaged points, n. NaN points )
  ##        ---> plot (if pl = "yes")
  ## ------------------------------------------------------------
  ## start/stop chron vectors
  tst.start <- tst.df$StartTime
  tst.stop <- tst.df$StopTime
  n.tst <- nrow(tst.df)
  ## chron and data vectors must have same size
  if (length(tst.orig) == length(dat.orig)) {
    ## initialize vectors
    vect.avg <- rep(NA, n.tst)
    vect.med <- rep(NA, n.tst)
    vect.std <- rep(NA, n.tst)
    vect.npt <- rep(NA, n.tst)
    vect.nan <- rep(NA, n.tst)
    ## define time intervals and average data
    start.pt <- 1
    stop.pt <- 1
    for (i in 1:n.tst) {
      start.pt <- fFindPnt(tst.orig, "GE", tst.start[i], start.pt)
      stop.pt <- fFindPnt(tst.orig, "LE", tst.stop[i], stop.pt)
      ## printout for debugging
       # cat("------------------------------\n")
       # cat("start:"); print(tst.start[i])
       # cat("\t"); print(tst.orig[start.pt])
       # cat("\t"); print(tst.orig[stop.pt])
       # cat("stop:"); print(tst.stop[i])
      ## average data between time intervals
      if ((tst.orig[start.pt] >= tst.start[i]) &
          (tst.orig[stop.pt] <= tst.stop[i])) {
          if ((stop.pt - start.pt) >= 1) {         # multiple data points
            vect.avg[i] <- mean(dat.orig[start.pt:stop.pt], na.rm=TRUE)
            vect.med[i] <- median(dat.orig[start.pt:stop.pt], na.rm=TRUE)
            vect.std[i] <- sd(dat.orig[start.pt:stop.pt], na.rm=TRUE)
            vect.npt[i] <- sum(!is.na(dat.orig[start.pt:stop.pt]))
            vect.nan[i] <- sum(is.na(dat.orig[start.pt:stop.pt]))
          } else if ((stop.pt - start.pt) == 0) {  # one data point
            vect.avg[i] <- dat.orig[start.pt]
            vect.med[i] <- dat.orig[start.pt]
            vect.std[i] <- 0
            vect.npt[i] <- 1
            vect.nan[i] <- as.numeric(is.na(dat.orig[start.pt]))
          }
      }
    }
    ## make plot (if requested)
    if (pl == "yes") {
      plot(tst.orig, dat.orig, type="l", col="blue",
           main="", xlab="Time", ylab=dat.str)
      points(tst.df$StartTime, vect.avg, col="red", pch=10)
    }
    ## output data.frame
    vect.df <- cbind(vect.avg, vect.med, vect.std,
                     vect.npt, vect.nan)
    vect.str <- c("mean", "median", "stddev", "pnts", "nans")
    colnames(vect.df) <- paste(dat.str, vect.str, sep=".")
    df.out <- data.frame(tst.df, vect.df)
    return(df.out)
  } else {
    stop("--> ERROR: data.frames have different sizes!\n")
  }
}

fAvgStartStopDF <- function(tst.orig, df.orig, tst.df, pl.str) {
  ## 3. calculate mean, median, standard deviation, number of averaged
  ## points and number of NaN points between time intervals for all
  ## variables in a data.frame
  ##
  ## NB: see documentation of fMakeStartStop() and fAvgStartStop()
  ## functions
  ##
  ## input:
  ##       tst.orig = original chron vector ("d-m-y h:m:s")
  ##       df.orig = data.frame of original data
  ##       tst.df = start/mid/stop chron vector ("d-m-y h:m:s")
  ##       pl.str = filename to save plots OR ""
  ## output:
  ##        list( start chron, mid chron, stop chron,
  ##              data.frame ( mean, median, standard deviation,
  ##                           n. averaged points, n. NaN points )
  ##              data.frame ( mean, median, standard deviation,
  ##                           n. averaged points, n. NaN points )
  ##              ... )
  ##        ---> save plots if filename is given (`pl.str'.pdf)
  ## ------------------------------------------------------------
  ## get dimensions of data.frame
  nrw <- nrow(df.orig)
  ncl <- ncol(df.orig)
  ## initialize output list and add chron vector
  lst.out <- list()
  lst.out <- tst.df
  ## open file to save plots (if required)
  if ( pl.str != "") {
    pdf(paste(pl.str, ".pdf", sep=""), paper="a4r", width=0, height=0)
  }
  ## average variables in data.frame
  for (i in 1:ncl) {
    dat.orig <- df.orig[,i]
    dat.str <- colnames(df.orig)[i]
    cat("averaging:", dat.str, "\n")
    avg.df <- fAvgStartStop(tst.orig, dat.orig, dat.str, tst.df, "yes")
    ## data.frame of averaged data: <variable name>_avg
    r <- paste(dat.str, "avg", sep="_")
    lst.out[[r]] <- avg.df[,-1:-ncol(tst.df)]
  }
  ## close file to save plots
  if ( pl.str != "") {
    dev.off()
  }
  ## output list of data.frames
  return(lst.out)
}
