### ---------------------------------------------------------------- ###
### functions for data processing and analysis:
###  1. load text data file
###  2. start/mid/stop chron vectors
###  3. statistics of variable using start/stop
###  4. statistics of variables data.frame using start/stop
###
### version 1.5, Sep 2015
### author: RS
###
### based on code by DS (NOAA Aeronomy Lab)
### ---------------------------------------------------------------- ###

fImportData <- function(data.dir, data.fn, miss.flag, ...) {
  ## 1. load data from a text file (*.csv, *.dat), convert date/time
  ## to chron, replace missing data points with NA
  ##
  ## NB: time strings must have hours, minutes and seconds
  ##
  ## input:
  ##    data.dir = data file directory
  ##    data.fn = name of data file
  ##    miss.flag = missing value flag (e.g., -9999)
  ##    ... = date/time strings (e.g., "d/m/y h:m:s", "h:m:s")
  ## output:
  ##    data.out = data.frame ( time variables, data variables )
  ## ------------------------------------------------------------
  ## select type of data file
  fn.num <- nchar(data.fn)
  fn.ext <- substr(data.fn, (fn.num - 3), (fn.num))
  if (fn.ext == ".csv") {         # comma delimited
    fn.sep <- ","
  } else if (fn.ext == ".txt") {  # tab delimited
    fn.sep <- "\t"
  } else if (fn.ext == ".dat") {  # tab/space delimited
    fn.sep <- ""
  } else {
    stop("invalid file extension")
  }
  ## load data file
  data.file <- paste(data.dir, data.fn, sep="")
  data.df <- read.delim(data.file, header=TRUE, sep=fn.sep)
  ## convert date/time strings to chron
  time.fmt <- list(...)
  n.time <- length(time.fmt)
  time.df <- list()
  for (t in 1:n.time) {
    time.vec <- fChronStr(data.df[[t]], time.fmt[[t]])
    time.df[[t]] <- time.vec
  }
  time.df <- as.data.frame(time.df)
  colnames(time.df) <- colnames(data.df[1:n.time])
  ## set missing values to NA
  data.filt <- data.df[,(n.time+1):ncol(data.df), drop=F]
  data.filt[data.filt == miss.flag] <- NA
  data.filt[is.na(data.filt)] <- NA
  ## output data.frame
  n.data <- ncol(data.df)
  data.out <- cbind(time.df, data.filt)
  return(data.out)
}

fMakeStartStop <- function(start.str, stop.str, tstep.str, inter.str) {
  ## 2. make start, mid, stop datetime chron vectors with given
  ## time step and interval (format: "d-m-y h:m:s")
  ##
  ## e.g., 30 minutes time step with 5 minutes interval:
  ##     start     mid       stop
  ##   12:00:00  12:02:30  12:04:59
  ##   12:30:00  12:32:30  12:34:59
  ##   01:00:00  01:02:30  01:04:59
  ##
  ## input:
  ##    start.str = start datetime ("d-m-y h:m:s")
  ##    stop.str = stop datetime ("d-m-y h:m:s")
  ##    tstep.str = time step between start (minutes)
  ##    inter.str = interval between start and stop (minutes)
  ## output:
  ##    df.out = data.frame ( StartTime = start chron,
  ##                          MidTime = mid chron,
  ##                          StopTime = stop chron )
  ## ------------------------------------------------------------
  ## datetime chron vector
  begin.start <- fChronStr(start.str, "d-m-y h:m:s")
  end.start <- fChronStr(stop.str, "d-m-y h:m:s")
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

fAvgStartStop <- function(tst.orig, dat.orig, tst.df, pl) {
  ## 3. calculate statistics (mean, median, standard deviation,
  ## etc...) of a variable between time intervals defined by
  ## start/stop chron vectors; make plot of averaged data
  ##
  ## NB: see documentation of fMakeStartStop()
  ##
  ## input:
  ##    tst.orig = original chron vector ("d-m-y h:m:s")
  ##    dat.orig = original data vector
  ##    tst.df = start/mid/stop chron vector ("d-m-y h:m:s")
  ##    pl = make plot of averaged data ("yes" or "no")
  ## output:
  ##    df.out <- data.frame ( start chron, mid chron, stop chron,
  ##                           mean, median, standard deviation,
  ##                           n. averaged points, n. NA points )
  ##    --> plot (if pl = "yes")
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
      start.pt <- fFindIdx(tst.orig, "GE", tst.start[i])
      stop.pt <- fFindIdx(tst.orig, "LE", tst.stop[i])
      ## printout for debugging
      # cat("------------------------------\n")
      # cat("start:"); print(tst.start[i])
      # cat("\t"); print(tst.orig[start.pt])
      # cat("\t"); print(tst.orig[stop.pt])
      # cat("stop:"); print(tst.stop[i])
      # average data between time intervals
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
    ## make plot of original and averaged data
    if (pl == "yes") {
      vect.name <- fVarStr(dat.orig)
      dev.new()
      plot(tst.orig, dat.orig, type="l", col="blue",
           main="", xlab="Time", ylab=vect.name)
      points(tst.df$StartTime, vect.avg, col="red", pch=10)
    }
    ## output data.frame
    vect.df <- cbind(vect.avg, vect.med, vect.std,
                     vect.npt, vect.nan)
    vect.str <- c("Mean", "Median", "StdDev", "Pnts", "NaNs")
    colnames(vect.df) <- vect.str
    df.out <- data.frame(tst.df, vect.df)
    return(df.out)
  } else {
    stop("time and data vectors not compatible")
  }
}

fAvgStartStopDF <- function(tst.orig, df.orig, tst.df, fn.str) {
  ## 4. calculate statistics (mean, median, standard deviation,
  ## etc...) of all variables in a data.frame using start/stop chron
  ## vectors; save plots of averaged data to pdf file
  ##
  ## NB: see documentation of fMakeStartStop() and fAvgStartStop()
  ##
  ## input:
  ##    tst.orig = original chron vector ("d-m-y h:m:s")
  ##    df.orig = original data.frame
  ##    tst.df = start/mid/stop chron vector ("d-m-y h:m:s")
  ##    fn.str = name of file to save plots OR ""
  ## output:
  ##    lst.out = list ( start chron, mid chron, stop chron,
  ##                     data.frame ( mean, median, standard deviation,
  ##                                  n. averaged points, n. NA points ),
  ##                     data.frame ( mean, median, standard deviation,
  ##                                  n. averaged points, n. NA points ),
  ##                     ... )
  ##        --> pdf file : `fn.str'.pdf
  ## ------------------------------------------------------------
  ## get dimensions of data.frame
  nrw <- nrow(df.orig)
  ncl <- ncol(df.orig)
  ## initialize output list and add chron vector
  lst.out <- list()
  lst.out <- tst.df
  ## open pdf file to save plots
  if ( fn.str != "") {
    pdf(paste(fn.str, ".pdf", sep=""), paper="a4r", width=0, height=0)
  }
  ## average variables in data.frame
  for (i in 1:ncl) {
    dat.orig <- t(df.orig[,i, drop=F])
    dat.str <- colnames(df.orig)[i]
    cat("averaging:", dat.str, "\n")
    avg.df <- fAvgStartStop(tst.orig, dat.orig, tst.df, "yes")
    ## add data.frame of averaged data to output list
    avg.str <- paste(dat.str, "avg", sep=".")
    lst.out[[avg.str]] <- avg.df[,-1:-ncol(tst.df)]
  }
  ## close pdf file
  if ( fn.str != "") {
    dev.off()
  }
  ## output list
  return(lst.out)
}
