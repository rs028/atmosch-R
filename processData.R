### ---------------------------------------------------------------- ###
### functions for data processing and analysis:
### - fOpenair()        : convert data.frame to openair format
### - fMakeStartStop()  : make start/mid/stop chron vectors
### - fAvgStartStop()   : average variable using start/stop
### - fAvgStartStopDF() : average data.frame using start/stop
###
### version 1.9, Mar 2017
### author: RS
###
### NB: the functions fMakeStartStop(), fAvgStartStop() are based on
### code written by DS (NOAA Aeronomy Lab) in Wavemetrics Igor
### ---------------------------------------------------------------- ###

fOpenair <- function(data.df, time.str, ws.str, wd.str) {
  ## convert a data.frame to the openair format:
  ## * openair naming standard for datetime, wind data
  ## * datetime from chron to POSIX format
  ##
  ## openair is an R package for the analysis of air pollution data
  ## (http://www.openair-project.org/)
  ##
  ## input:
  ##     data.df = input data.frame
  ##     time.str = name of datetime variable
  ##     ws.str = name of wind speed variable (m/s)
  ##     wd.str = name of wind direction variable (deg N)
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
      cat("-> wind speed data [", ws.str,"]: m/s\n")
    }
    if (wd.str != "") {
      df.vars[which(df.vars == wd.str)] <- "wd"
      cat("-> wind direction data [", wd.str,"]: deg N\n")
    }
    colnames(df.out) <- df.vars
    ## convert datetime to POSIX format
    time.x <- as.POSIXlt(df.out$date, tz="GMT")
    time.x <- round.POSIXt(time.x)
    df.out$date <- as.POSIXct(time.x)#, tz="GMT")
    ## output data.frame
    return(df.out)
  } else {
    stop("input must be a data.frame")
  }
}

fMakeStartStop <- function(start.str, stop.str, tstep.str, inter.str) {
  ## make start, mid, stop datetime chron vectors with given time step
  ## and interval (format: "d-m-y h:m:s")
  ##
  ## e.g., 30 minutes time step with 5 minutes interval:
  ##     start     mid       stop
  ##   12:00:00  12:02:30  12:04:59
  ##   12:30:00  12:32:30  12:34:59
  ##   01:00:00  01:02:30  01:04:59
  ##
  ## input:
  ##     start.str = start datetime string ("d-m-y h:m:s")
  ##     stop.str = stop datetime string ("d-m-y h:m:s")
  ##     tstep.str = time step between start (minutes)
  ##     inter.str = interval between start and stop (minutes)
  ## output:
  ##     df.out = data.frame ( StartTime = start chron,
  ##                           MidTime = mid chron,
  ##                           StopTime = stop chron )
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
  ## calculate statistics (mean, median, standard deviation, etc...)
  ## of a variable between time intervals defined by start/stop chron
  ## vectors
  ##
  ## NB: use fMakeStartStop() to create the start/mid/stop chron
  ## vector (tst.df)
  ##
  ## input:
  ##     tst.orig = original chron vector ("d-m-y h:m:s")
  ##     dat.orig = original data vector
  ##     tst.df = start/mid/stop chron vector ("d-m-y h:m:s")
  ##     pl = make plot of averaged data ("yes" or "no")
  ## output:
  ##     df.out = data.frame ( start chron, mid chron, stop chron,
  ##                           mean, median, standard deviation,
  ##                           n. averaged points, n. NA points )
  ##     --> plot averaged data (if pl = "yes")
  ## ------------------------------------------------------------
  if (!is.data.frame(tst.df)) {
    df.name <- deparse(substitute(tst.df))
    stop(paste(df.name, "must be a data.frame", sep=" "))
  }
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
    ## make plot of original and averaged data
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
    vect.str <- c("Mean", "Median", "StdDev", "Pnts", "NAs")
    colnames(vect.df) <- vect.str
    df.out <- data.frame(tst.df, vect.df)
    return(df.out)
  } else {
    stop("time and data vectors not compatible")
  }
}

fAvgStartStopDF <- function(df.orig, tst.df, fn.str) {
  ## calculate statistics (mean, median, standard deviation, etc...)
  ## of all variables in a data.frame between time intervals defined
  ## by start/stop chron vectors save plots of averaged data to pdf if
  ## filename given
  ##
  ## NB: see documentation of fMakeStartStop() and fAvgStartStop()
  ##
  ## input:
  ##     df.orig = original data.frame (first column must be a
  ##               chron vector in "d-m-y h:m:s" format)
  ##     tst.df = start/mid/stop chron vector ("d-m-y h:m:s")
  ##     fn.str = name of pdf file to save plots OR ""
  ## output:
  ##     lst.out = list ( start chron, mid chron, stop chron,
  ##                      data.frame ( mean, median, standard deviation,
  ##                                   n. averaged points, n. NA points ),
  ##                      data.frame ( mean, median, standard deviation,
  ##                                   n. averaged points, n. NA points ),
  ##                     ... )
  ##     --> pdf file : `fn.str'.pdf
  ## ------------------------------------------------------------
  if (!is.data.frame(df.orig) | !is.data.frame(tst.df)) {
    stop("input must be a data.frame")
  }
  ## initialize output list with chron vector
  lst.out <- tst.df
  ## open pdf file to save plots
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
  ## output list
  return(lst.out)
}
