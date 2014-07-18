### ---------------------------------------------------------------- ###
### utilities and tools for R functions:
###  1. clear workspace
###  2. merge list of data.frames
###  3. find point in vector greater/less than value
###  4. convert date/time string to chron
###
### version 1.1, Jan 2014
### author: RS
### ---------------------------------------------------------------- ###

fClearWS <- function() {
  ## 1. clear workspace --> delete all variables, keep functions
  ##
  ## NB: functions names = `f + uppercase letter'
  ## ------------------------------------------------------------
  rm(list = setdiff(ls(pos=.GlobalEnv),
                    ls(pos=.GlobalEnv, pattern="^f[.A-Z]")),
     pos = .GlobalEnv)
}

fMergeDF <- function(df.lst, var.str, all.str, suff.lst) {
  ## 2. merge list of data.frames by a common variable and rename the
  ## other variables using a list of suffixes
  ##
  ## NB: see documentation of merge() function
  ##
  ## input:
  ##       df.lst = list of data.frames to merge
  ##       var.str = name of common variable
  ##       all.str = type of merge operation ("TRUE" OR "FALSE")
  ##       suff.lst = list of suffixes to rename variables
  ## output:
  ##        data.frame ( merged data.frames )
  ## ------------------------------------------------------------
  if (length(df.lst) == 2 ) {  # two data.frames
    df.merg <- merge(df.lst[1], df.lst[2],
                     by.x=var.str, by.y=var.str,
                     all=as.logical(all.str),
                     suffixes=unlist(suff.lst))
  } else {                     # multiple data.frames
    df.merg <- as.data.frame(df.lst[1])
    colnames(df.merg)[-1] <- paste(colnames(df.merg)[-1],
                                   suff.lst[[1]], sep="")
    for (i in 2:length(df.lst)) {
      df.i <- as.data.frame(df.lst[i])
      colnames(df.i)[-1] <- paste(colnames(df.i)[-1],
                                  suff.lst[[i]], sep="")
      df.merg <- merge(df.merg, df.i,
                       by.x=var.str, by.y=var.str,
                       all=as.logical(all.str))
    }
  }
  ## merged data.frames
  return(df.merg)
}

fFindPnt <- function(vecd, ops, xval, xst) {
  ## 3. find the first point greater/equal or less/equal than a
  ## reference value in a data vector starting from a given point in
  ## the data vector
  ##
  ## NB: mostly for use with fAvgStartStop() and fMakeExpand()
  ## functions (processData.R)
  ##
  ## input:
  ##       vecd = data vector
  ##       ops = greater/equal ("GE") or less/equal ("LE")
  ##       xval = reference value
  ##       xst = starting point in data vector
  ## output:
  ##        xv = point in data vector greater/less than reference value
  ## ------------------------------------------------------------
  nv <- length(vecd)
  ## starting point is greater than data vector length
  if (xst >= nv) {
    xv <- nv
    ## starting point is zero/negative
  } else if (xst <= 0) {
    xv <- -1
  } else {
    switch(ops,
           "GE" = {  # GREATER/EQUAL
             if (vecd[xst] >= xval) {
               xv <- xst
             } else {
               while ((vecd[xst] < xval) & (xst < nv)) {
                 xst <- xst + 1
               }
               if ((vecd[xst] >= xval) & (vecd[xst-1] < xval)) {
                 xv <- xst
               } else {
                 xv <- nv
               }
             }
           },
           "LE" = {  # LESS/EQUAL
             if (vecd[xst] >= xval) {
               xv <- xst
             } else {
               while ((vecd[xst] <= xval) & (xst < nv)) {
                 xst <- xst + 1
               }
               if ((vecd[xst-1] <= xval) & (vecd[xst] > xval)) {
                 xv <- xst - 1
               } else {
                 xv <- nv
               }
             }
           }
           )
  }
  return(xv)
}

fChronStr <- function(dt.str, d.fmt, t.fmt) {
  ## 4. convert date, time, datetime strings in given format to
  ## chron vectors with format "d-m-y h:m:s"
  ##
  ## input:
  ##       dt.str = date/time string
  ##       d.fmt = format of date string ("d/m/y" OR "")
  ##       t.fmt = format of time string ("h:m:s" OR "")
  ## output:
  ##        chron ( d-m-y h:m:s )
  ## ------------------------------------------------------------
  ## split date and time strings
  dt.lst <- sapply( as.character(dt.str),
                    function(x) unlist(strsplit(x, " ")) )
  dt.lst <- t(dt.lst)
  rownames(dt.lst) <- NULL
  ## convert strings to chron
  if (d.fmt == "") {         # time only
    dt.chron <- chron(times=dt.lst[1,], format=t.fmt,
                      out.format="h:m:s")
  } else if (t.fmt == "") {  # date only
    dt.chron <- chron(dates=dt.lst[1,], format=d.fmt,
                      out.format="d-m-y")
  } else {                   # date and time
    dt.lst <- t(dt.lst)
    dt.chron <- chron(dates=dt.lst[1,], times=dt.lst[2,],
                      format=c(d.fmt,t.fmt),
                      out.format=c("d-m-y","h:m:s"))
  }
  ## output chron vector
  return(dt.chron)
}
