### ---------------------------------------------------------------- ###
### utilities and tools for atmosch-R functions:
###  1. clear workspace
###  2. merge list of data.frames
###  3. find point in vector greater/less than value => !!OBSOLETE!!
###  4. find point in vector greater/less than value
###  5. convert date/time string to chron
###
### version 1.6, Oct 2015
### author: RS
### ---------------------------------------------------------------- ###

fClearWS <- function() {
  ## 1. clear workspace: delete all variables, keep atmosch-R
  ## functions
  ##
  ## NB: names of atmosch-R functions = `f + uppercase letter'
  ## ------------------------------------------------------------
  rm(list=base::setdiff(ls(pos=.GlobalEnv),
                        ls(pos=.GlobalEnv, pattern="^f[.A-Z]")),
     pos=.GlobalEnv)
}

fMergeDF <- function(df.lst, var.str, all.str, suff.lst) {
  ## 2. merge list of data.frames by a common variable and rename the
  ## other variables using a list of suffixes
  ##
  ## NB: see documentation of merge()
  ##
  ## input:
  ##    df.lst = list of data.frames to merge
  ##    var.str = name of common variable
  ##    all.str = type of merge operation ("TRUE" OR "FALSE")
  ##    suff.lst = list of suffixes to rename variables
  ## output:
  ##    df.merg = data.frame ( merged data.frames )
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
      df.i <- as.data.frame(df.lst[i]) #!
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
  ## NB: this function is inefficient and slow and is kept only for
  ## backward compatibility => fFindIdx() should be used instead
  ##
  ## input:
  ##    vecd = data vector
  ##    ops = greater/equal ("GE") or less/equal ("LE")
  ##    xval = reference value
  ##    xst = starting point in data vector
  ## output:
  ##    xv = point in data vector greater/less than reference value
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

fFindIdx <- function(vecd, ops, xval) {
  ## 4. find the first point greater/less than a reference value in an
  ## ordered data/chron vector
  ##
  ## input:
  ##    vecd = ordered data/chron vector
  ##    ops = greater/equal ("GE") OR greater ("G") OR
  ##          less ("L") OR less/equal ("LE")
  ##    xval = reference value
  ## output:
  ##    xv = index of point in vector greater/less than reference value
  ## ------------------------------------------------------------
  vecd <- unlist(vecd, use.names=FALSE)
  ## first and last values of data vector
  vecd.first <- vecd[1]
  vecd.last <- vecd[length(vecd)]
  switch(ops,
         "GE" = {  # GREATER/EQUAL
           if (xval >= vecd.last) {
             xv <- which(vecd == vecd.last)
           } else {
             xv <- which(vecd >= xval)
             xv <- xv[1]
           }
         },
         "G" = {  # GREATER
           if (xval > vecd.last) {
             xv <- which(vecd == vecd.last)
           } else {
             xv <- which(vecd > xval)
             xv <- xv[1]
           }
         },
         "L" = {  # LESS
           if (xval < vecd.first) {
             xv <- which(vecd == vecd.first)
           } else {
             xv <- which(vecd < xval)
             xv <- xv[length(xv)]
           }
         },
         "LE" = {  # LESS/EQUAL
           if (xval <= vecd.first) {
             xv <- which(vecd == vecd.first)
           } else {
             xv <- which(vecd <= xval)
             xv <- xv[length(xv)]
           }
         }
         )
  return(xv)
}

fChronStr <- function(dt.str, dt.fmt) {
  ## 5. convert date, time, datetime string vector to chron vector
  ## with format "d-m-y h:m:s"
  ##
  ## input:
  ##    dt.str = date/time string vector
  ##    dt.fmt = format of date/time string ("d/m/y h:m:s" OR
  ##                                         "d/m/y" OR "h:m:s")
  ## output:
  ##    dt.chron = chron ( d-m-y h:m:s )
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
