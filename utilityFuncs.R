### ---------------------------------------------------------------- ###
### utilities and tools for atmosch-R functions:
### - fListWS()   : show variables in workspace
### - fClearWS()  : clear workspace
### - fMergeDF()  : merge list of data.frame
### - fFindIdx()  : find point in vector greater/less than value
### - fChronStr() : convert date/time string to chron
### - fVarName()  : name of variable(s) in data.frame
###
### version 2.5, Mar 2019
### author: RS
### ---------------------------------------------------------------- ###

fListWS <- function(arg="") {
  ## Show variables in R workspace:
  ## * if no argument is given, atmosch-R functions/variables are
  ##   excluded
  ## * if argument is "atmosch", only the atmosch-R
  ## * functions/variables are shown
  ##
  ## NB: the names of all atmosch-R functions/variables begin with
  ## lowercase `f` followed by a capital letter (functions) or by a
  ## dot (variables) -- see documentation of fClearWS() and ls().
  ## ------------------------------------------------------------
  vv1 <- ls(pos=.GlobalEnv)
  vv2 <- ls(pos=.GlobalEnv, pattern="^f[.A-Z]")
  vv3 <- ls(pos=.GlobalEnv, pattern="^f[A-Z]")
  vv4 <- ls(pos=.GlobalEnv, pattern="^f[.]")
  if (arg == "atmosch") {
    cat("\natmosch-R variables:\n")
    print(vv4)
    cat("\natmosch-R functions:\n")
    print(vv3)
  } else {
    print(base::setdiff(vv1, vv2))
  }
}

fClearWS <- function() {
  ## Clear R workspace:
  ## * delete all variables
  ## * keep atmosch-R functions/variables
  ## * close plot windows
  ##
  ## NB: the names of all atmosch-R functions/variables begin with
  ## lowercase `f` followed by a capital letter (functions) or by a
  ## dot (variables) -- see documentation of fListWS().
  ## ------------------------------------------------------------
  vv1 <- ls(pos=.GlobalEnv)
  vv2 <- ls(pos=.GlobalEnv, pattern="^f[.A-Z]")
  rm(list=base::setdiff(vv1, vv2), pos=.GlobalEnv)
  graphics.off()
}

fMergeDF <- function(df.lst, var.str, all.str, suff.lst) {
  ## Merge two or more data.frames using a common variable and rename
  ## the other variables.
  ##
  ## NB: the base function merge() only works on two data.frames at a
  ## time -- see documentation of merge()
  ##
  ## input:
  ##     df.lst = list of data.frames to merge
  ##     var.str = name of common variable
  ##     all.str = type of merge operation ("TRUE" OR "FALSE")
  ##     suff.lst = list of suffixes to rename variables
  ## output:
  ##     df.merg = data.frame ( merged data )
  ## ------------------------------------------------------------
  if (!is.list(df.lst)) {
    lst.name <- deparse(substitute(df.lst))
    stop(paste(lst.name, "must be a list", sep=" "))
  }
  if (length(df.lst) == 2 ) {  # two data.frames
    df.merg <- merge(df.lst[1], df.lst[2], by=var.str, all=as.logical(all.str),
                     suffixes=unlist(suff.lst))
  } else {                     # multiple data.frames
    df.merg <- as.data.frame(df.lst[1])
    var.n <- which(colnames(df.merg) == var.str)
    colnames(df.merg)[-var.n] <- paste(colnames(df.merg)[-var.n],
                                       suff.lst[[1]], sep="")
    for (i in 2:length(df.lst)) {
      df.i <- as.data.frame(df.lst[i])
      colnames(df.i)[-var.n] <- paste(colnames(df.i)[-var.n],
                                      suff.lst[[i]], sep="")
      df.merg <- merge(df.merg, df.i, by=var.str, all=as.logical(all.str))
    }
  }
  ## merged data.frame
  return(df.merg)
}

fFindIdx <- function(vecd, ops, xval) {
  ## Find the first point greater/less than a reference value in an
  ## ordered data/chron vector.
  ##
  ## input:
  ##     vecd = ordered data/chron vector
  ##     ops = greater/equal ("GE") OR greater ("G") OR
  ##           less ("L") OR less/equal ("LE")
  ##     xval = reference value
  ## output:
  ##     xv = index of point in vector greater/less than reference value
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
  ## Convert date, time, datetime string vector to chron vector with
  ## format "d-m-y h:m:s".
  ##
  ## input:
  ##     dt.str = date/time string vector
  ##     dt.fmt = format of date/time string ("d/m/y h:m:s" OR
  ##                                          "d/m/y" OR "h:m:s")
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

fVarName <- function(var.dat) {
  ## Return a string with the name of one or more variables in a
  ## data.frame.
  ##
  ## NB: a variable can be addressed by column number (df[1], df[,1])
  ## or by name df["A"]) or with the $ operator (df$A).
  ##
  ## input:
  ##    var.dat = variable(s) in data.frame
  ## output:
  ##    var.name = name of variable(s)
  ## ------------------------------------------------------------
  if (is.data.frame(var.dat)) {              # df[1] OR df["A"]
    var.name <- colnames(var.dat)
  } else {
    var.char <- deparse(substitute(var.dat))
    if (grepl("$", var.char, fixed=TRUE)) {  # df$A
      var.str <- strsplit(var.char, "$", fixed=TRUE)
      var.name <- unlist(var.str)[2]
    } else {                                 # df[,1]
      var.name <- "" #! FIX THIS !#
    }
  }
  return(var.name)
}
