### ---------------------------------------------------------------- ###
### obsolete functions - keep for backward compatibility:
### - fFindPnt()    : use fFindIdx()
### - fRead_TS42i() : use fRead_Thermo()
###
### version 0.1, Mar 2019
### author: RS
### ---------------------------------------------------------------- ###

fFindPnt <- function(vecd, ops, xval, xst) {
  ## find the first point greater/equal or less/equal than a reference
  ## value in a data vector starting from a given point in the data
  ## vector
  ##
  ## input:
  ##     vecd = data vector
  ##     ops = greater/equal ("GE") or less/equal ("LE")
  ##     xval = reference value
  ##     xst = starting point in data vector
  ## output:
  ##     xv = point in data vector greater/less than reference value
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

fRead_TS49i <- function(data.dir, data.fn, data.log, data.var=NULL) {
  ## Thermo Scientific Ozone monitor (model 49i).
  ##
  ## The TS 49i monitor can log data in two ways:
  ## 1. using the default iPort program which exports to a csv file
  ##    with header.
  ## 2. streaming to a terminal (e.g., TeraTerm) which saves to a
  ##    delimited text file without header.
  ##
  ## NB: the instrument streams 8 variables (plus timestamp and flags)
  ## by default; the streaming variables can be changed by the user
  ## from the instrument control panel.
  ##
  ## input:
  ##     data.dir = data file directory
  ##     data.fn = name of data file
  ##     data.log = "iport" (iPort program) OR
  ##                "term" (terminal)
  ##     data.var = user-set streaming variables     [ OPTIONAL ]
  ## output:
  ##     data.out = data.frame ( date/time chron variables,
  ##                             data variables )
  ## ------------------------------------------------------------
  ## load data file
  data.file <- paste(data.dir, data.fn, sep="")
  if (data.log == "iport") {       # log with iPort
    data.df <- read.table(data.file, header=TRUE, sep="", skip=5)
    data.df$Time <- paste(data.df$Time, "00", sep=":")
  } else if (data.log == "term"){  # log with terminal
    data.df <- read.table(data.file, header=F, sep="")
    if (!is.null(data.var)) {  # user-set streaming variables
      print(ncol(data.df))
      colnames(data.df) <- data.var
    } else {                   # default streaming variables
      colnames(data.df) <- c("Time", "Date", "Flags", "o3",
                             "cellai", "cellbi", "noisa", "noisb",
                             "flowa", "flowb", "pres")
    }
  } else {                     # log unknown
    stop("INPUT ERROR: logger not found")
  }
  ## convert date/time variables to chron
  tst.dt <- paste(data.df[,2], data.df[,1], sep=" ")
  data.d <- fChronStr(data.df[,2], "m-d-y")
  data.t <- fChronStr(data.df[,1], "h:m:s")
  data.dt <- fChronStr(tst.dt, "m-d-y h:m:s")
  data.time <- data.frame(data.dt, data.d, data.t)
  rownames(data.time) <- NULL
  colnames(data.time) <- c("Datetime", "Date", "Time")
  ## output data.frame
  data.out <- data.frame(data.time, data.df[-1:-2])
  return(data.out)
}
