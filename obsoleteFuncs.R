### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### obsolete functions:     [ KEEP FOR BACKWARD COMPATIBILITY ]
### - fConvSI()
### - fFindPnt()     ==>  use fFindIdx()
### - fRead_TS42i()  ==>  use fRead_Thermo()
###
### author: RS
### ---------------------------------------------------------------- ###

fConvSI <- function(data.in, unit.in, unit.out) {
  ## Convert between multiples and fractions of SI units:
  ## * peta      = "P"
  ## * tera      = "T"
  ## * giga      = "G"
  ## * mega      = "M"
  ## * kilo      = "K"
  ## * hecto     = "H"
  ## * deca      = "D"
  ## * base unit = "-"
  ## * deci      = "d"
  ## * centi     = "c"
  ## * milli     = "m"
  ## * micro     = "u"
  ## * nano      = "n"
  ## * pico      = "p"
  ## * femto     = "f"
  ##
  ## INPUT:
  ##     data.in = data in original unit
  ##     unit.in = original measurement unit
  ##     unit.out = final measurement unit
  ## OUTPUT:
  ##     data.out = data in final unit
  ## EXAMPLE:
  ##     xx <- fConvSI(4321, "u", "-")
  ## ------------------------------------------------------------
  ## data in original unit to reference unit (base unit)
  switch(unit.in,
         "P" = {
           data.ref <- data.in * 1.0e+15
         },
         "T" = {
           data.ref <- data.in * 1.0e+12
         },
         "G" = {
           data.ref <- data.in * 1.0e+09
         },
         "M" = {
           data.ref <- data.in * 1.0e+06
         },
         "K" = {
           data.ref <- data.in * 1.0e+03
         },
         "H" = {
           data.ref <- data.in * 1.0e+02
         },
         "D" = {
           data.ref <- data.in * 1.0e+01
         },
         "-" = {
           data.ref <- data.in
         },
         "d" = {
           data.ref <- data.in * 1.0e-01
         },
         "c" = {
           data.ref <- data.in * 1.0e-02
         },
         "m" = {
           data.ref <- data.in * 1.0e-03
         },
         "u" = {
           data.ref <- data.in * 1.0e-06
         },
         "n" = {
           data.ref <- data.in * 1.0e-09
         },
         "p" = {
           data.ref <- data.in * 1.0e-12
         },
         "f" = {
           data.ref <- data.in * 1.0e-15
         },
         stop("INPUT ERROR: unit not found")
         )
  ## data in reference unit (base unit) to final unit
  switch(unit.out,
         "P" = {
           data.out <- data.ref / 1.0e+15
         },
         "T" = {
           data.out <- data.ref / 1.0e+12
         },
         "G" = {
           data.out <- data.ref / 1.0e+09
         },
         "M" = {
           data.out <- data.ref / 1.0e+06
         },
         "K" = {
           data.out <- data.ref / 1.0e+03
         },
         "H" = {
           data.out <- data.ref / 1.0e+02
         },
         "D" = {
           data.out <- data.ref / 1.0e+01
         },
         "-" = {
           data.out <- data.ref
         },
         "d" = {
           data.out <- data.ref / 1.0e-01
         },
         "c" = {
           data.out <- data.ref / 1.0e-02
         },
         "m" = {
           data.out <- data.ref / 1.0e-03
         },
         "u" = {
           data.out <- data.ref / 1.0e-06
         },
         "n" = {
           data.out <- data.ref / 1.0e-09
         },
         "p" = {
           data.out <- data.ref / 1.0e-12
         },
         "f" = {
           data.out <- data.ref / 1.0e-15
         },
         stop("INPUT ERROR: unit not found")
         )
  ## data in final unit
  return(data.out)
}

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
  } else if (data.log == "term") {  # log with terminal
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
