### ---------------------------------------------------------------- ###
### functions to read data files from commercial instruments:
### - fImport_TS49i() : Thermo Scientific O3 Monitor
### - fSwitchFlag     : remove points before/after switch
###
### version 1.1, Feb 2019
### author: RS
### ---------------------------------------------------------------- ###

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

fSwitchFlag <- function(data.df, flag.var, flag.ref, skip.fore, skip.aft) {
  ## Find a switch (e.g., of a valve) and flag the points before/after
  ## the switch for later removal.
  ##
  ## input:
  ##     data.df = data.frame of instrument data
  ##     flag.var = flag used by the switch
  ##     flag.ref = flag used as reference
  ##     skip.fore = points to skip before
  ##     skip.aft = points to skip after
  ## output:
  ##     data.out = data.frame of instrument data with flag added
  ## ------------------------------------------------------------
  if (!is.data.frame(data.df)) {
    df.name <- deparse(substitute(data.df))
    stop(paste(df.name, "must be a data.frame", sep=" "))
  }
  ## find switch
  nf <- which(colnames(data.df) == flag.var)
  fl1 <- data.df[,nf]
  fl2 <- data.df[,nf]
  fl1[which(data.df[,nf] == flag.ref) - skip.aft] <- 9999
  fl2[which(data.df[,nf] == flag.ref) + skip.fore] <- 9999
  ## remove extra points
  n.data <- nrow(data.df)
  if ((length(fl1) - n.data) != 0) {
    fl1 <- fl1[-1:-(length(fl1)-n.data)]
  }
  if ((length(fl2) - n.data) != 0) {
    fl2 <- fl2[-(n.data+1):-length(fl2)]
  }
  ## create flag
  data.out <- data.df
  data.out$fl1 <- fl1
  data.out$fl2 <- fl2
  data.out$Flag <- ifelse((fl1 == fl2 & fl1 == 9999), 1,
                   ifelse((fl1 == fl2 & fl1 != 9999), 0, -1))
  ## output data.frame
  return(data.out)
}
