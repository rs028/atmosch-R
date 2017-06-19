### ---------------------------------------------------------------- ###
### functions to read data files from commercial instruments:
### - fImport_TS49i() : Thermo Scientific O3 Monitor
###
### version 1.0, May 2017
### author: RS
### ---------------------------------------------------------------- ###

fRead_TS49i <- function(data.dir, data.fn, data.log, data.var=NULL) {
  ## Thermo Scientific Ozone monitor (model 49i)
  ##
  ## data can be logged in two ways:
  ## 1) using the iPort program which exports to a csv file
  ## 2) streaming to a terminal which writes to a space-delimited
  ## file. the streaming variables can be set by the user, if
  ## different from the default variables.
  ##
  ## input:
  ##     data.dir = data file directory
  ##     data.fn = name of data file
  ##     data.log = "iport" (iPort program) OR
  ##                "term" (terminal)
  ##     data.var = user-set streaming variables     [OPTIONAL]
  ## output:
  ##     data.out = data.frame ( date/time chron variables,
  ##                             data variables )
  ## ------------------------------------------------------------
  ## load data file
  data.file <- paste(data.dir, data.fn, sep="")
  if (data.log == "iport") {       # log with iPort
    data.df <- read.delim(data.file, header=TRUE, sep="", skip=5)
    data.df$Time <- paste(data.df$Time, "00", sep=":")
  } else if (data.log == "term"){  # log with terminal
    data.df <- read.delim(data.file, header=F, sep="")
    if (!is.null(data.var)) { # user-set streaming variables
      print(ncol(data.df))
      colnames(data.df) <- data.var
    } else {                  # default streaming variables
      colnames(data.df) <- c("Time", "Date", "Flags", "o3", "cellai",
                             "cellbi", "noisa", "noisb", "flowa",
                             "flowb", "pres")
    }
  } else {                         # log unknown
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
