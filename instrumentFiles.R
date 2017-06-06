### ---------------------------------------------------------------- ###
### functions to read data files by commercial scientific instruments:
### - fImport_TS49i() : Thermo Scientific O3 Monitor
###
### version 0.9, May 2017
### author: RS
### ---------------------------------------------------------------- ###

fRead_TS49i <- function(data.dir, data.fn, data.log, data.var=NULL) {
  ## the Thermo Scientific Ozone monitor (model 49i) can log data in
  ## two ways: 1. using the iPort software which exports to a csv file
  ## 2. streaming through a serial port to a terminal. the variables
  ## in this case cn be specified
  ##
  ## input:
  ##     data.dir = data file directory
  ##     data.fn = name of data file
  ##     data.log = "iport" (logged with iPort software) OR
  ##                "term" (logged with terminal)
  ##     data.var = list of variables [optional]
  ## output:
  ##     data.out = data.frame ( date/time variables, data variables )
  ## ------------------------------------------------------------
  ## open data file
  data.file <- paste(data.dir, data.fn, sep="")
  if (data.log == "iport") {       # 
    data.df <- read.delim(data.file, header=TRUE, sep="", skip=5)
    data.df$Time <- paste(data.df$Time, "00", sep=":")
  } else if (data.log == "term"){  # 
    data.df <- read.delim(data.file, header=F, sep="")
    if (!is.null(data.var)) {
      print(ncol(data.df))
      colnames(data.df) <- data.var
    } else {
      colnames(data.df) <- c("Time", "Date", "Flags", "o3", "cellai", "cellbi",
                             "noisa", "noisb", "flowa", "flowb", "pres")
    }
  } else {                         # 
    stop("logger not specified")
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
