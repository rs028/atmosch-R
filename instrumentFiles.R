### ---------------------------------------------------------------- ###
### functions to read data files from commercial instruments:
### - fRead_Thermo() : Thermo Scientific monitors
###
### version 1.4, June 2020
### author: RS
### ---------------------------------------------------------------- ###

fRead_Thermo <- function(data.dir, data.fn, type.str, data.var=NULL) {
  ## Thermo Scientific monitors.
  ##
  ## The Thermo Scientific monitors can log data in two ways:
  ## 1. using the default iPort program which exports to a delimited
  ##    file with header.
  ## 2. streaming to a terminal (e.g., TeraTerm) which saves to a
  ##    delimited file without header. The default streaming variables
  ##    are different for each monitor, but can be changed from the
  ##    instrument control panel.
  ##
  ## input:
  ##     data.dir = data file directory
  ##     data.fn = name of data file
  ##     type.str = "iport" OR "42c" OR "42i" OR "42iTL" OR
  ##                 "49i" OR "user"
  ##     data.var = user-set streaming variables     [ OPTIONAL ]
  ## output:
  ##     data.out = data.frame ( date/time chron variables,
  ##                             data variables )
  ## ------------------------------------------------------------
  data.file <- paste(data.dir, data.fn, sep="")
  ## import data file
  switch(type.str,
         "iport" = {  # any monitor - iPort mode
           data.df <- read.table(data.file, header=TRUE, fill=TRUE, sep="", skip=5)
         },
         "42c" = {    # NOx monitor 42c - iPort mode
           data.df <- read.table(data.file, header=FALSE, fill=TRUE, sep="", skip=6)
           colnames(data.df) <- c("Time", "Date", "Flags", "no", "nox", "pmt_volt",
                                  "pmt_temp", "int_temp", "chamb_temp", "conv_temp",
                                  "pres", "samp_flow", "o3_flow")
           ## get year from file header
           info.str <- readLines(data.file, n=2)[2]
           yr.str <- substr(info.str, nchar(info.str)-1, nchar(info.str))
           data.df$Date <- paste(data.df$Date, yr.str, sep="-")
         },
         "42i" = {    # NOx monitor 42i - streaming mode
           data.df <- read.table(data.file, header=FALSE, fill=TRUE, sep="")
           colnames(data.df) <- c("Time", "Date", "Flags", "no", "no2", "nox", "intt",
                                  "pres", "smplf")
         },
         "42iTL" = {  # NOx monitor 42iTL - streaming mode
           data.df <- read.table(data.file, header=FALSE, fill=TRUE, sep="")
           colnames(data.df) <- c("Time", "Date", "no", "no2", "nox", "pre", "intt",
                                  "pres", "smplf")
         },
         "49i" = {    # O3 monitor 49i - streaming mode
           data.df <- read.table(data.file, header=FALSE, fill=TRUE, sep="")
           colnames(data.df) <- c("Time", "Date", "Flags", "o3", "cellai", "cellbi",
                                  "noisa", "noisb", "flowa", "flowb", "pres")
         },
         "user" = {   # any monitor - streaming mode (user-set variables)
           data.df <- read.table(data.file, header=FALSE, fill=TRUE, sep="")
           if (!is.null(data.var) & (ncol(data.df) == length(data.var))) {
             colnames(data.df) <- data.var
           } else {
             stop("INPUT ERROR: streaming variables missing")
           }
         },
         stop("INPUT ERROR: monitor not found")
         )
  ## convert date and time to chron
  if (nchar(as.character(data.df$Time[1])) == 5) {
    data.df$Time <- paste(data.df$Time, "00", sep=":")
  }
  data.d <- fChronStr(data.df[,2], "m-d-y")
  data.t <- fChronStr(data.df[,1], "h:m:s")
  ## datetime chron variable
  tst.dt <- paste(data.df[,2], data.df[,1], sep=" ")
  data.dt <- fChronStr(tst.dt, "m-d-y h:m:s")
  ## output data.frame
  data.time <- data.frame(data.dt, data.d, data.t)
  rownames(data.time) <- NULL
  colnames(data.time) <- c("Datetime", "Date", "Time")
  data.out <- cbind(data.time, data.df[-1:-2])
  return(data.out)
}
