### ---------------------------------------------------------------- ###
### functions to read data files from commercial instruments:
### - fRead_Thermo() : Thermo Scientific monitors
###
### version 1.2, Mar 2019
### author: RS
### ---------------------------------------------------------------- ###

fRead_Thermo <- function(data.dir, data.fn, model.n, data.log, data.var=NULL) {
  ## Thermo Scientific monitors.
  ##
  ## The monitors can log data in two ways:
  ## 1. using the default iPort program which exports to a csv file
  ##    with header.
  ## 2. streaming to a terminal (e.g., TeraTerm) which saves to a
  ##    delimited text file without header. The default streaming
  ##    variables can be changed from the instrument control panel.
  ##
  ## input:
  ##     data.dir = data file directory
  ##     data.fn = name of data file
  ##     model.n = instrument model number
  ##     data.log = "iport" OR "stream"
  ##     data.var = user-set streaming variables     [ OPTIONAL ]
  ## output:
  ##     data.out = data.frame ( date/time chron variables,
  ##                             data variables )
  ## ------------------------------------------------------------
  data.file <- paste(data.dir, data.fn, sep="")
  ## data logged with iPort
  if (data.log == "iport") {
    data.df <- read.table(data.file, header=TRUE, sep="", skip=5)
    data.df$Time <- paste(data.df$Time, "00", sep=":")
  ## data streaming to terminal
  } else if (data.log == "term"){
    data.df <- read.table(data.file, header=F, sep="")
    ## user-set streaming variables
    if (!is.null(data.var)) {
      print(ncol(data.df))
      colnames(data.df) <- data.var
    ## default streaming variables
    } else {
      switch(model.n,
             "42iTL" = { # NOx monitor
               colnames(data.df) <- c("Time", "Date", "no",
                                      "no2", "nox", "pre", "intt",
                                      "pres","smplf")
             },
             "49i" = {  # O3 monitor
               colnames(data.df) <- c("Time", "Date", "Flags", "o3",
                                      "cellai", "cellbi", "noisa", "noisb",
                                      "flowa", "flowb", "pres")
             },
             stop("INPUT ERROR: model number not found")
             )
    }
  ## log unknown
  } else {
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
