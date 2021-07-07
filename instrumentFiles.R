### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### Functions to read data files from commercial instruments:
### - fRead_Thermo() : Thermo Scientific monitors
### - fRead_QCL()    : Aerodyne QCL-TILDAS
###
### version 1.6, Jul 2021
### author: RS
### ---------------------------------------------------------------- ###

fRead_Thermo <- function(data.dir, data.fn, type.str, data.var=NULL) {
  ## Thermo Scientific monitors.
  ##
  ## The Thermo Scientific monitors can be operated in two modes:
  ## 1. using the default iPort program, which exports data to a
  ##    space-delimited file with header (iPort mode).
  ## 2. streaming to a terminal, such as TeraTerm, which saves data to
  ##    a space-delimited file without header (streaming mode). The
  ##    default streaming variables are different for each monitor,
  ##    but can be changed from the instrument control panel.
  ##
  ## INPUT:
  ##     data.dir = data file directory
  ##     data.fn = name of data file
  ##     type.str = monitor mode ("iport" OR "42c" OR "42i" OR
  ##                              "42iTL" OR "49i" OR "user")
  ##     data.var = user-set streaming variables     [ OPTIONAL ]
  ## OUTPUT:
  ##     data.out = data.frame ( date/time chron variables,
  ##                             data variables )
  ## EXAMPLE:
  ##     xx <- fRead_Thermo("directory/", "filename.dat", "49i")
  ##     xx <- fRead_Thermo("directory/", "filename.dat", "user", c("Time","Date","no","no2","nox"))
  ## ------------------------------------------------------------
  ## import data file
  data.file <- paste(data.dir, data.fn, sep="")
  switch(type.str,
         "iport" = {  # any monitor - iPort mode
           data.df <- read.table(data.file, header=TRUE, fill=TRUE, sep="", skip=5)
         },
         "42c" = {    # NOx monitor 42c - iPort mode
           data.df <- read.table(data.file, header=FALSE, fill=TRUE, sep="", skip=6)
           colnames(data.df) <- c("Time", "Date", "Flags", "no", "nox", "pmtv", "pmtt",
                                  "intt", "rctt", "convt", "pres", "smplf", "ozonf")
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

fRead_QCL <- function(data.dir, data.fn, diagn) {
  ## Aerodyne QCL-TILDAS.
  ##
  ## The Aerodyne QCL-TILDAS saves data in two files:
  ## * a space-delimited data file (*.str)
  ## * a comma-delimited diagnostic file (*.stc)
  ##
  ## INPUT:
  ##     data.dir = data file directory
  ##     data.fn = name of data file
  ##     diagn = include diagnostic variables ("yes" OR "no")
  ## OUTPUT:
  ##     data.out = data.frame ( date/time chron variables,
  ##                             data variables )
  ## EXAMPLE:
  ##     xx <- fRead_QCL("directory/", "filename.dat", "yes")
  ## ------------------------------------------------------------
  ## import data file
  data.file <- paste(data.dir, data.fn, ".str", sep="")
  data.df <- read.table(data.file, header=FALSE, fill=TRUE, sep="", skip=1)
  colnames(data.df) <- c("Time", "HNO3", "H2O_a", "H2O_b", "H2O_c", "HONO", "H2O_d", "N2O_a", "N2O_b", "CH4")
  ## import diagnostic file
  if (diagn == "yes") {
    diagn.file <- paste(data.dir, data.fn, ".stc", sep="")
    diagn.df <- read.table(diagn.file, header=TRUE, fill=TRUE, sep=",", skip=1)
    colnames(diagn.df)[1] <- "Time"
    data.out <- merge(data.df, diagn.df, by="Time")
  } else {
    data.out <- data.df
  }
  ## output data.frame
  data.out$Datetime <- chron("01/01/1904", "00:00:00") + (data.df$Time / 86400)
  return(data.out)
}
