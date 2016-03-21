### ---------------------------------------------------------------- ###
### functions for data processing and analysis:
###  1. load text data file
###
### version 0.1, Sep 2015
### author: RS
### ---------------------------------------------------------------- ###

fImportData <- function(data.dir, data.fn, miss.flag, ...) {
  ## 1. load data from a text file (*.csv, *.dat), convert date/time
  ## to chron, replace missing data points with NA
  ##
  ## NB: time strings must have hours, minutes and seconds
  ##
  ## input:
  ##    data.dir = data file directory
  ##    data.fn = name of data file
  ##    miss.flag = missing value flag (e.g., -9999)
  ##    ... = date/time strings (e.g., "d/m/y h:m:s", "h:m:s")
  ## output:
  ##    data.out = data.frame ( time variables, data variables )
  ## ------------------------------------------------------------
  ## select type of data file
  fn.num <- nchar(data.fn)
  fn.ext <- substr(data.fn, (fn.num - 3), (fn.num))
  if (fn.ext == ".csv") {         # comma delimited
    fn.sep <- ","
  } else if (fn.ext == ".txt") {  # tab delimited
    fn.sep <- "\t"
  } else if (fn.ext == ".dat") {  # tab/space delimited
    fn.sep <- ""
  } else {
    stop("invalid file extension")
  }
  ## load data file
  data.file <- paste(data.dir, data.fn, sep="")
  data.df <- read.delim(data.file, header=TRUE, sep=fn.sep)
  ## convert date/time strings to chron
  time.fmt <- list(...)
  n.time <- length(time.fmt)
  time.df <- list()
  for (t in 1:n.time) {
    time.vec <- fChronStr(data.df[[t]], time.fmt[[t]])
    time.df[[t]] <- time.vec
  }
  time.df <- as.data.frame(time.df) #!
  colnames(time.df) <- colnames(data.df[1:n.time])
  ## set missing values to NA
  data.filt <- data.df[(n.time+1):ncol(data.df)]
  data.filt[data.filt == miss.flag] <- NA
  data.filt[is.na(data.filt)] <- NA
  ## output data.frame
  n.data <- ncol(data.df)
  data.out <- cbind(time.df, data.filt)
  return(data.out)
}
