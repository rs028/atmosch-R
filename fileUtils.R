### ---------------------------------------------------------------- ###
### functions for :
###  1. load text data file
###
### version 1.0, Apr 2016
### author: RS
### ---------------------------------------------------------------- ###

fImportTXT <- function(data.dir, data.fn, data.sep, data.miss, ...) {
  ## 1. load data from a delimited text file (tab, space, comma,
  ## semicolon, etc...), convert the date/time variables to chron and
  ## replace missing data points with NA
  ##
  ## data files must be without header (except names of the variables)
  ## and must have at least one date/time variable in the first
  ## column:
  ##
  ##   time        variable       variable     variable
  ##  12:00:00        10             25           30
  ##  12:30:00        25             30           45
  ##  13:00:00        40             55           60
  ##   ...
  ##
  ## input:
  ##     data.dir = data file directory
  ##     data.fn = name of data file
  ##     data.sep = delimiter of data file (e.g., "\t" OR ",")
  ##     data.miss = missing values (e.g., -9999 OR "" OR NA)
  ##     ... = format of date/time variables ("d/m/y h:m:s", "h:m:s"
  ##           OR "FD" if fractional days are used)
  ## output:
  ##     data.out = data.frame ( date/time variables, data variables )
  ## ------------------------------------------------------------
  ## load data file
  data.file <- paste(data.dir, data.fn, sep="")
  if (data.sep == " ") {
    data.df <- read.delim(data.file, header=TRUE, sep="")
  } else {
    data.df <- read.delim(data.file, header=TRUE, sep=data.sep)
  }
  ## format of date/time variables
  time.fmt <- list(...)
  n.time <- length(time.fmt)
  time.df <- data.frame(F1 = rep(NA,nrow(data.df)))
  ## convert date/time variables to chron
  for (t in 1:n.time) {
    if (time.fmt[t] == "FD") {  # fractional days
      time.vec <- data.df[[t]]
    } else {                    # date/time
      time.vec <- fChronStr(data.df[[t]], time.fmt[[t]])
    }
    time.df <- cbind(time.df, time.vec)
  }
  ## rename time variables
  time.df <- time.df[-1]
  colnames(time.df) <- colnames(data.df[1:n.time])
  ## set missing values to NA
  data.filt <- data.df[(n.time+1):ncol(data.df)]
  data.filt[data.filt == data.miss] <- NA
  data.filt[is.na(data.filt)] <- NA
  ## output data.frame
  n.data <- ncol(data.df)
  data.out <- cbind(time.df, data.filt)
  return(data.out)
}
