### ---------------------------------------------------------------- ###
### functions to import/export data files:
### - fImportTXT() : delimited text files
###
### version 1.3, Apr 2019
### author: RS
### ---------------------------------------------------------------- ###

fImportTXT <- function(data.dir, data.fn, data.sep, data.miss, ...) {
  ## Import data from a delimited text file (tab, space, comma,
  ## semicolon). Convert the date/time variables to chron and replace
  ## missing data points with NA.
  ##
  ## The data files must have no header, except one row with the names
  ## of the variables, and one or more date/time variables:
  ##
  ##   date variable    time variable    variable 1    variable 2
  ##     12/01/2009       12:00:00           10            25
  ##     12/01/2009       12:30:00           25            30
  ##     12/01/2009       13:00:00           40            55
  ##     ...              ...                ...           ...
  ##
  ## input:
  ##     data.dir = data file directory
  ##     data.fn = name of data file
  ##     data.sep = delimiter of data file (e.g., "\t" OR ",")
  ##     data.miss = missing value flag (e.g., -9999 OR "" OR NaN)
  ##     ... = format of date/time variables (e.g., "d/m/y h:m:s" OR
  ##           "FD" if fractional days are used)
  ## output:
  ##     data.out = data.frame ( date/time chron variables,
  ##                             data variables )
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
  data.out <- cbind(time.df, data.filt)
  return(data.out)
}
