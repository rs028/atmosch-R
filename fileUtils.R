### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### Functions to import/export data files:
### - fImportTXT() : delimited text files
###
### version 1.4, Apr 2021
### author: RS
### ---------------------------------------------------------------- ###

fImportTXT <- function(data.dir, data.fn, data.sep, data.miss, ...) {
  ## Import data from a delimited text file (tab, space, comma,
  ## etc...), convert the date/time variables to chron, and replace
  ## missing data points with NA.
  ##
  ## The data files must have no header, except one row with the names
  ## of the variables, and must have one or more date/time variables:
  ##
  ##   date variable    time variable    variable 1    variable 2
  ##     12-01-2009       12:00:00           10            25
  ##     12-01-2009       12:30:00           25            30
  ##     12-01-2009       13:00:00           40            55
  ##     ...              ...                ...           ...
  ##
  ## NB: date/time can be in fractional days ("FD"), in which case it
  ## is not converted to chron.
  ##
  ## INPUT:
  ##     data.dir = data file directory
  ##     data.fn = name of data file
  ##     data.sep = delimiter of data file (e.g., "\t" OR ",")
  ##     data.miss = missing value flag (e.g., -9999 OR "" OR NA)
  ##     ... = format of date/time variables (e.g., "d/m/y h:m" OR "y/m/d h:m" OR
  ##                                          ""d-m-y" OR "h:m:s" OR "FD")
  ## OUTPUT:
  ##     data.out = data.frame ( date/time chron variables,
  ##                             data variables )
  ## EXAMPLE:
  ##     xx <- fImportTXT("directory/", "filename.csv", ",", "-9999", "d-m-y", "h:m:s")
  ## ------------------------------------------------------------
  ## load data file
  data.file <- paste(data.dir, data.fn, sep="")
  if (data.sep == " ") {
    data.df <- read.delim(data.file, header=TRUE, fill=TRUE, sep="")
  } else {
    data.df <- read.delim(data.file, header=TRUE, fill=TRUE, sep=data.sep)
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
