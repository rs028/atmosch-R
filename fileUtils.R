### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### Functions to import/export data files:
### - fImportTXT() : delimited text files
###
### version 1.5, Feb 2026
### author: RS
### ---------------------------------------------------------------- ###

fImportTXT <- function(data.dir, data.fn, data.sep, data.miss, ...) {
  ## Import data from a delimited text file (tab, space, comma), convert
  ## date/time variables to chron, replace NaN and missing data points with NA.
  ##
  ## The data file must have one header row with variable names and one or more
  ## date/time variables in the first columns:
  ##
  ##   date variable    time variable    variable 1    variable 2
  ##     12-01-2009       12:00:00           10            25
  ##     12-01-2009       12:30:00           25            30
  ##     12-01-2009       13:00:00           40            55
  ##     ...              ...                ...           ...
  ##
  ## NB: date/time is not converted to chron if it is in fractional days ("FD").
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
  ## ---------------------------------------------------------------------
  data.file <- file.path(data.dir, data.fn)
  if (!file.exists(data.file)) {
    stop("INPUT ERROR: file ", data.file, " does not exist")
  }
  ## read data file
  if (data.sep == " ") {
    data.df <- read.delim(data.file, header=TRUE, fill=TRUE, sep="")
  } else {
    data.df <- read.delim(data.file, header=TRUE, fill=TRUE, sep=data.sep)
  }
  ## date/time variable formats
  time.fmt <- list(...)
  n.time <- length(time.fmt)
  if (n.time == 0) {
    stop("INPUT ERROR: date/time format must be provided")
  }
  ## convert date/time variables to chron
  time.lst <- vector("list", n.time)
  for (t in seq_len(n.time)) {
    if (time.fmt[[t]] == "FD") {
      time.lst[[t]] <- data.df[[t]]
    } else {
      time.lst[[t]] <- fChronStr(data.df[[t]], time.fmt[[t]])
    }
  }
  time.df <- as.data.frame(time.lst)
  colnames(time.df) <- colnames(data.df)[1:n.time]
  ## replace missing values with NA and convert NaN to NA
  data.clean <- data.df[(n.time+1):ncol(data.df)]
  data.clean[data.clean == data.miss] <- NA
  data.clean[is.na(data.clean)] <- NA
  ## output data.frame
  data.out <- cbind(time.df, data.clean)
  return(data.out)
}
