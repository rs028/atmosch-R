### ---------------------------------------------------------------- ###
### functions for the MCM/AtChem model:
### - fAtchemIn()  : AtChem input files
### - fAtchemOut() : AtChem output files
###
### version 0.9, Mar 2017
### author: RS
### ---------------------------------------------------------------- ###

fAtchemIn <- function(input.dir, input.df, start.str) {
  ## make input files for the Atchem model
  ##
  ## the data.frame with the input data must have one datetime
  ## variable (TIME) and one or more data variables:
  ##
  ##        TIME           variable 1     variable 2   variable 3
  ##  21-01-15 12:00:00        10             25           30
  ##  21-01-15 12:30:00        25             30           45
  ##  21-01-15 13:00:00        40             55           60
  ##   ...
  ##
  ## input:
  ##     input.dir = directory for input files
  ##     input.df = input data.frame
  ##     start.str = model start datetime string ("d-m-y h:m:s")
  ## output:
  ##     init = list of values at model start datetime
  ##     --> `fn.str'
  ## ------------------------------------------------------------
  if (!is.data.frame(input.df)) {
    df.name <- deparse(substitute(input.df))
    stop(paste(df.name, "must be a data.frame", sep=" "))
  }
  ## time in seconds since model start
  model.start <- fChronStr(start.str, "d-m-y h:m:s")
  input.df$SEC <- round((input.df$TIME - model.start) * 86400)
  cc <- ncol(input.df) - 1
  ## write each variable to file and make list of initial values
  init <- list()
  for (c in 2:cc) {
    inp.str <- fVarName(input.df[c])
    ## set negative values to zero and remove rows with NaN
    inp.var <- ifelse(input.df[[c]] < 0, 0, input.df[[c]])
    df.in <- data.frame(SEC=input.df$SEC, VAR=inp.var)
    df.in <- df.in[which(!is.na(df.in$VAR)),]
    ## get initial value
    a1 <- fFindIdx(df.in$SEC, "LE", 0)
    init <- rbind(init, c(inp.str, df.in[a1,2]))
    ## write to file
    in.str <- fVarName(input.df[c])
    write(nrow(df.in), file=paste(input.dir, in.str, sep=""))
    write.table(df.in, file=paste(input.dir, in.str, sep=""),
                append=T, sep="\t", row.names=F, col.names=F)
  }
  return(init)
}

fAtchemOut <- function(output.dir, start.str) {
  ## load MCM output files with diagnostic and environmental
  ## variables
  ##
  ## input:
  ##       mcm_dir = model output directory
  ##       start.str = model start datetime string ("d-m-y h:m:s")
  ## output:
  ##        data.frame ( seconds, datetime, date, time,
  ##                     variable1, variable2, ... )
  ## ------------------------------------------------------------
  ## load output files
  fn.conc <- paste(output.dir, "concentration.output", sep="")
  fn.envir <- paste(output.dir, "envVar.output", sep="")
  fn.photo <- paste(output.dir, "photolysisRates.output", sep="")
  fn.param <- paste(output.dir, "photoRateCalcParameters.output", sep="")
  ## load output files
  out.conc <- read.delim(fn.conc, header=TRUE, sep="")
  out.envir <- read.delim(fn.envir, header=TRUE, sep="")
  out.photo <- read.delim(fn.photo, header=TRUE, sep="")
  out.param <- read.delim(fn.param, header=TRUE, sep="")
  ##
  df.out <- out.conc
  df.out <- merge(out.conc, out.envir, by="time")
  df.out <- merge(df.out, out.photo, by="time")
  df.out <- merge(df.out, out.param, by="time")
  #
  colnames(df.out) <- toupper(colnames(df.out))
  return(df.out)
}
