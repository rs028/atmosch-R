### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### Functions for the AtChem2 model (https://github.com/AtChem/AtChem2):
### - fAtchemIn()    : constraint files
### - fAtchemOut()   : output files
### - fAtchemRates() : output files (production/loss rates)
### - fConstrGap()   : find gaps in constrained data
###
### version 1.6, Jun 2020
### author: RS
### ---------------------------------------------------------------- ###

fAtchemIn <- function(constr.dir, constr.df, start.str) {
  ## Generate constraint files for the AtChem2 model:
  ## * concentrations (molecule cm-3)
  ## * temperature (K)
  ## * pressure (mbar)
  ## * relative humidity (%)
  ## * water concentration (molecule cm-3)
  ## * sun declination (rad)
  ## * boundary layer height (cm)
  ## * dilution rate (s-1)
  ## * photolysis rates (s-1)
  ## * JFAC (scaling factor for photolysis rates)
  ##
  ## The data.frame with the constraint data must have one datetime
  ## chron variable (`TIME`), and one or more data variables:
  ##
  ##         TIME           variable 1    variable 2    variable 3
  ##   21-01-15 12:00:00        10            25            30
  ##   21-01-15 12:30:00        25            30            45
  ##   21-01-15 13:00:00        40            55            60
  ##   ...                      ...           ...           ...
  ##
  ## input:
  ##     constr.dir = constraint files directory
  ##     constr.df = data.frame with model constraints
  ##     start.str = model start datetime string ("d-m-y h:m:s")
  ## output:
  ##     init = list of values at model start datetime
  ##     --> constraint files in `constr.dir`
  ## ------------------------------------------------------------
  if (!is.data.frame(constr.df)) {
    df.name <- deparse(substitute(constr.df))
    stop(paste(df.name, "must be a data.frame", sep=" "))
  }
  ## time in seconds since model start
  model.start <- fChronStr(start.str, "d-m-y h:m:s")
  constr.df$SEC <- round((constr.df$TIME - model.start) * 86400)
  ## write each variable to file and make list of initial values
  init <- list()
  for (c in 2:(ncol(constr.df)-1)) {
    inp.str <- fVarName(constr.df[c])
    ## set negative values to zero and remove rows with NaN
    inp.var <- ifelse(constr.df[[c]] < 0, 0, constr.df[[c]])
    df.in <- data.frame(SEC=constr.df$SEC, VAR=inp.var)
    df.in <- df.in[which(!is.na(df.in$VAR)),]
    ## find initial value of variable
    a1 <- fFindIdx(df.in$SEC, "LE", 0)
    init <- rbind(init, c(inp.str, df.in[a1,2]))
    ## save constraint file
    in.str <- fVarName(constr.df[c])
    write.table(df.in, file=paste(constr.dir, in.str, sep=""),
                sep="\t", row.names=F, col.names=F)
  }
  return(init)
}

fAtchemOut <- function(output.dir, output.lst, start.str) {
  ## Import output files from the AtChem2 model:
  ## * concentrations of chemical species
  ## * environment variables
  ## * photolysis rates
  ## * diagnostic variables
  ##
  ## input:
  ##       output.dir = model output directory
  ##       output.lst = list of output files
  ##       start.str = model start datetime string ("d-m-y h:m:s")
  ## output:
  ##        data.frame ( seconds, datetime chron, variable1, variable2, ... )
  ## ------------------------------------------------------------
  if (!is.list(output.lst)) {
    lst.name <- deparse(substitute(output.lst))
    stop(paste(lst.name, "must be a list", sep=" "))
  }
  ## load output files
  df.res <- read.table(paste(output.dir, output.lst[1], sep=""), header=TRUE, fill=TRUE, sep="")
  if (length(output.lst) > 1) {
    for (i in 2:length(output.lst)) {
      res.i <- read.table(paste(output.dir, output.lst[i], sep=""), header=TRUE, fill=TRUE, sep="")
      df.res <- merge(df.res, res.i, by="t")
    }
  }
  ## model time (seconds since model start)
  model.start <- fChronStr(start.str, "d-m-y h:m:s")
  df.out <- data.frame(SEC=df.res$t)
  df.out$datetime <- df.out$SEC / 86400 + model.start
  ## output data.frame
  df.out <- cbind(df.out, df.res[-1])
  colnames(df.out) <- toupper(colnames(df.out))
  return(df.out)
}

fAtchemRates <- function(output.dir, output.file, species.str, start.str) {
  ## Import output files from the AtChem2 model:
  ## * production rates of target species
  ## * loss rates of target species
  ##
  ## input:
  ##       output.dir = model output directory
  ##       output.file = reaction rates output file
  ##       species.str = target species
  ##       start.str = model start datetime string ("d-m-y h:m:s")
  ## output:
  ##        list( data.frame ( reactions involving target species ),
  ##              data.frame ( time, reaction rate, reaction ) )
  ## ------------------------------------------------------------
  ## load reaction rates output file
  df.rates <- read.table(paste(output.dir, output.file, sep=""), header=TRUE, fill=TRUE, sep="")
  ## model start time
  model.start <- fChronStr(start.str, "d-m-y h:m:s")
  ## reaction rates of target species
  df.spec <- df.rates[which(df.rates$speciesName==species.str),
                      c("time", "reactionNumber", "rate", "reaction")]
  ## model time (seconds since model start)
  df.spec$datetime <- df.spec$time / 86400 + model.start
  ## list of reactions that include target species
  df1 <- df.spec$time[1]
  df.reac <- df.spec[which(df.spec$time == df1),
                     c("reactionNumber", "reaction")]
  ## output list
  lst.out <- list(df.reac, df.spec)
  return(lst.out)
}

fConstrGap <- function(constr.dir, constr.lst, max.gap, fn.str) {
  ## Parse the AtChem2 constraint files and find gaps in the constraint
  ## data that are larger than the data gap threshold.
  ##
  ## Optional: save plots of model constraints to pdf file.
  ##
  ## NB: use fAtchemIn() to generate the constraint files.
  ##
  ## input:
  ##     constr.dir = constraint files directory
  ##     constr.lst = list of constraint files
  ##     max.gap = data gap threshold (seconds)
  ##     fn.str = name of pdf file to save plots OR ""
  ## output:
  ##     df.out = data.frame ( name of variable, start time of gap,
  ##                           stop time of gap, duration of gap)
  ##     --> pdf file : `fn.str`.pdf
  ## ------------------------------------------------------------
  if (!is.list(constr.lst)) {
    lst.name <- deparse(substitute(constr.lst))
    stop(paste(lst.name, "must be a list", sep=" "))
  }
  ## open pdf file to save plots [optional]
  if (fn.str != "") {
    pdf(paste(fn.str, ".pdf", sep=""), paper="a4r", width=0, height=0)
  }
  ## import constraint files
  data.df <- data.frame()
  for (constr in constr.lst) {
    constr.df <- read.table(paste(constr.dir, constr, sep=""))
    ## flag gaps in data larger than threshold
    constr.df$V3 <- c(NA, diff(constr.df$V1, 1))
    constr.df$V4 <- ifelse(constr.df$V3 > max.gap, 0, 1)
    flag1 <- which(constr.df$V4 != 1)
    flag2 <- flag1 - 1
    ## add name of constraint variable and time interval
    if (length(flag1) >= 1) {
      sec.df <- data.frame(VARIABLE=constr,
                           START=constr.df[flag2,1],
                           STOP=constr.df[flag1,1])
      data.df <- rbind(data.df, sec.df)
    }
    ## make plot [optional]
    plot(constr.df[,1], constr.df[,2], type="b",
         main=constr, xlab="seconds", ylab="")
    points(constr.df[flag2,1], constr.df[flag2,2], pch=16, col="green", cex=2)
    points(constr.df[flag1,1], constr.df[flag1,2], pch=16, col="red", cex=2)
  }
  ## close pdf file
  if (fn.str != "") {
    dev.off()
  }
  ## output data.frame
  data.df$DELTA <- data.df$STOP - data.df$START
  if (nrow(data.df) != 0) {
    df.out <- data.df[order(data.df$START),]
  } else {
    df.out <- data.df
  }
  return(df.out)
}
