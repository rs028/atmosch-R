### ---------------------------------------------------------------- ###
### atmosch-R                                                        ###
### ---------------------------------------------------------------- ###
### Utilities and tools for atmosch-R functions:
### - fListWS()  : show variables in R workspace
### - fClearWS() : clear R workspace
### - fMergeDF() : merge list of data.frame
### - fFindIdx() : find point in vector greater/less than value
### - fVarName() : name of variable(s) in data.frame
###
### version 2.7, Apr 2021
### author: RS
### ---------------------------------------------------------------- ###

fListWS <- function(arg="") {
  ## Show functions/variables in the R workspace:
  ## * if no argument is given, atmosch-R functions/variables are
  ##   not shown
  ## * if argument is "atmosch", only the atmosch-R functions/variables
  ##   are shown
  ##
  ## NB: the names of all atmosch-R functions/variables begin with
  ## lowercase `f` followed by a capital letter (functions) or by a
  ## dot (variables).
  ##
  ## EXAMPLE:
  ##     fListWS("atmosch")
  ## ------------------------------------------------------------
  vv1 <- ls(pos=.GlobalEnv)
  vv2 <- ls(pos=.GlobalEnv, pattern="^f[.A-Z]")
  vv3 <- ls(pos=.GlobalEnv, pattern="^f[A-Z]")
  vv4 <- ls(pos=.GlobalEnv, pattern="^f[.]")
  if (arg == "atmosch") {
    cat("\natmosch-R variables:\n")
    print(vv4)
    cat("\natmosch-R functions:\n")
    print(vv3)
  } else {
    print(base::setdiff(vv1, vv2))
  }
}

fClearWS <- function() {
  ## Clear the R workspace:
  ## * delete all variables
  ## * keep atmosch-R functions/variables
  ## * close plot windows
  ##
  ## NB: the names of all atmosch-R functions/variables begin with
  ## lowercase `f` followed by a capital letter (functions) or by a
  ## dot (variables).
  ##
  ## EXAMPLE:
  ##     fClearWS()
  ## ------------------------------------------------------------
  vv1 <- ls(pos=.GlobalEnv)
  vv2 <- ls(pos=.GlobalEnv, pattern="^f[.A-Z]")
  rm(list=base::setdiff(vv1, vv2), pos=.GlobalEnv)
  graphics.off()
}

fMergeDF <- function(df.lst, var.str, type.str, suff.lst) {
  ## Merge two or more data.frames using a common variable and rename
  ## the other variables.
  ##
  ## NB: the base function merge() only works on two data.frames at a
  ## time -- see the documentation of merge().
  ##
  ## INPUT:
  ##     df.lst = list of data.frames to merge
  ##     var.str = name of common variable
  ##     type.str = type of merge operation ("ALL" to keep all rows OR
  ##                "NOTALL" to keep only the common rows)
  ##     suff.lst = list of suffixes to rename variables
  ## OUTPUT:
  ##     df.merg = data.frame ( merged data )
  ## EXAMPLE:
  ##     xx <- fMergeDF(list(data_df1,data_df2,data_df3), "Time", "ALL", list("_a","_b","_c"))
  ## ------------------------------------------------------------
  if (!is.list(df.lst)) {
    lst.name <- deparse(substitute(df.lst))
    stop(paste(lst.name, "must be a list", sep=" "))
  }
  ## set type of merge
  if (type.str == "ALL") {
    type.all <- TRUE
  } else {
    type.all <- FALSE
  }
  if (length(df.lst) == 2) {  # two data.frames
    df.merg <- merge(df.lst[1], df.lst[2], by=var.str, all=type.all,
                     suffixes=unlist(suff.lst))
  } else {                    # multiple data.frames
    df.merg <- as.data.frame(df.lst[1])
    var.n <- which(colnames(df.merg) == var.str)
    colnames(df.merg)[-var.n] <- paste(colnames(df.merg)[-var.n],
                                       suff.lst[[1]], sep="")
    for (i in 2:length(df.lst)) {
      df.i <- as.data.frame(df.lst[i])
      colnames(df.i)[-var.n] <- paste(colnames(df.i)[-var.n],
                                      suff.lst[[i]], sep="")
      df.merg <- merge(df.merg, df.i, by=var.str, all=type.all)
    }
  }
  return(df.merg)
}

fFindIdx <- function(vecd, ops, xval) {
  ## Find the first point greater/less than a reference value in an
  ## ordered data/chron vector.
  ##
  ## INPUT:
  ##     vecd = ordered data/chron vector
  ##     ops = greater/equal ("GE") OR greater ("G") OR
  ##           less ("L") OR less/equal ("LE")
  ##     xval = reference value
  ## OUTPUT:
  ##     xv = index of point in vector greater/less than reference value
  ## EXAMPLE:
  ##     xx <- fFindIdx(data_df$Datetime, "GE", chron("01/21/15","10:15:30"))
  ## ------------------------------------------------------------
  vecd <- unlist(vecd, use.names=FALSE)
  ## first and last values of data vector
  vecd.first <- vecd[1]
  vecd.last <- vecd[length(vecd)]
  switch(ops,
         "GE" = {  # GREATER/EQUAL
           if (xval >= vecd.last) {
             xv <- which(vecd == vecd.last)
           } else {
             xv <- which(vecd >= xval)
             xv <- xv[1]
           }
         },
         "G" = {  # GREATER
           if (xval > vecd.last) {
             xv <- which(vecd == vecd.last)
           } else {
             xv <- which(vecd > xval)
             xv <- xv[1]
           }
         },
         "L" = {  # LESS
           if (xval < vecd.first) {
             xv <- which(vecd == vecd.first)
           } else {
             xv <- which(vecd < xval)
             xv <- xv[length(xv)]
           }
         },
         "LE" = {  # LESS/EQUAL
           if (xval <= vecd.first) {
             xv <- which(vecd == vecd.first)
           } else {
             xv <- which(vecd <= xval)
             xv <- xv[length(xv)]
           }
         }
         )
  return(xv)
}

fVarName <- function(var.dat) {
  ## Extract the name of one or more variables in a data.frame.
  ##
  ## NB: a variable can be addressed using the column number (df[1] or
  ## df[,1]), the name (df["A"]), or the `$` operator (df$A).
  ##
  ## INPUT:
  ##    var.dat = variable(s) in data.frame
  ## OUTPUT:
  ##    var.name = name of variable(s)
  ## EXAMPLE:
  ##     xx <- fVarName(data_df$Methane.ppm)
  ## ------------------------------------------------------------
  if (is.data.frame(var.dat)) {              # df[1] OR df["A"]
    var.name <- colnames(var.dat)
  } else {
    var.char <- deparse(substitute(var.dat))
    if (grepl("$", var.char, fixed=TRUE)) {  # df$A
      var.str <- strsplit(var.char, "$", fixed=TRUE)
      var.name <- unlist(var.str)[2]
    } else {                                 # df[,1]
      var.name <- "" # !!! TODO
    }
  }
  return(var.name)
}
