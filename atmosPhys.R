### ---------------------------------------------------------------- ###
### functions for atmospheric physics:
###  1. humidity
###
### version 0.2, Jun 2014
### author: RS
### ---------------------------------------------------------------- ###

fHumid <- function() {
  ## 1. 
  ##
  ## input:
  ##    temp = temperature (K)
  ##    press = pressure (Pa)
  ## output:
  ##    df.out = 
  ## ------------------------------------------------------------
  ## saturation water vapour pressure over water
  #temp.c <- fConvTemp(temp, "K", "C")
  temp.c <- c(-20, -10, 0, 10, 20, 30, 40, 50)
  press.ws <- 100* 6.116441 * 10^((7.591386 * temp.c) / (temp.c + 240.7263))
  press.ws1 <- 610.78 * exp(temp.c/(temp.c+238.3) * 17.2694)
  press.ws2 <- 100* 6.11 * 10^((7.5 * temp.c)/(237.3 + temp.c))
  ## ## data in original unit to reference unit (K)
  ## switch(unit_in,
  ##        "K" = {
  ##          data_ref <- data_in
  ##        },
  ##        "C" = {
  ##          data_ref <- data_in + 273.15
  ##        },
  ##        "F" = {
  ##          data_ref <- (data_in + 459.67)*5/9
  ##        }
  ## )
  ## ## data in reference unit (K) to final unit
  ## switch(unit_out,
  ##        "K" = {
  ##          data_out <- data_ref
  ##        },
  ##        "C" = {
  ##          data_out <- data_ref - 273.15
  ##        },
  ##        "F" = {
  ##          data_out <- (data_ref*9/5) - 459.67
  ##        }
  ## )
  ## data in final unit
  df.out <- data.frame(temp.c, press.ws, press.ws1, press.ws2)
  return(df.out)
}

