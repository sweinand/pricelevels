# START

# Title:  Package options
# Author: Sebastian Weinand
# Date:   18 May 2024

.onLoad <- function(libname, pkgname){
  options(pricelevels.chatty=TRUE) # print package induced messages and warnings
  options(pricelevels.connect=TRUE) # check and connect price data
  options(pricelevels.check.inputs=TRUE) # check user inputs in functions
  options(pricelevels.missings=TRUE) # remove NAs from data
  options(pricelevels.duplicates=TRUE) # aggregate duplicated observations
  options(pricelevels.plot=FALSE) # plot price levels with ratios
}

.onAttach <- function(libname, pkgname){
  options(pricelevels.chatty=TRUE)
  options(pricelevels.connect=TRUE)
  options(pricelevels.check.inputs=TRUE)
  options(pricelevels.missings=TRUE)
  options(pricelevels.duplicates=TRUE)
  options(pricelevels.plot=FALSE)
}

# END
