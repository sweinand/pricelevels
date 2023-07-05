# START

# Title:  Speed comparisons for calculating bilateral index pairs
# Author: Sebastian Weinand
# Date:   2020-05-10

# set working directory:
setwd("C:\\Users\\User\\Dropbox\\Regionaler Preisvergleich Deutschland\\Measuring spatial price differentials")

# packages:
library(data.table)

# old functions:
source("R/Funktionen/calcIndex_v01.R")
source("R/Funktionen/calcIndexSystem_v02.R")
source("R/Funktionen/sampleGaps_v03.R")
source("R/Funktionen/samplePrices_v01.R")

#  FULL PRICE DATA --------------------------------------------------------

# sample prices:
prices_df <- samplePrices(region=300,product=400,price = c("mean"=100,"sd"=20), missings = list("avg"=0))

# old approach:
start_time <- proc.time()
prices_mat <- with(data = prices_df, tapply(X = price, INDEX = list(product, region), FUN = mean))
new <- spIndex::index_pairs(prices = prices_mat, index = "jevons", all_pairs = FALSE, as_df = FALSE)
proc.time() - start_time

# new approach:
start_time <- proc.time()
old <- with(data = prices_df, calcIndexSystem(region, product, price, index = "Jevons", redundancies = FALSE, matrix = TRUE))
proc.time() - start_time

diag(old)<-1
all.equal(new, old)


# INCOMPLETE PRICE DATA ---------------------------------------------------

# sample prices:
prices_df <- samplePrices(region=300,product=400,price = c("mean"=100,"sd"=20), missings = list("avg"=0.5))

# old approach:
start_time <- proc.time()
prices_mat <- with(data = prices_df, tapply(X = price, INDEX = list(product, region), FUN = mean))
new <- spIndex::index_pairs(prices = prices_mat, index = "jevons", all_pairs = FALSE, as_df = FALSE)
proc.time() - start_time

# new approach:
start_time <- proc.time()
old <- with(data = prices_df, calcIndexSystem(region, product, price, index = "Jevons", redundancies = FALSE, matrix = TRUE))
proc.time() - start_time

diag(old)<-1
all.equal(new, old)

# END
