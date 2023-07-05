# START

# Title:  Speed comparisons for calculating price indices
# Author: Sebastian Weinand
# Date:   2020-05-12

# -> data.table solutions is fastest
# -> keep in mind that weights have to be re-normalized

# packages:
library(data.table)

# functions:
toernq <- function(region, product, price, weights, base = NULL){
   
   # check input for "region":
   if(missing(region)){stop("'region' is missing.")}
   if(!(is.vector(x = region, mode = "character") | is.factor(x = region))){stop("'region' must be a character vector or a factor.")}
   if(length(region) <= 0L){stop("'region' must be of length greater than 0.")}
   
   # check input for "product":
   if(missing(product)){stop("'product' is missing.")}
   if(!(is.vector(x = product, mode = "character") | is.factor(x = product))){stop("'product' must be a character vector or a factor.")}
   if(length(product) <= 0L){stop("'product' must be of length greater than 0.")}
   
   # check input for "price":
   if(missing(price)){stop("'price' is missing.")}
   if(!is.vector(x = price, mode = "numeric")){stop("'price' must be numeric.")}
   if(length(price) <= 0L){stop("'price' must be of length greater than 0.")}
   
   # check input for "weights":
   if(missing(weights)){stop("'weights' is missing.")}
   if(!is.vector(x = weights, mode = "numeric")){stop("'weights' must be numeric.")}
   if(length(weights) <= 0L){stop("'weights' must be of length greater than 0.")}
   
   # check that input vectors are of same length:
   if(length(region) != length(product)){stop("Input vectors are of different lengths.")}
   if(length(region) != length(price)){stop("Input vectors are of different lengths.")}
   if(length(region) != length(weights)){stop("Input vectors are of different lengths.")}
   
   # check input for "base":
   if(missing(base)){base <- NULL} # set to default value
   if(!is.null(base)){
      
      if(!is.vector(x = base, mode = "character")){stop("'base' must be a character")}
      if(length(base) != 1L){stop("'base' must be a single character.")}
      if(is.na(base)){stop("'base' is not allowed to be NA.")}
      
   }
   
   # coerce to character:
   region <- as.character(region)
   product <- as.character(product)
   
   # transform data into price matrix:
   price_mat <- tapply(X = price, INDEX = list(product, region), FUN = mean)
   # note that X should not have names for better performance in terms of speed
   
   # get column names:
   price_mat_cols <- colnames(price_mat)
   
   # set base region:
   if(is.null(base)){base <- 1L}else{
      
      if(base %in% price_mat_cols){base <- which(x = base == price_mat_cols)}else{base <- 1L}
      
   }
   
   # transform weights into matrix:
   weights_mat <- tapply(X = weights, INDEX = list(product, region), FUN = mean)
   
   # apply index to price matrix:
   .toernq(prices = price_mat, weights = weights_mat, base = base)
   
}
.toernq <- function(prices, weights, base = 1L){
   
   # take logarithm:
   price_mat <- log(prices)
   
   # compute logarithmic differences:
   log_diff <- price_mat - price_mat[, base]
   
   # compute weighting matrix:
   weights_mat <- 0.5*(weights + weights[, base])
   
   # set weights to NA when no intersection of prices:
   weights_mat[is.na(log_diff)] <- NA
   
   # re-normalize weights:
   weights_mat <- weights_mat/colSums(x = weights_mat, na.rm = TRUE)[col(weights_mat)]
   
   # compute weighted logarithmic difference, i.e. logarithmic Toernqvist index:
   out <- colSums(x = weights_mat*log_diff, na.rm = TRUE)
   
   # set values to NA:
   out_na <- ifelse(test = colSums(x = !is.na(weights_mat*log_diff), na.rm = FALSE) <= 0, NA, 1)
   # -> "colSums"-function returns 0 when everything is NA, but we need NA
   
   # adjust output:
   out <- out*out_na
   
   # set to NA when no intersecting prices were found:
   out[is.nan(out)] <- NA
   
   # re-transform logs:
   exp(out)
   
}
carli <- function(region, product, price, base = NULL){
   
   # check input for "region":
   if(missing(region)){stop("'region' is missing.")}
   if(!(is.vector(x = region, mode = "character") | is.factor(x = region))){stop("'region' must be a character vector or a factor.")}
   if(length(region) <= 0L){stop("'region' must be of length greater than 0.")}
   
   # check input for "product":
   if(missing(product)){stop("'product' is missing.")}
   if(!(is.vector(x = product, mode = "character") | is.factor(x = product))){stop("'product' must be a character vector or a factor.")}
   if(length(product) <= 0L){stop("'product' must be of length greater than 0.")}
   
   # check input for "price":
   if(missing(price)){stop("'price' is missing.")}
   if(!is.vector(x = price, mode = "numeric")){stop("'price' must be numeric.")}
   if(length(price) <= 0L){stop("'price' must be of length greater than 0.")}
   
   # check that input vectors are of same length:
   if(length(region) != length(product)){stop("Input vector are of different lengths.")}
   if(length(region) != length(price)){stop("Input vector are of different lengths.")}
   
   # check input for "base":
   if(missing(base)){base <- NULL} # set to default value
   if(!is.null(base)){
      
      if(!is.vector(x = base, mode = "character")){stop("'base' must be a character")}
      if(length(base) != 1L){stop("'base' must be a single character.")}
      if(is.na(base)){stop("'base' is not allowed to be NA.")}
      
   }
   
   # coerce to character:
   region <- as.character(region)
   product <- as.character(product)
   
   # transform data into price matrix:
   price_mat <- tapply(X = price, INDEX = list(product, region), FUN = mean)
   # note that X should not have names for better performance in terms of speed
   
   # get column names:
   price_mat_cols <- colnames(price_mat)
   
   # set base region:
   if(is.null(base)){base <- 1L}else{
      
      if(base %in% price_mat_cols){base <- which(x = base == price_mat_cols)}else{base <- 1L}
      
   }
   
   # apply index to price matrix:
   .carli(prices = price_mat, weights = NULL, base = base)
   
}
.carli <- function(prices, weights = NULL, base = 1L){
   
   # compute Carli price index:
   out <- colMeans(x = prices/prices[, base], na.rm = TRUE)
   
   # set to NA when no intersecting prices were found:
   out[is.nan(out)] <- NA
   
   # print output to console:
   out
   
}

# FULL PRICE DATA ---------------------------------------------------------

# sample price data:
data <- spIndex::sample_prices(n_regions = 700, n_products = 1200)
data$weights <- runif(nrow(data),0.1,0.4)
data$weights <- data$weights/sum(data$weights)

### (1) Elementary indices

t0 <- proc.time()
data_all <- base::merge(x = data,
                        y = data[data$region==1L,],
                        by = "product",
                        all = FALSE)
out_new <- sapply(X = split(x = data_all, f = data_all$region.x), FUN = function(z) .carli(prices = cbind(z$price.y, z$price.x))[-1])
proc.time() - t0

t0 <- proc.time()
dt <- as.data.table(data)
setkeyv(x = dt, cols = c("region","product"))
# ensure that no duplicated entries are present:
if(anyDuplicated(x = dt, by = key(dt)) > 0){
   dt <- dt[, list("price" = mean(price)), by = c("region", "product")]
}
dt_all <- merge(x = dt,
                y = dt[region==1L,],
                by = "product",
                all = FALSE)
# note that this approach, using cbind() is only a wrapper
# to use the pre-defined function. A simple vectorized solution
# would be also possible and faster. However, for the package
# we also have index_pairs() which is faster on the basis
# of a price matrix:
dt_sub <- dt_all[, .carli(cbind(price.y, price.x))[-1], by = "region.x"]
# ensure that results contain all regions, also in cases
# where no product matches were found. This is important
# in cases of incomplete price data:
dt_sub <- merge(x = data.table("region.x" = levels(dt$region)),
                y = dt_sub,
                by = "region.x",
                all.x = TRUE)
out_new2 <- dt_sub$V1
names(out_new2) <- dt_sub$region.x
proc.time() - t0

t0 <- proc.time()
out_old <- with(data, carli(region, product, price))
proc.time() - t0

# identical results?
all.equal(out_old[match(names(out_new), names(out_old))], out_new)
all.equal(out_old[match(names(out_new2), names(out_old))], out_new2)

# (2) Weighted indices

t0 <- proc.time()
data_all <- base::merge(x = data,
                        y = data[data$region==1L,],
                        by = "product",
                        all = FALSE)
out_new <- sapply(X = split(x = data_all, f = data_all$region.x), FUN = function(z) .toernq(prices = cbind(z$price.y, z$price.x), weights = cbind(z$weights.y, z$weights.x))[-1])
proc.time() - t0

t0 <- proc.time()
dt <- as.data.table(data)
setkeyv(x = dt, cols = c("region","product"))
# ensure that no duplicated entries are present:
if(anyDuplicated(x = dt, by = key(dt)) > 0){
   dt <- dt[, list("price" = mean(price),
                   "weights" = mean(weights)),
            by = c("region", "product")]
}
dt_all <- merge(x = dt,
                y = dt[region==1L,],
                by = "product",
                all = FALSE)
dt_sub <- dt_all[, .toernq(prices = cbind(price.y, price.x), weights = cbind(weights.y, weights.x))[-1], by = "region.x"]
# ensure that results contain all regions, also in cases
# where no product matches were found. This is important
# in cases of incomplete price data.
dt_sub <- merge(x = data.table("region.x" = levels(dt$region)),
                y = dt_sub,
                by = "region.x",
                all.x = TRUE)
out_new2 <- dt_sub$V1
names(out_new2) <- dt_sub$region.x
proc.time() - t0

t0 <- proc.time()
out_old <- with(data, toernq(region, product, price, weights))
proc.time() - t0

# identical results?
all.equal(out_old[match(names(out_new), names(out_old))], out_new)
all.equal(out_old[match(names(out_new2), names(out_old))], out_new2)


# INCOMPLETE PRICE DATA ---------------------------------------------------

# introduce random gaps:
idx <- spIndex::sample_gaps(data$region, data$product, missings = list("avg" = 0.4))
data_gaps <- data[!idx,]

### (1) Elementary indices

# base merge:
t0 <- proc.time()
data_gaps_all <- base::merge(x = data_gaps,
                             y = data_gaps[data_gaps$region==1L,],
                             by = "product",
                             all = FALSE)
out_new <- sapply(X = split(x = data_gaps_all, f = data_gaps_all$region.x), FUN = function(z) .carli(prices = cbind(z$price.y, z$price.x))[-1])
proc.time() - t0

# data.table merge:
t0 <- proc.time()
dt <- as.data.table(data_gaps)
setkeyv(x = dt, cols = c("region","product"))
# ensure that no duplicated entries are present:
if(anyDuplicated(x = dt, by = key(dt)) > 0){
   dt <- dt[, list("price" = mean(price)), by = c("region", "product")]
}
dt_all <- merge(x = dt,
                y = dt[region==1L,],
                by = "product",
                all = FALSE)
dt_sub <- dt_all[, .carli(prices = cbind(price.y, price.x))[-1], by = "region.x"]
# ensure that results contain all regions, also in cases
# where no product matches were found. This is important
# in cases of incomplete price data.
dt_sub <- merge(x = data.table("region.x" = levels(dt$region)),
                y = dt_sub,
                by = "region.x",
                all.x = TRUE)
out_new2 <- dt_sub$V1
names(out_new2) <- dt_sub$region.x
proc.time() - t0

# matrix calculations:
t0 <- proc.time()
out_old <- with(data_gaps, carli(region, product, price))
proc.time() - t0

# identical results?
all.equal(out_old[match(names(out_new), names(out_old))], out_new)
all.equal(out_old[match(names(out_new2), names(out_old))], out_new2)

# (2) Weighted indices

t0 <- proc.time()
data_gaps_all <- base::merge(x = data_gaps,
                             y = data_gaps[data_gaps$region==1L,],
                             by = "product",
                             all = FALSE)
out_new <- sapply(X = split(x = data_gaps_all, f = data_gaps_all$region.x), FUN = function(z) .toernq(prices = cbind(z$price.y, z$price.x), weights = cbind(z$weights.y, z$weights.x))[-1])
proc.time() - t0

t0 <- proc.time()
dt <- as.data.table(data_gaps)
setkeyv(x = dt, cols = c("region","product"))
# ensure that no duplicated entries are present:
if(anyDuplicated(x = dt, by = key(dt)) > 0){
   dt <- dt[, list("price" = mean(price),
                   "weights" = mean(weights)),
            by = c("region", "product")]
}
dt_all <- merge(x = dt,
                y = dt[region==1L,],
                by = "product",
                all = FALSE)
dt_sub <- dt_all[, .toernq(prices = cbind(price.y, price.x), weights = cbind(weights.y, weights.x))[-1], by = "region.x"]
# ensure that results contain all regions, also in cases
# where no product matches were found. This is important
# in cases of incomplete price data.
dt_sub <- merge(x = data.table("region.x" = levels(dt$region)),
                y = dt_sub,
                by = "region.x",
                all.x = TRUE)
out_new2 <- dt_sub$V1
names(out_new2) <- dt_sub$region.x
proc.time() - t0

t0 <- proc.time()
out_old <- with(data_gaps, toernq(region, product, price, weights))
proc.time() - t0

# identical results?
all.equal(out_old[match(names(out_new), names(out_old))], out_new)
all.equal(out_old[match(names(out_new2), names(out_old))], out_new2)

# END
