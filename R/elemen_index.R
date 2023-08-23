# START

# Title:  Elementary price indices
# Author: Sebastian Weinand
# Date:   2023-08-23

# helper functions:
.jevons <- function(prices, weights = NULL, base = 1L){

  # take logarithm:
  price_mat <- log(prices)

  # compute logarithmic difference, i.e. logarithmic Jevons index:
  out <- colMeans(x = price_mat - price_mat[, base], na.rm = TRUE)

  # set to NA when no intersecting prices were found:
  out[is.nan(out)] <- NA

  # re-transform logs:
  exp(out)

}
.dutot <- function(prices, weights = NULL, base = 1L){

  # NA matrix to avoid that means are based on different or non-intersecting sets:
  na_mat <- ifelse(is.na(prices[, base]), NA, 1)

  # compute Dutot price index:
  out <- colMeans(x = na_mat*prices, na.rm = TRUE)/mean(x = prices[, base], na.rm = TRUE)

  # set to NA when no intersecting prices were found:
  out[is.nan(out)] <- NA

  # print output to console:
  out

}
.carli <- function(prices, weights = NULL, base = 1L){

  # compute Carli price index:
  out <- colMeans(x = prices/prices[, base], na.rm = TRUE)

  # set to NA when no intersecting prices were found:
  out[is.nan(out)] <- NA

  # print output to console:
  out

}
.harmonic <- function(prices, weights = NULL, base = 1L){

  # compute harmonic mean index:
  out <- 1/colMeans(x = 1/(prices/prices[, base]), na.rm = TRUE)

  # set to NA when no intersecting prices were found:
  out[is.nan(out)] <- NA

  # print output to console:
  out

}

# helper function for input checking and calculations:
.elem.index <- function(p, r, n, type, base = NULL){

  # input checks:
  .check.num(x=p, int=c(0, Inf))
  .check.char(x=r)
  .check.char(x=n)
  .check.char(x=base, min.len=1, max.len=1, miss.ok=TRUE, null.ok=TRUE, na.ok=FALSE)
  .check.char(x=type, min.len=1, max.len=1, na.ok=FALSE)
  .check.lengths(x=r, y=n)
  .check.lengths(x=r, y=p)

  # set index function based on type:
  type <- match.arg(arg = type, choices = c("jevons", "carli", "dutot", "harmonic"))
  if(type == "jevons"){index_func <- .jevons}
  if(type == "carli"){index_func <- .carli}
  if(type == "dutot"){index_func <- .dutot}
  if(type == "harmonic"){index_func <- .harmonic}

  # set default base if necessary:
  if(is.null(base)){base <- names(which.max(table(r)))[1]} # when base is NULL
  if(!(base%in%r)){ # when base is no valid region
    base <- names(which.max(table(r)))[1]
    warning(paste("Base region not found and reset to", base))
  }

  # gather in data.table:
  price_data <- data.table("region" = factor(r),
                           "product" = factor(n),
                           "price" = as.numeric(p))

  # store initial ordering of region levels:
  region.lev <- levels(price_data$region)

  # set key:
  setkeyv(x = price_data, cols = c("region", "product"))

  # check for duplicated entries:
  if(anyDuplicated(x = price_data, by = key(price_data)) > 0){

    # average duplicated prices and weights:
    price_data <- price_data[, list("price" = mean(price)), by = c("region", "product")]

  }

  # add base region prices and weights:
  price_data <- merge.data.table(x = price_data,
                                 y = price_data[region == base,],
                                 by = "product",
                                 all = FALSE)

  # compute price index for each region:
  index_data <- price_data[, index_func(prices = cbind(price.y, price.x),
                                        weights = NULL,
                                        base = 1L)[-1],
                           by = "region.x"]

  # ensure that results contain all regions, also in cases
  # where no product matches were found. This is important
  # in cases of incomplete price data:
  index_data <- merge.data.table(x = data.table("region.x" = levels(price_data$region.x)),
                                 y = index_data,
                                 by = "region.x",
                                 all.x = TRUE)

  # coerce to vector:
  out <- setNames(index_data$V1, index_data$region.x)

  # match to initial ordering and unlog:
  out <- out[match(x=region.lev, table=names(out))]

  # print output to console:
  return(out)

}

# package functions:
jevons <- function(p, r, n, base = NULL){

  .elem.index(r = r, n = n, p = p, type = "jevons", base = base)

}
dutot <- function(p, r, n, base = NULL){

  .elem.index(r = r, n = n, p = p, type = "dutot", base = base)

}
carli <- function(p, r, n, base = NULL){

  .elem.index(r = r, n = n, p = p, type = "carli", base = base)

}
harmonic <- function(p, r, n, base = NULL){

  .elem.index(r = r, n = n, p = p, type = "harmonic", base = base)

}

# END
