# START

# Title:  Weighted price indices
# Author: Sebastian Weinand
# Date:   2023-08-23

# helper functions:
.walsh <- function(prices, weights, base = 1L){

  # NA matrix to avoid that means are based on different or non-intersecting sets:
  na_mat <- ifelse(is.na(prices[, base]), NA, 1)

  # compute logarithmic differences:
  price_mat <- prices*na_mat

  # compute weighting matrix:
  weights_mat <- sqrt(weights*weights[, base])

  # set weights to NA when no intersection of prices:
  weights_mat[is.na(price_mat)] <- NA

  # re-normalize weights:
  weights_mat <- weights_mat/colSums(x = weights_mat, na.rm = TRUE)[col(weights_mat)]

  # compute weighted logarithmic difference, i.e. logarithmic Toernqvist index:
  out <- colSums(x = weights_mat*sqrt(price_mat/price_mat[, base]), na.rm = TRUE)/colSums(x = weights_mat*sqrt(price_mat[, base]/price_mat), na.rm = TRUE)

  # set values to NA:
  out_na <- ifelse(test = colSums(x = !is.na(weights_mat*price_mat), na.rm = FALSE) <= 0, NA, 1)
  # -> "colSums"-function returns 0 when everything is NA

  # adjust output:
  out <- out*out_na

  # set to NA when no intersecting prices were found:
  out[is.nan(out)] <- NA

  # print output to console:
  out

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
.laspey <- function(prices, weights, base = 1L){

  # compute price ratios:
  ratios <- prices / prices[, base]

  # compute weighting matrix:
  weights_mat <- weights[, base] * matrix(data = 1, nrow = nrow(weights), ncol = ncol(weights), dimnames = dimnames(weights))

  # set weights to NA when no intersection of prices:
  weights_mat[is.na(ratios)] <- NA

  # re-normalize weights:
  weights_mat <- weights_mat/colSums(x = weights_mat, na.rm = TRUE)[col(weights_mat)]

  # compute weighted logarithmic difference, i.e. logarithmic Toernqvist index:
  out <- colSums(x = weights_mat*ratios, na.rm = TRUE)

  # set values to NA:
  out_na <- ifelse(test = colSums(x = !is.na(weights_mat*ratios), na.rm = FALSE) <= 0, NA, 1)
  # -> "colSums"-function returns 0 when everything is NA

  # adjust output:
  out <- out*out_na

  # set to NA when no intersecting prices were found:
  out[is.nan(out)] <- NA

  # print output to console:
  out

}
.paasche <- function(prices, weights, base = 1L){

  # compute price ratios:
  ratios <- 1 / (prices / prices[, base])

  # compute weighting matrix:
  weights_mat <- weights[, base] * matrix(data = 1, nrow = nrow(weights), ncol = ncol(weights), dimnames = dimnames(weights))

  # set weights to NA when no intersection of prices:
  weights_mat[is.na(ratios)] <- NA

  # re-normalize weights:
  weights_mat <- weights_mat/colSums(x = weights_mat, na.rm = TRUE)[col(weights_mat)]

  # compute weighted logarithmic difference, i.e. logarithmic Toernqvist index:
  out <- 1/colSums(x = weights_mat*ratios, na.rm = TRUE)

  # set values to NA:
  out_na <- ifelse(test = colSums(x = !is.na(weights_mat*ratios), na.rm = FALSE) <= 0, NA, 1)
  # -> "colSums"-function returns 0 when everything is NA

  # adjust output:
  out <- out*out_na

  # set to NA when no intersecting prices were found:
  out[is.nan(out)] <- NA

  # print output to console:
  out

}
.fisher <- function(prices, weights, base = 1L){

  # compute Laspeyres indices:
  l <- .laspey(prices = prices, weights = weights, base = base)

  # compute Laspeyres indices:
  p <- .paasche(prices = prices, weights = weights, base = base)

  # compute Fisher:
  sqrt(x = l*p)

}
# -> note that Paasche and Laspeyres index still differ when
#    prices and weights are identical. Paasche and Laspeyres
#    would be identical when the quantities underlying these
#    weights are identical. Otherwise, the Paasche is kind of
#    a weighted harmonic and the Laspeyres a weighted Carli index.

# helper function for input checking and calculations:
.weighted.index <- function(p, r, n, w, type, base = NULL){

  # input checks:
  .check.num(x=p, int=c(0, Inf))
  .check.char(x=r)
  .check.char(x=n)
  .check.num(x=w, null.ok=TRUE, int=c(0, Inf))
  .check.char(x=base, min.len=1, max.len=1, miss.ok=TRUE, null.ok=TRUE, na.ok=FALSE)
  .check.char(x=type, min.len=1, max.len=1, na.ok=FALSE)
  .check.lengths(x=r, y=n)
  .check.lengths(x=r, y=p)
  .check.lengths(x=r, y=w)

  # set index function based on type:
  type <- match.arg(arg = type, choices = c("toernq", "laspey", "paasche", "walsh", "fisher"))
  if(type == "toernq"){index_func <- .toernq}
  if(type == "laspey"){index_func <- .laspey}
  if(type == "paasche"){index_func <- .paasche}
  if(type == "walsh"){index_func <- .walsh}
  if(type == "fisher"){index_func <- .fisher}

  # set default base if necessary:
  if(is.null(base)){base <- names(which.max(table(r)))[1]} # when base is NULL
  if(!(base%in%r)){ # when base is no valid region
    base <- names(which.max(table(r)))[1]
    warning(paste("Base region not found and reset to", base))
  }

  # gather in data.table:
  price_data <- data.table("region" = factor(r),
                           "product" = factor(n),
                           "price" = as.numeric(p),
                           "weights" = as.numeric(w))

  # store initial ordering of region levels:
  region.lev <- levels(price_data$region)

  # set key:
  setkeyv(x = price_data, cols = c("region", "product"))

  # check for duplicated entries:
  if(anyDuplicated(x = price_data, by = key(price_data)) > 0){

    # average duplicated prices and weights:
    price_data <- price_data[, list("price" = mean(price),
                                    "weights" = mean(weights)),
                             by = c("region", "product")]

  }

  # add base region prices and weights:
  price_data <- merge.data.table(x = price_data,
                                 y = price_data[region == base,],
                                 by = "product",
                                 all = FALSE)

  # compute price index for each region:
  index_data <- price_data[, index_func(prices = cbind(price.y, price.x),
                                        weights = cbind(weights.y, weights.x),
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
walsh <- function(p, r, n, w, base = NULL){

  .weighted.index(r = r,
                  n = n,
                  p = p,
                  w = w,
                  type = "walsh",
                  base = base)

}
toernq <- function(p, r, n, w, base = NULL){

  .weighted.index(r = r,
                  n = n,
                  p = p,
                  w = w,
                  type = "toernq",
                  base = base)

}
laspey <- function(p, r, n, w, base = NULL){

  .weighted.index(r = r,
                  n = n,
                  p = p,
                  w = w,
                  type = "laspey",
                  base = base)

}
paasche <- function(p, r, n, w, base = NULL){

  .weighted.index(r = r,
                  n = n,
                  p = p,
                  w = w,
                  type = "paasche",
                  base = base)

}
fisher <- function(p, r, n, w, base = NULL){

  .weighted.index(r = r,
                  n = n,
                  p = p,
                  w = w,
                  type = "fisher",
                  base = base)

}

# END
