# START

# Title:  GEKS index
# Author: Sebastian Weinand
# Date:   2023-08_23

# compute mutlilateral GEKS index:
geks <- function(p, r, n, w = NULL, type = "jevons", base = NULL){

  # input checks:
  .check.char(x=base, min.len=1, max.len=1, miss.ok=TRUE, null.ok=TRUE, na.ok=FALSE)
  # -> input checks of other arguments are performed in index_pairs()

  # compute bilateral price index numbers:
  price_mat <- index.pairs(r = r,
                           n = n,
                           p = p,
                           w = w,
                           type = type,
                           all.pairs = TRUE,
                           as.dt = FALSE)

  # define position of base region in matrix:
  if(is.null(base) | missing(base)){
    idx <- 1L
  }else{
    if(base%in%colnames(price_mat)){
      idx <- which(base == colnames(price_mat))
    }else{
      idx <- 1L
      warning(paste("Base region not found and reset to", colnames(price_mat)[idx]))
    }
  }

  # compute multilateral GEKS index numbers:
  out <- colMeans(x = log(price_mat*price_mat[idx,]), na.rm = TRUE)

  # scale if necessary:
  if(is.null(base)){out <- scale(x = out, center = TRUE, scale = FALSE)[,1]}

  # unlog price levels:
  exp(out)

}
# -> IDEA:
#    o GEKS: two-step procedure. Therefore, estimated standard errors (SE) are
#      zero in the second (regression) step when prices are fully available
#    o CPD: one step procedure. Therefore, SE are always greater than zero.
#    -> split the SE of CPD in two components, similar to GEKS: (i) SE-component
#       for the aggregation of prices into index numbers, and (ii) SE-component
#       for uncertainty due to gaps in the price data

# END
