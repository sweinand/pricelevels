# START

# Title:  Groupwise calculation of price ratios
# Author: Sebastian Weinand
# Date:   11 September 2023

# main function to perform groupwise calculations:
.apply_per_group <- function(p, r, n, base=NULL, static=FALSE, drop=FALSE, FUN=function(x, x0){x/x0}){

  # select base region:
  .base <- function(p, r, base){
    na.p <- !is.na(p)
    if(base%in%r && any(!is.na(p[r==base]))){
      res <- r==base & na.p
    }else{
      res <- na.p & !duplicated(na.p)
    }
    return(res)
  }

  # define environment:
  env <- environment()

  # set base if not provided:
  if(is.null(base)) base <- names(which.max(table(r)))[1]

  # gather data into data.table:
  dt <- data.table("region"=r, "product"=n, "price"=p)

  # add row identifier for ordering and flagging of duplicates:
  dt[, "rid" := 1:.N]

  # set base region for each product:
  if(static){
    dt[, "is_base" := region==base, by="product"]
  }else{
    dt[, "is_base" := .base(p=price, r=region, base=base), by="product"]
  }

  # subset to unique observations of base region:
  dt_base <- unique(x=dt[is_base==TRUE, ], by=c("region", "product"))

  # set names:
  setnames(x=dt_base, old="region", new="base")

  # add base observations to intial data:
  out <- merge(x=dt, y=dt_base, by="product", all.x=TRUE)

  # fill base:
  if(static) out[is.na(base), "base" := get(x="base", envir=env)]

  # apply function:
  out[, "ratio" := FUN(x=price.x, x0=price.y)]

  # detect extact base price also when there are duplicates:
  out[, "is_base" := rid.x==rid.y]

  # preserve inital ordering:
  setorderv(x=out, cols="rid.x")

  # drop base observations:
  if(drop) out <- out[is_base != TRUE, ]

  # coerce into named vector:
  out <- setNames(out$ratio, out$base)

  # print output to console:
  return(out)

}
# this function split is made to use .apply_per_group()
# also internally for function cd(), e.g. for computing
# value shares as weights per group

# wrapper function for calculating price ratios per group:
ratios <- function(p, r, n, base=NULL, static=FALSE, drop=FALSE){

  # input checks:
  .check.num(x=p, int=c(0, Inf))
  .check.char(x=r)
  .check.char(x=n)
  .check.char(x=base, min.len=1, max.len=1, miss.ok=TRUE, null.ok=TRUE, na.ok=FALSE)
  .check.log(x=static, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
  .check.log(x=drop, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
  .check.lengths(x=r, y=n)
  .check.lengths(x=r, y=p)

  # compute price ratios:
  .apply_per_group(r = r,
                   n = n,
                   p = p,
                   base = base,
                   static = static,
                   drop = drop,
                   FUN = function(x, x0){x/x0})

}

# END
