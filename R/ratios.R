# START

# Title:  Calculation of price ratios per product
# Author: Sebastian Weinand
# Date:   22 July 2025

ratios <- function(p, r, n, q=NULL, w=NULL, base=NULL, settings=list()){

  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("pricelevels.chatty")
  if(is.null(settings$static)) settings$static <- TRUE

  # non-exported settings:
  if(is.null(settings$check.inputs)) settings$check.inputs <- TRUE

  # input checks:
  if(settings$check.inputs){

    # main inputs:
    check.num(x=p, int=c(0.001, Inf))
    check.char(x=r)
    check.char(x=n)
    check.num(x=w, null.ok=TRUE, int=c(0, Inf))
    check.num(x=q, null.ok=TRUE, int=c(0, Inf))
    check.char(x=base, min.len=1, max.len=1, miss.ok=TRUE, null.ok=TRUE, na.ok=FALSE)
    check.lengths(x=r, y=n)
    check.lengths(x=r, y=p)
    check.lengths(x=r, y=q)
    check.lengths(x=r, y=w)

    # settings:
    check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
    check.log(x=settings$static, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)

  }

  # function for flexible setting of base region:
  set.flexible.base <- function(p, r, base){

    # frequency table of regions where prices are not NA:
    rtab <- table(r[!is.na(p)])

    # set new base region if not found in data:
    if(!base%in%names(rtab) && length(rtab)>0) base <- names(which.max(rtab))[1]

    # flag base region where no NA prices are present:
    res <- r==base & !is.na(p)

    # remove duplicates:
    res <- res & !duplicated(res)

    # flag first value if all prices NA:
    if(all(!res)) res[1] <- TRUE

    # return output:
    return(res)

  }

  # set base region:
  base <- set.base(r=factor(r), base=base, null.ok=TRUE, settings=settings)

  # gather data:
  dt <- data.table("r"=r, "n"=n, "p"=p, "q"=q, "w"=w, "pagg"=p)

  # address global bindings note when checking:
  pagg <- r.y <- NULL

  # average duplicated prices:
  if(anyDuplicated(x=dt, by=c("r","n"))>0L){

    if(is.null(q) & is.null(w)){
      dt[, "pagg" := mean(x=p, na.rm=TRUE), by=c("r","n")]
    }else{
      if(is.null(q)){
        dt[, "pagg" := stats::weighted.mean(x=p, w=ifelse(is.na(w), 0, w), na.rm=TRUE), by=c("r","n")]
      }else{
        dt[, "pagg" := stats::weighted.mean(x=p, w=ifelse(is.na(q), 0, q), na.rm=TRUE), by=c("r","n")]
      }
    }

    # print warning:
    if(settings$chatty){
      warning("Duplicated observations found and aggregated", call.=FALSE)
    }

  }

  if(is.null(base)){

    # compute regional average price per product:
    dtbase <- unique(x=dt, by=c("r","n"))
    dtbase <- dtbase[, list("pbase" = mean(pagg, na.rm=TRUE)), by="n"]
    dt <- merge(x=dt, y=dtbase, by="n", all.x=TRUE, sort=FALSE)

  }else{

    # set base region for each product:
    if(settings$static){
      dt[, "is_base" := r==base, by="n"]
    }else{
      dt[, "is_base" := set.flexible.base(p=pagg, r=r, base=base), by="n"]
    }

    # subset to unique observations of base region:
    dtbase <- unique(x=dt[is_base==TRUE, list(r,n,"pbase"=pagg)], by=c("r","n"))

    # add base observations to initial data:
    dt <- merge(x=dt, y=dtbase, by="n", all.x=TRUE, sort=FALSE)

    # any changes in base:
    if(settings$chatty && any(dt$r.y!=base, na.rm=TRUE)){
      warning("Base region 'base' adjusted for some products -> see 'attr(x, which='base')'", call.=FALSE)
    }

  }

  # compute price ratios:
  res <- as.vector(dt$p/dt$pbase)

  # add attribute about base region per product:
  if(!is.null(base) & !settings$static){
    rbases <- dt[is_base==TRUE, list("r"=unique(r.y)), by="n"]
    attr(x=res, "base") <- stats::setNames(as.character(rbases$r), rbases$n)
  }

  # print output to console:
  return(res)

}

# END
