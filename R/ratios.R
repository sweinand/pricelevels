# START

# Title:  Groupwise calculation of price ratios
# Author: Sebastian Weinand
# Date:   18 May 2024

ratios <- function(p, r, n, base=NULL, static=FALSE, settings=list()){

  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("pricelevels.chatty")

  # non-exported settings:
  if(is.null(settings$check.inputs)) settings$check.inputs <- TRUE

  # input checks:
  if(settings$check.inputs){

    # main inputs:
    check.num(x=p, int=c(0, Inf))
    check.char(x=r)
    check.char(x=n)
    check.char(x=base, min.len=1, max.len=1, miss.ok=TRUE, null.ok=TRUE, na.ok=FALSE)
    check.log(x=static, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
    check.lengths(x=r, y=n)
    check.lengths(x=r, y=p)

    # settings:
    check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)

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

  # gather data into data.table:
  dt <- data.table("r"=r, "n"=n, "p"=p)

  if(is.null(base)){

    # compute price ratios with respect to product-specific average:
    dt[, "ratio" := p/mean(p, na.rm=TRUE), by="n"]
    res <- dt$ratio

  }else{

    # add row identifier for ordering and flagging of duplicates:
    dt[, "rid" := 1:.N]

    # set base region for each product:
    if(static){
      dt[, "is_base" := r==base, by="n"]
    }else{
      dt[, "is_base" := set.flexible.base(p=p, r=r, base=base), by="n"]
    }

    # subset to unique observations of base region:
    dt_base <- unique(x=dt[dt$is_base==TRUE, ], by=c("r", "n"))

    # add base observations to initial data:
    out <- merge(x=dt, y=dt_base, by="n", all.x=TRUE)

    # any changes in base:
    check.base <- any(out$r.y!=base, na.rm=TRUE)
    if(settings$chatty && check.base){
      warning("Base region 'base' adjusted for some products -> see 'attr('base')'", call.=FALSE)
    }

    # fill base:
    if(static) out$r.y[is.na(out$r.y)] <- base

    # apply function:
    out$ratio <- out$p.x/out$p.y

    # detect exact base price also when there are duplicates:
    out$is_base <- out$rid.x==out$rid.y

    # preserve initial ordering:
    setorderv(x=out, cols="rid.x")

    # coerce into named vector:
    res <- out$ratio

  }

  # add info about base prices used:
  if(!is.null(base) && any(check.base)){
    attr(res, "base") <- stats::setNames(out$is_base, out$r.y)
  }

  # print output to console:
  return(res)

}

# END
