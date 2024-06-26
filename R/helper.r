# START

# Title:  General helper functions
# Author: Sebastian Weinand
# Date:   18 May 2024

# set base region:
set.base <- function(r, base, null.ok, qbase=FALSE, settings=list()){

  # @ allowed settings:
  # - chatty

  # @args:
  # r         factor, vector of regions
  # base      character, one base region
  # null.ok   logical, base=NULL allowed or not
  # qbase     logical, qbase=TRUE if base region quantities relevant
  # chatty    logical, print warning or not

  # derive base region if required and base=NULL:
  if(is.null(base) && !null.ok){
    base <- names(which.max(table(r)))[1]
    if(settings$chatty){
      if(!qbase){
        message(paste0("Base region set to base='", base, "'"))
      }
    }
  }

  # derive base region if not found in regions and base not NULL:
  if(!base%in%levels(r) && !is.null(base)){
    base <- names(which.max(table(r)))[1]
    if(settings$chatty){
      if(qbase){
        warn.msg <- paste0("settings$qbase='", settings$qbase, "' not found -> reset to '", base, "'")
      }else{
        warn.msg <- paste0("Base region not found -> reset to base='", base, "'")
      }
      warning(warn.msg, call.=FALSE)
    }
  }

  # return output:
  return(base)

}

# initialize data:
arrange <- function(p, r, n, q=NULL, w=NULL, base, settings=list()){

  # @ allowed settings:
  # - missings : check and remove NAs
  # - connect : check and remove non-connected regions
  # - duplicates : check and aggregate duplicated values
  # - chatty : print warnings
  # - norm.weights : normalize given weights or not

  # @output:
  # if 'q' and/or 'w' is provided, the output data will contain 'q=q'
  # and expenditure share weights that may differ to 'w' (if provided)
  # if only 'w' is provided, the output data will contain 'q=NA' and
  # 'w=w' if settings$norm.weights=FALSE and 'w=w/sum(w)' if
  # settings$norm.weights=TRUE
  # if neither 'q' nor 'w' is provided, the output data will
  # contain 'w=NA' and 'z=NA'

  # set quantities or weights if available:
  if(is.null(q) && is.null(w)){
    z <- rep(1, length(p))
  }else{
    if(is.null(q)) z <- w else z <- q
  }

  # gather in data.table:
  dt <- data.table("r"=as.character(r), "n"=as.character(n), "p"=as.numeric(p), "z"=as.numeric(z))

  # check and remove NAs:
  if(settings$missings){

    # if both q and w are provided, q will be checked:
    nas <- dt[, stats::complete.cases(r, n, p, z)]
    dt <- dt[nas,]
    if(settings$chatty & sum(!nas)>0L){
      warning(paste(sum(!nas), "incomplete case(s) found and removed"), call.=FALSE)
    }

    # stop if no observations left:
    if(nrow(dt)<=0L){
      stop("No complete cases available -> all data pairs contain at least one NA", call.=FALSE)
    }

  }

  # subset to connected data:
  if(settings$connect){

    if(dt[, !is.connected(r=r, n=n)]){

      # subset based on input:
      if(!base%in%levels(factor(dt$r)) || is.null(base)){
        dt <- dt[connect(r=r, n=n), ]
      }else{
        dt[, "ng" := neighbors(r=r, n=n, simplify=TRUE)]
        ngbs <- unique(dt$ng[dt$r%in%base])
        dt <- dt[dt$ng%in%ngbs,]
      }

      # warning message:
      if(settings$chatty){
        warning("Non-connected regions -> computations with subset of data", call.=FALSE)
      }

    }

  }

  # check for duplicated entries:
  if(settings$duplicates && anyDuplicated(x=dt, by=c("r","n"))>0L){

    # average duplicated prices and weights, sum duplicated quantities:
    if(is.null(q)){
      dt <- dt[, list("p"=stats::weighted.mean(x=p, w=ifelse(is.na(z), 0, z), na.rm=TRUE), "z"=mean(z, na.rm=TRUE)), by=c("r","n")]
    }else{
      dt <- dt[, list("p"=stats::weighted.mean(x=p, w=ifelse(is.na(z), 0, z), na.rm=TRUE), "z"=sum(z, na.rm=TRUE)), by=c("r","n")]
    }

    # print warning:
    if(settings$chatty){
      warning("Duplicated observations found and aggregated", call.=FALSE)
    }

  }

  # derive weights:
  if(!is.null(q)){
    # expenditure share weights for each region:
    dt[, "w" := (p*z)/sum(p*z, na.rm=TRUE), by="r"]
    setnames(x=dt, old="z", new="q")
  }else{
    if(is.null(w)){
      dt[, c("q","w") := NA_real_]
    }else{
      dt[, "q" := NA_real_]
      if(settings$norm.weights){
        # normalize given weights:
        dt[, "w" := z/sum(z, na.rm=TRUE), by="r"]
      }else{
        # else go with provided input:
        dt[, "w" := z]
      }
    }
    # drop working variable again:
    dt[, "z" := NULL]
  }

  # coerce regions and products to factor:
  dt[, c("r","n") := list(factor(r), factor(n))]
  # do not use "as.factor()" because this does not drop unused factor levels

  # return data:
  return(dt)

}

# plot price ratios and price levels:
plot.pricelevels <- function(data, P=numeric()){

  # @Args:
  # data    a data.frame with columns region and ratio
  # P       a vector or matrix with price levels

  # create empty boxplot:
  graphics::boxplot(ratio~region, data=data,
                    border=NA,
                    axes=FALSE,
                    xlab="Region",
                    ylab="Price levels and ratios",
                    main="Price ratios and price levels of the regions")

  # add horizontal lines:
  y.ticks <- graphics::par("yaxp")
  graphics::abline(h=seq(from=y.ticks[1], to=y.ticks[2], length=y.ticks[3]+1),
                   col="lightgrey", lty=2)

  # add horizontal line at equality:
  graphics::abline(h=1, col="lightgrey", lty=2)

  # add vertical lines:
  x.ticks <- graphics::par("xaxp")
  graphics::abline(v=seq(from=x.ticks[1], to=x.ticks[2], length=x.ticks[3]+1),
                   col="lightgrey", lty=2)

  # add boxplot:
  graphics::boxplot(ratio~region, data=data, add=TRUE)

  # add price indices:
  if(is.matrix(P)){
    if(nrow(P)>1) mycols <- 1:nrow(P) else mycols <- "red"
    graphics::matpoints(t(P), pch=20, cex=1.5, col=mycols)
  }else{
    graphics::matpoints(P, pch=20, cex=1.5, col="red")
  }

}

# END
