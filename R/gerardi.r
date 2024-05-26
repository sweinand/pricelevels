# START

# Title:    Gerardi index
# Author:   Sebastian Weinand
# Date:     18 May 2024

gerardi <- function(p, r, n, q, w=NULL, base=NULL, simplify=TRUE, settings=list()){

  # set default if missing:
  if(missing(q)) q <- NULL
  if(missing(w)) w <- NULL

  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("pricelevels.chatty")
  if(is.null(settings$connect)) settings$connect <- getOption("pricelevels.connect")
  if(is.null(settings$plot)) settings$plot <- getOption("pricelevels.plot")
  if(is.null(settings$variant)) settings$variant <- "original"

  # non-exported settings:
  if(is.null(settings$check.inputs)) settings$check.inputs <- getOption("pricelevels.check.inputs")
  if(is.null(settings$missings)) settings$missings <- getOption("pricelevels.missings")
  if(is.null(settings$duplicates)) settings$duplicates <- getOption("pricelevels.duplicates")
  if(is.null(settings$norm.weights)) settings$norm.weights <- TRUE

  # input checks:
  if(settings$check.inputs){

    # main inputs:
    check.num(x=p, int=c(0, Inf))
    check.char(x=r)
    check.char(x=n)
    check.num(x=w, null.ok=TRUE, int=c(0, Inf))
    check.num(x=q, null.ok=TRUE, int=c(0, Inf))
    check.char(x=base, miss.ok=TRUE, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE)
    check.log(x=simplify, miss.ok=TRUE, min.len=1, max.len=1, na.ok=FALSE)
    check.lengths(x=r, y=n)
    check.lengths(x=r, y=p)
    check.lengths(x=r, y=q)
    check.lengths(x=r, y=w)

    # settings:
    check.log(x=settings$connect, min.len=1, max.len=1, na.ok=FALSE)
    check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
    check.char(x=settings$variant, min.len=1, max.len=1, na.ok=FALSE)

  }

  # check against allowed variants:
  variant <- match.arg(arg=settings$variant, choices=c("original","adjusted"))

  # error handling for quantity and weights:
  if(settings$check.inputs){

    if(is.null(q) && is.null(w)){
      stop(paste0("Non-valid input -> 'q' or 'w' required but both missing"), call.=FALSE)
    }

  }

  # initialize data:
  pdata <- arrange(p=p, r=r, n=n, q=q, w=w, base=base, settings=settings)

  # set base region:
  base <- set.base(r=pdata$r, base=base, null.ok=TRUE, settings=settings)

  # compute international prices:
  if(variant=="original"){
    pdata[, "v":=exp(mean(log(p))), by="n"]
  }else{
    pdata[, "v":=exp(stats::weighted.mean(x=log(p), w=w)), by="n"]
  }

  # compute price levels:
  P <- pdata[, list("index"=sum(w)/sum(w*(v/p))), by="r"]
  P <- stats::setNames(P$index, P$r)

  # normalization:
  if(is.null(base)){
    P <- P/mean(P)
  }else{
    P <- P/P[names(P)==base]
  }

  if(simplify || settings$plot){

    # match to initial ordering:
    r.lvl <- levels(factor(r))
    res <- P[match(x=r.lvl, table=names(P))]
    names(res) <- r.lvl

  }

  if(settings$plot){

    # compute price ratios:
    pdata[, "ratio":=ratios(p=p, r=r, n=n, base=base, static=TRUE, settings=list(chatty=FALSE))]
    pdata[, "region":=factor(r, levels=r.lvl)]
    plot.pricelevels(data=pdata, P=res)

  }

  if(!simplify){

    # average product prices using normalized price levels:
    v <- unique(x=pdata, by="n")
    v <- stats::setNames(v$v, v$n)

    # define output:
    res <- c("v"=v, "P"=P)

  }

  # return output:
  return(res)

}

# END
