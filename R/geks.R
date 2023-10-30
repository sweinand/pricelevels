# START

# Title:  Bilateral index pairs and GEKS method
# Author: Sebastian Weinand
# Date:   30 October 2023

# compute bilateral index pairs:
index.pairs <- function(p, r, n, q=NULL, w=NULL, settings=list()){

  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- TRUE
  if(is.null(settings$type)) settings$type <- "jevons"
  if(is.null(settings$all.pairs)) settings$all.pairs <- TRUE
  if(is.null(settings$as.dt)) settings$as.dt <- FALSE
  if(is.null(settings$connect)) settings$connect <- FALSE

  # the setting 'setting$base' is only used by geks() but not
  # exported/visible. it is needed to derive the block of
  # connected regions

  # input checks:
  .check.num(x=p, int=c(0, Inf))
  .check.char(x=r)
  .check.char(x=n)
  .check.num(x=q, miss.ok=TRUE, null.ok=TRUE, int=c(0, Inf))
  .check.num(x=w, miss.ok=TRUE, null.ok=TRUE, int=c(0, Inf))
  .check.char(x=settings$type, min.len=1, max.len=1, na.ok=FALSE)
  .check.log(x=settings$all.pairs, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
  .check.log(x=settings$as.dt, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
  .check.log(x=settings$connect, min.len=1, max.len=1, na.ok=FALSE)
  .check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
  .check.lengths(x=r, y=n)
  .check.lengths(x=r, y=p)
  .check.lengths(x=r, y=q)
  .check.lengths(x=r, y=w)

  # set quantities or weights if available:
  if(is.null(q) && is.null(w)){
    z <- rep(1, length(p))
  }else{
    if(is.null(q)) z <- w else z <- q
  }

  # allowed index types:
  type.vals <- c("jevons", "carli", "dutot", "harmonic", "toernq", "laspey", "paasche", "walsh", "fisher")

  # check against allowed index types:
  type <- match.arg(arg=settings$type, choices=type.vals)

  # set index function based on type:
  # NOTE that these are the matrix-version functions!
  if(type=="jevons"){index_func <- .jevons}
  if(type=="carli"){index_func <- .carli}
  if(type=="dutot"){index_func <- .dutot}
  if(type=="harmonic"){index_func <- .harmonic}
  if(type=="toernq"){index_func <- .toernq}
  if(type=="laspey"){index_func <- .laspey}
  if(type=="paasche"){index_func <- .paasche}
  if(type=="walsh"){index_func <- .walsh}
  if(type=="fisher"){index_func <- .fisher}

  # weights required for following index types:
  type.weights <- c("toernq", "laspey", "paasche", "walsh", "fisher")

  # error handling for quantity and weights:
  if(type%in%type.weights && is.null(q) && is.null(w)){
    stop(paste0("Non-valid input for type -> 'q' or 'w' required for type='", type, "'"))
  }

  # gather in data.table:
  pdata <- data.table("r"=as.character(r), "n"=as.character(n), "p"=as.numeric(p), "z"=as.numeric(z))

  # if both q and w are provided, q will be checked:
  pdata <- pdata[complete.cases(r, n, p, z), ]

  # stop if no observations left:
  if(nrow(pdata)<=0L){
    stop("No complete cases available -> all data pairs contain at least one NA", call.=FALSE)
  }

  # store initial ordering of region levels:
  r.lvl <- levels(factor(pdata$r))

  # subset to connected data:
  if(settings$connect){

    if(pdata[, !spin::is.connected(r=r, n=n)]){

      # subset based on input:
      if(!settings$base%in%r.lvl || is.null(settings$base)){
        pdata <- pdata[spin::connect(r=r, n=n), ]
      }else{
        pdata[, "ng" := spin::neighbors(r=r, n=n, simplify=TRUE)]
        pdata <- pdata[ng%in%pdata[r%in%settings$base, unique(ng)], ]
      }

      # warning message:
      if(settings$chatty){
        warning("Non-connected regions -> computations with subset of data", call.=FALSE)
      }

    }

  }

  # check for duplicated entries:
  if(anyDuplicated(x=pdata, by=c("r","n"))>0L){
    if(settings$chatty){
      warning("Duplicated observations found and aggregated", call.=FALSE)
    }
  }

  # convert prices into matrix:
  P <- as.matrix(
    x=dcast(data=pdata, formula=n~r, fun.aggregate=mean, value.var="p", fill=NA),
    rownames="n")

  # convert weights or quantities into matrix, where duplicated weights
  # are averaged, duplicated quantites added up:
  if(is.null(q) && is.null(w)){
    Z <- NULL
  }else{
    Z <- as.matrix(
      x=dcast(
        data=pdata,
        formula=n~r,
        fun.aggregate=function(j){if(is.null(q)) mean(j) else sum(j)},
        value.var="z",
        fill=NA),
      rownames="n")
  }

  # number of regions or time periods:
  R <- ncol(P)

  # container to store price levels:
  res <- matrix(data=NA_real_, nrow=R, ncol=R, dimnames=list(colnames(P), colnames(P)))

  # slow, but complete or quick, but only non-redundant:
  if(!settings$all.pairs){

    if(is.null(q)){
      for(i in 1:R){
        idx <- i:R # tighten column selection in each iteration
        res[i, idx] <- index_func(P=P[, idx, drop=FALSE], W=Z[, idx, drop=FALSE], base=1L)
      }
    }else{
      for(i in 1:R){
        idx <- i:R # tighten column selection in each iteration
        res[i, idx] <- index_func(P=P[, idx, drop=FALSE], Q=Z[, idx, drop=FALSE], base=1L)
      }
    }

  }else{

    if(is.null(q)){
      for(i in 1:R) res[i, ] <- index_func(P=P, W=Z, base=i)
    }else{
      for(i in 1:R) res[i, ] <- index_func(P=P, Q=Z, base=i)
    }

  }

  # convert to dataframe:
  if(settings$as.dt){

    # convert into dataframe:
    res <- as.data.table(as.table(t(res)))

    # drop missing values:
    res <- res[!is.na(N),]

    # set column names:
    setnames(x=res, c("region", "base", "index"))

    # set key:
    setkeyv(x=res, cols = c("region", "base"))

  }

  # print output to console:
  return(res)

}

# compute mutlilateral GEKS index:
geks <- function(p, r, n, q=NULL, w=NULL, base=NULL, simplify=TRUE, settings=list()){

  # define settings:
  if(is.null(settings$chatty)) settings$chatty <- TRUE
  if(is.null(settings$connect)) settings$connect <- TRUE
  if(is.null(settings$type)) settings$type <- "jevons"
  if(is.null(settings$method)) settings$method <- "none"
  if(is.null(settings$all.pairs)) settings$all.pairs <- TRUE

  # input checks:
  .check.log(x=simplify, miss.ok=TRUE, min.len=1, max.len=1, na.ok=FALSE)
  .check.char(x=base, min.len=1, max.len=1, miss.ok=TRUE, null.ok=TRUE, na.ok=FALSE)
  .check.char(x=settings$method, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE)
  # -> input checks of other arguments are performed in index.pairs()

  # match weighting method:
  method <- match.arg(arg=settings$method, choices=c("none","obs","shares"))

  # check if weighting method can be applied:
  if(is.null(q) && is.null(w) && settings$method=="shares"){
    settings$method <- "none"
    if(settings$chatty){
      warning("No quantities 'q' or weights 'w' provided -> settings$method reset to 'none'")
    }
  }

  # store initial ordering of region levels:
  r.lvl <- levels(factor(r))

  # compute bilateral price index numbers:
  pdata <- index.pairs(r=r, n=n, p=p, q=q, w=w,
                       settings=list(
                         base=base, # this setting is not visible/exported
                         chatty=settings$chatty,
                         as.dt=TRUE,
                         connect=settings$connect,
                         type=settings$type,
                         all.pairs=settings$all.pairs))

  # no weighting in second aggregation step:
  if(settings$method=="none"){
    w2 <- NULL
  }

  # weighting with respect to number of intersecting items:
  if(settings$method=="obs"){

    # compute intersecting observations:
    dtw <- crossprod(as.matrix(table(n, r, dnn=NULL))>0)
    wdata <- as.data.table(as.table(dtw))
    setnames(x=wdata, new=c("region","base","w"))
    pdata <- merge(x=pdata, y=wdata, all.x=TRUE, by=c("region","base"))
    w2 <- pdata$w

  }

  # weighting with respect to intersecting expenditure shares:
  if(settings$method=="shares"){

    # derive or set expenditure shares:
    if(!is.null(q)){
      dtw <- data.table(r, n, "s"=p*q)
      dtw[, "s" := s/sum(s), by="r"]
    }else{
      dtw <- data.table(r, n, "s"=w)
    }

    # compute average intersecting expenditure shares:
    dtw <- merge(x=dtw, y=dtw, by="n", all=TRUE, allow.cartesian=TRUE)
    wdata <- dtw[, sum((s.x+s.y)/2), by=c("r.x","r.y")]
    setnames(x=wdata, new=c("region","base","w"))
    pdata <- merge(x=pdata, y=wdata, all.x=TRUE, by=c("region","base"))
    w2 <- pdata$w

  }

  # set response and explanatory variables:
  index <- pdata$index
  r <- factor(pdata$region)
  rb <- factor(pdata$base)

  # set base region:
  if(!base%in%levels(r) && !is.null(base)){
    # reset base region and print warning:
    base <- names(which.max(table(r)))[1]
    if(settings$chatty){
      warning(paste0("Base region not found -> reset to base='", base, "'"), call.=FALSE)
    }
  }

  # relevel to base region:
  if(!is.null(base)){
    r <- relevel(x=r, ref=base)
    rb <- relevel(x=rb, ref=base)
  }

  # define regression model if one region only:
  if(nlevels(r) <= 1){

    # empty regression formula:
    geks_mod <- log(index) ~ 0
    # no regional comparison of prices possible in this case

  }else{

    # update contrasts:
    if(is.null(base)){

      contrasts(x=r) <- contr.sum(levels(r))
      colnames(contrasts(x=r)) <- levels(r)[-nlevels(r)]

      contrasts(x=rb) <- contr.sum(levels(rb))
      colnames(contrasts(x=rb)) <- levels(rb)[-nlevels(rb)]

    }else{

      contrasts(x=r) <- contr.treatment(levels(r))

      contrasts(x=rb) <- contr.treatment(levels(rb))

    }

    # gather all region levels:
    r.lvl.all <- unique(x=c(levels(r), levels(rb)))

    # compute model matrix:
    lnP <- model.matrix(~r, xlev=r.lvl.all)-model.matrix(~rb, xlev=r.lvl.all)
    colnames(lnP) <- sub(pattern="^r", replacement="", x=colnames(lnP))
    lnP <- lnP[,-1]

    # GEKS regression formula:
    geks_mod <- log(index) ~ lnP - 1

  }

  # estimate GEKS regression model:
  geks_reg_out <- lm(formula=geks_mod, weights=w2, singular.ok=FALSE)

  # simplify to price levels only or full regression output:
  if(simplify){

    # extract estimated regional price levels:
    out <- coef(geks_reg_out)
    names(out) <- gsub(pattern="^lnP", replacement="", x=names(out))

    # add price level of base region:
    r.miss <- setdiff(x=levels(r), y=names(out))
    if(is.null(base)) out.miss <- -sum(out) else out.miss <- 0
    names(out.miss) <- r.miss
    out <- c(out, out.miss)

    # match to initial ordering and unlog:
    out <- exp(out)[match(x=r.lvl, table=names(out))]
    names(out) <- r.lvl

  }else{

    # keep lm-object:
    out <- geks_reg_out
    names(out$coefficients) <- sub("^(lnP)", "\\1.", names(out$coefficients))

  }

  # print output to console:
  return(out)

  # # compute multilateral GEKS index numbers:
  # out <- colMeans(x=log(pmat*pmat[idx,]), na.rm=TRUE)
  #
  # # scale if necessary:
  # if(is.null(base)){out <- scale(x=out, center=TRUE, scale=FALSE)[,1]}
  #
  # # unlog price levels:
  # return(exp(out))

}

# -> IDEA:
#    o GEKS: two-step procedure. Therefore, estimated standard errors (SE) are
#      zero in the second (regression) step when prices are fully available
#    o CPD: one step procedure. Therefore, SE are always greater than zero.
#    -> split the SE of CPD in two components, similar to GEKS: (i) SE-component
#       for the aggregation of prices into index numbers, and (ii) SE-component
#       for uncertainty due to gaps in the price data

# END
