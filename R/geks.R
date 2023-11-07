# START

# Title:  Bilateral index pairs and GEKS method
# Author: Sebastian Weinand
# Date:   6 November 2023

# compute bilateral index pairs:
index.pairs <- function(p, r, n, q=NULL, w=NULL, settings=list()){

  # set default if missing:
  if(missing(q)) q <- NULL
  if(missing(w)) w <- NULL

  # set default settings if missing:
  if(is.null(settings$connect)) settings$connect <- FALSE
  if(is.null(settings$chatty)) settings$chatty <- TRUE
  if(is.null(settings$type)) settings$type <- "jevons"
  if(is.null(settings$all.pairs)) settings$all.pairs <- TRUE
  if(is.null(settings$as.dt)) settings$as.dt <- FALSE

  # non-exported settings:
  if(is.null(settings$check.inputs)) settings$check.inputs <- TRUE
  if(is.null(settings$missings)) settings$missings <- TRUE
  if(is.null(settings$duplicates)) settings$duplicates <- TRUE
  # the setting 'setting$base' is only used by geks() as provided
  # by the user, but always FALSE for index.pairs()

  # input checks:
  if(settings$check.inputs){

    # main inputs:
    .check.num(x=p, int=c(0, Inf))
    .check.char(x=r)
    .check.char(x=n)
    .check.num(x=q, miss.ok=TRUE, null.ok=TRUE, int=c(0, Inf))
    .check.num(x=w, miss.ok=TRUE, null.ok=TRUE, int=c(0, Inf))
    .check.lengths(x=r, y=n)
    .check.lengths(x=r, y=p)
    .check.lengths(x=r, y=q)
    .check.lengths(x=r, y=w)

    # settings:
    .check.char(x=settings$type, min.len=1, max.len=1, na.ok=FALSE)
    .check.log(x=settings$all.pairs, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
    .check.log(x=settings$as.dt, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
    .check.log(x=settings$connect, min.len=1, max.len=1, na.ok=FALSE)
    .check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)

  }

  # allowed index types:
  type.vals <- c("jevons", "carli", "dutot", "harmonic", "toernq", "laspey", "paasche", "walsh", "fisher")

  # check against allowed index types:
  type <- match.arg(arg=settings$type, choices=type.vals)

  # weights required for following index types:
  type.weights <- c("toernq", "laspey", "paasche", "walsh", "fisher")

  # error handling for quantity and weights:
  if(settings$check.inputs && type%in%type.weights && is.null(q) && is.null(w)){
    stop(paste0("Non-valid input for type='", type, "' -> 'q' or 'w' required"), call.=FALSE)
  }

  # set index function based on type:
  # NOTE that these are the matrix-version functions!
  index_func <- switch(type,
                       jevons = spin:::.jevons,
                       carli = spin:::.carli,
                       dutot = spin:::.dutot,
                       harmonic = spin:::.harmonic,
                       toernq = spin:::.toernq,
                       laspey = spin:::.laspey,
                       paasche = spin:::.paasche,
                       fisher = spin:::.fisher,
                       walsh = spin:::.walsh)

  # initialize data:
  pdata <- spin:::arrange(p=p, r=r, n=n, q=q, w=w, base=settings$base, settings=settings)

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

  # set default if missing:
  if(missing(q)) q <- NULL
  if(missing(w)) w <- NULL

  # set default settings if missing:
  if(is.null(settings$connect)) settings$connect <- TRUE
  if(is.null(settings$chatty)) settings$chatty <- TRUE
  if(is.null(settings$type)) settings$type <- "jevons"
  if(is.null(settings$wmethod)) settings$wmethod <- "none"
  if(is.null(settings$all.pairs)) settings$all.pairs <- TRUE

  # non-exported settings:
  if(is.null(settings$check.inputs)) settings$check.inputs <- TRUE
  if(is.null(settings$missings)) settings$missings <- TRUE
  if(is.null(settings$duplicates)) settings$duplicates <- TRUE

  # input checks:
  if(settings$check.inputs){

    # main inputs:
    .check.log(x=simplify, miss.ok=TRUE, min.len=1, max.len=1, na.ok=FALSE)
    .check.char(x=base, min.len=1, max.len=1, miss.ok=TRUE, null.ok=TRUE, na.ok=FALSE)

    # settings:
    .check.char(x=settings$wmethod, min.len=1, max.len=1, na.ok=FALSE)

    # input checks of other arguments are performed in index.pairs()

  }

  # match weighting method:
  wmethod <- match.arg(arg=settings$wmethod, choices=c("none","obs","shares"))

  # check if weighting method can be applied:
  if(is.null(q) && is.null(w) && settings$wmethod=="shares"){
    settings$wmethod <- "none"
    if(settings$chatty){
      warning("No quantities 'q' or weights 'w' provided -> settings$wmethod reset to 'none'")
    }
  }

  # store initial ordering of region levels:
  r.lvl <- levels(factor(r))

  # compute bilateral price index numbers:
  pdata <- index.pairs(r=r, n=n, p=p, q=q, w=w,
                       settings=list(
                         check.inputs=settings$check.inputs,
                         missings=settings$missings,
                         duplicates=settings$duplicates,
                         chatty=settings$chatty,
                         connect=settings$connect,
                         base=base, # this setting is not visible/exported
                         as.dt=TRUE,
                         type=settings$type,
                         all.pairs=settings$all.pairs))

  # no weighting in second aggregation step:
  if(settings$wmethod=="none"){
    w2 <- NULL
  }

  # weighting with respect to number of intersecting items:
  if(settings$wmethod=="obs"){

    # compute intersecting observations:
    dtw <- crossprod(as.matrix(table(n, r, dnn=NULL))>0)
    wdata <- as.data.table(as.table(dtw))
    setnames(x=wdata, new=c("region","base","w"))
    pdata <- merge(x=pdata, y=wdata, all.x=TRUE, by=c("region","base"))
    w2 <- pdata$w

  }

  # weighting with respect to intersecting expenditure shares:
  if(settings$wmethod=="shares"){

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
  base <- spin:::set.base(r=r, base=base, null.ok=TRUE, settings=settings)

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

}

# END
