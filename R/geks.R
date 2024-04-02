# START

# Title:  Bilateral index pairs and GEKS method
# Author: Sebastian Weinand
# Date:   14 March 2024

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

  # non-exported settings:
  if(is.null(settings$check.inputs)) settings$check.inputs <- TRUE
  if(is.null(settings$missings)) settings$missings <- TRUE
  if(is.null(settings$duplicates)) settings$duplicates <- TRUE
  settings$norm.weights <- TRUE
  # the setting 'setting$base' is only used by geks() as provided
  # by the user, but always FALSE for index.pairs()

  # input checks:
  if(settings$check.inputs){

    # main inputs:
    check.num(x=p, int=c(0, Inf))
    check.char(x=r)
    check.char(x=n)
    check.num(x=q, miss.ok=TRUE, null.ok=TRUE, int=c(0, Inf))
    check.num(x=w, miss.ok=TRUE, null.ok=TRUE, int=c(0, Inf))
    check.lengths(x=r, y=n)
    check.lengths(x=r, y=p)
    check.lengths(x=r, y=q)
    check.lengths(x=r, y=w)

    # settings:
    check.char(x=settings$type, min.len=1, max.len=Inf, na.ok=FALSE)
    check.log(x=settings$all.pairs, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
    check.log(x=settings$connect, min.len=1, max.len=1, na.ok=FALSE)
    check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
    check.char(x=settings$qbase, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE)

  }

  # allowed index types:
  type.vals <- pindices[type=="bilateral", ]

  # check against allowed index types:
  type <- match.arg(arg=settings$type, choices=type.vals$name, several.ok=TRUE)

  # error handling for quantity and weights:
  if(settings$check.inputs){

    if(any(type%in%type.vals$name[type.vals$uses_q==TRUE & type.vals$uses_w==TRUE]) && is.null(q) && is.null(w)){
      stop(paste0("Non-valid input -> 'q' or 'w' required but both missing"), call.=FALSE)
    }

    if(any(type%in%type.vals$name[type.vals$uses_q==TRUE & type.vals$uses_w==FALSE]) && is.null(q)){
      stop(paste0("Non-valid input -> 'q' required but missing"), call.=FALSE)
    }

  }

  # set "matrix index function" based on type:
  indexfn <- Pmatrix[match(x=type, table=names(Pmatrix))]

  # initialize data:
  pdata <- arrange(p=p, r=r, n=n, q=q, w=w, base=settings$base, settings=settings)

  # convert prices into matrix:
  P <- as.matrix(
    x=dcast(data=pdata, formula=n~r, fun.aggregate=mean, value.var="p", fill=NA),
    rownames="n")

  # convert weights or quantities into matrix, where duplicated weights
  # are averaged, duplicated quantites added up:
  if(is.null(q) && is.null(w)){
    Z <- NULL
  }else{
    if(is.null(q)){
      Z <- dcast(data=pdata, formula=n~r, fun.aggregate=mean, value.var="w", fill=NA)
    }else{
      Z <- dcast(data=pdata, formula=n~r, fun.aggregate=sum, value.var="q", fill=NA)
    }
    Z <- as.matrix(x=Z, rownames="n")
  }

  # set quantity base region:
  if(any(c("lowe","young")%in%type)){
    qbase <- set.base(r=pdata$r, base=settings$qbase, null.ok=TRUE, qbase=TRUE, settings=settings)
    if(!is.null(qbase)) qbase <- which(colnames(P)%in%qbase)
  }else{
    qbase <- NULL
  }

  # number of regions or time periods:
  R <- ncol(P)

  # loop over all indices:
  out <- vector(mode="list", length=length(type))
  for(j in seq_along(type)){

    # container to store price levels:
    res <- matrix(data=NA_real_, nrow=R, ncol=R, dimnames=list(colnames(P), colnames(P)))

    if(!settings$all.pairs){

      # faster, but only-non-redundant
      if(is.null(q)){
        for(i in 1:R){
          idx <- i:R # tighten column selection in each iteration
          res[i, idx] <- indexfn[[j]](P=P[, idx, drop=FALSE], W=Z[, idx, drop=FALSE], base=1L, qbase=qbase)
        }
      }else{
        for(i in 1:R){
          idx <- i:R # tighten column selection in each iteration
          res[i, idx] <- indexfn[[j]](P=P[, idx, drop=FALSE], Q=Z[, idx, drop=FALSE], base=1L, qbase=qbase)
        }
      }

      # slower, but complete
    }else{

      if(is.null(q)){
        for(i in 1:R) res[i, ] <- indexfn[[j]](P=P, W=Z, base=i, qbase=qbase)
      }else{
        for(i in 1:R) res[i, ] <- indexfn[[j]](P=P, Q=Z, base=i, qbase=qbase)
      }

    }

    out[[j]] <- res

  }

  # convert into datatable:
  res <- expand.grid("base"=colnames(out[[1]]), "region"=rownames(out[[1]]), KEEP.OUT.ATTRS=FALSE)
  for(j in seq_along(type)) res[, type[j]] <- as.vector(out[[j]])
  res <- stats::na.omit(res)
  res <- as.data.table(x=res, key=c("base", "region"))

  # print output to console:
  return(res)

}

# compute mutlilateral GEKS index:
geks.main <- function(p, r, n, q=NULL, w=NULL, base=NULL, simplify=TRUE, settings=list()){

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
  settings$norm.weights <- TRUE

  # input checks:
  if(settings$check.inputs){

    # main inputs:
    check.log(x=simplify, miss.ok=TRUE, min.len=1, max.len=1, na.ok=FALSE)
    check.char(x=base, min.len=1, max.len=1, miss.ok=TRUE, null.ok=TRUE, na.ok=FALSE)

    # settings:
    check.char(x=settings$wmethod, min.len=1, max.len=1, na.ok=FALSE)

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
  pdata <- index.pairs(r=r, n=n, p=p, q=q, w=w, settings=c(list("base"=base), settings))

  # get the processed index types:
  type <- setdiff(colnames(pdata), c("base","region"))

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

    # address global bindings note when checking:
    s <- s.x <- s.y <- NULL

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
  index <- as.matrix(subset(x=pdata, select=type))
  colnames(index) <- paste("geks", colnames(index), sep="-")
  r <- factor(pdata$region)
  rb <- factor(pdata$base)

  # set base region:
  base <- set.base(r=r, base=base, null.ok=TRUE, settings=settings)

  # relevel to base region:
  if(!is.null(base)){
    r <- stats::relevel(x=r, ref=base)
    rb <- stats::relevel(x=rb, ref=base)
  }

  # define regression model if one region only:
  if(nlevels(r) <= 1){

    # empty regression formula:
    geks_mod <- log(index) ~ 0
    # no regional comparison of prices possible in this case

  }else{

    # update contrasts:
    if(is.null(base)){

      stats::contrasts(x=r) <- stats::contr.sum(levels(r))
      colnames(stats::contrasts(x=r)) <- levels(r)[-nlevels(r)]

      stats::contrasts(x=rb) <- stats::contr.sum(levels(rb))
      colnames(stats::contrasts(x=rb)) <- levels(rb)[-nlevels(rb)]

    }else{

      stats::contrasts(x=r) <- stats::contr.treatment(levels(r))

      stats::contrasts(x=rb) <- stats::contr.treatment(levels(rb))

    }

    # gather all region levels:
    r.lvl.all <- unique(x=c(levels(r), levels(rb)))

    # compute model matrix:
    lnP <- stats::model.matrix(~r, xlev=r.lvl.all)-stats::model.matrix(~rb, xlev=r.lvl.all)
    colnames(lnP) <- sub(pattern="^r", replacement="", x=colnames(lnP))
    lnP <- lnP[,-1, drop=FALSE]

    # GEKS regression formula:
    geks_mod <- log(index) ~ lnP - 1

  }

  # estimate GEKS regression model:
  geks_reg_out <- stats::lm(formula=geks_mod, weights=w2, singular.ok=FALSE)

  # simplify to price levels only or full regression output:
  if(simplify){

    # extract estimated regional price levels:
    out <- as.matrix(stats::coef(geks_reg_out))
    if(nlevels(r)>1 && ncol(lnP)<=1) rownames(out) <- paste0("lnP", colnames(lnP))
    rownames(out) <- gsub(pattern="^lnP", replacement="", x=rownames(out))

    # add price level of base region:
    r.miss <- setdiff(x=levels(r), y=rownames(out))
    if(is.null(base)) out.miss <- -colSums(out) else out.miss <- 0
    out.miss <- matrix(data=out.miss, ncol=ncol(out), dimnames=list(r.miss, paste("geks", type, sep="-")))
    out <- rbind(out, out.miss)

    # match to initial ordering and unlog:
    out <- exp(out)[match(x=r.lvl, table=rownames(out)),, drop=FALSE]
    rownames(out) <- r.lvl
    out <- t(out)

  }else{

    # keep lm-object:
    if(length(type)>1L){
      rownames(geks_reg_out$coefficients) <- sub("^(lnP)", "\\1.", rownames(geks_reg_out$coefficients))
    }else{
      names(geks_reg_out$coefficients) <- sub("^(lnP)", "\\1.", names(geks_reg_out$coefficients))
    }
    out <- geks_reg_out

  }

  # print output to console:
  return(out)

}

# exported function:
geks <- function(p, r, n, q=NULL, w=NULL, base=NULL, simplify=TRUE, settings=list()){

  res <- geks.main(p=p, r=r, n=n, q=q, w=w, base=base, simplify=simplify, settings=settings)
  if(simplify && nrow(res)<=1L) res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}

# END
