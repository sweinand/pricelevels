# START

# Title:    Bilateral price indices
# Author:   Sebastian Weinand
# Date:     26 August 2023

# helper functions for comparison of two regions if
# item sets are already matched. these will be called
# by their corresponding package functions
.jevons2 <- function(p1, q1=NULL, w1=NULL, p0, q0=NULL, w0=NULL){

  return(exp(mean(log(p1/p0))))

}
.dutot2 <- function(p1, q1=NULL, w1=NULL, p0, q0=NULL, w0=NULL){

  return(mean(p1)/mean(p0))

}
.carli2 <- function(p1, q1=NULL, w1=NULL, p0, q0=NULL, w0=NULL){

  return(mean(p1/p0))

}
.harmonic2 <- function(p1, q1=NULL, w1=NULL, p0, q0=NULL, w0=NULL){

  return(1/mean(p0/p1))

}
.toernq2 <- function(p1, q1, w1, p0, q0, w0){

  # define weights:
  if(missing(q0) | missing(q1)){
    w <- 0.5*(w0/sum(w0) + w1/sum(w1))
  }else{
    w <- 0.5*(p0*q0/sum(p0*q0) + p1*q1/sum(p1*q1))
  }

  # normalize weights:
  w <- w/sum(w)

  # compute index:
  res <- exp(weighted.mean(x=log(p1/p0), w=w))

  # return output:
  return(res)

}
.walsh2 <- function(p1, q1, w1, p0, q0, w0){

  # define weights:
  if(missing(q0) | missing(q1)){
    w <- sqrt(w0/sum(w0)*w1/sum(w1))
  }else{
    w <- sqrt((p0*q0/sum(p0*q0)) * (p1*q1/sum(p1*q1)))
  }

  # normalize weights:
  w <- w/sum(w)

  # compute index:
  res <- weighted.mean(x=sqrt(p1/p0), w=w) / weighted.mean(x=sqrt(p0/p1), w=w)

  # return output:
  return(res)

}
.laspey2 <- function(p1, q1, w1, p0, q0, w0){

  # define weights:
  if(missing(q0) | missing(q1)){
    w <- w0/sum(w0)
  }else{
    w <- p0*q0/sum(p0*q0)
  }

  # normalize weights:
  w <- w/sum(w)

  # compute index:
  res <- weighted.mean(x=p1/p0, w=w)

  # return output:
  return(res)

}
.paasche2 <- function(p1, q1, w1, p0, q0, w0){

  # define weights:
  if(missing(q0) | missing(q1)){
    w <- w1/sum(w1)
  }else{
    w <- p1*q1/sum(p1*q1)
  }

  # normalize weights:
  w <- w/sum(w)

  # compute index:
  res <- 1/weighted.mean(x=p0/p1, w=w)

  # return output:
  return(res)

}
.fisher2 <- function(p1, q1, w1, p0, q0, w0){

  # compute laspeyres indices:
  l <- .laspey2(p1, q1, w1, p0, q0, w0)

  # compute paasche indices:
  p <- .paasche2(p1, q1, w1, p0, q0, w0)

  # compute Fisher:
  return(sqrt(l*p))

}

# helper functions for comparison of multiple regions to
# one base region subsetting to intersecting items these
# will be called by index.pairs() and thus also geks(),
# where they are faster.
.jevons <- function(P, Q=NULL, W=NULL, base=1L){

  # compute index:
  res <- exp(colMeans(x=log(P/P[, base]), na.rm=TRUE))

  # set to NA when no intersecting prices were found:
  res[is.nan(res)] <- NA

  # re-transform logs:
  return(res)

}
.dutot <- function(P, Q=NULL, W=NULL, base=1L){

  # price matrix of base region:
  Pbase <- matrix(data=P[, base], ncol=ncol(P), nrow=nrow(P))

  # set NAs to avoid that means are based on different sets:
  P[is.na(Pbase)] <- NA
  Pbase[is.na(P)] <- NA

  # compute index:
  res <- colMeans(x=P, na.rm=TRUE)/colMeans(x=Pbase, na.rm=TRUE)

  # set to NA when no intersecting prices were found:
  res[is.nan(res)] <- NA

  # print output to console:
  return(res)

}
.carli <- function(P, Q=NULL, W=NULL, base=1L){

  # compute index:
  res <- colMeans(x=P/P[, base], na.rm=TRUE)

  # set to NA when no intersecting prices were found:
  res[is.nan(res)] <- NA

  # print output to console:
  return(res)

}
.harmonic <- function(P, Q=NULL, W=NULL, base=1L){

  # compute index:
  res <- 1/colMeans(x=1/(P/P[, base]), na.rm=TRUE)

  # set to NA when no intersecting prices were found:
  res[is.nan(res)] <- NA

  # print output to console:
  return(res)

}
.walsh <- function(P, Q, W, base=1L){

  # compute weights:
  if(missing(Q)){
    W <- sqrt(W*W[, base])
  }else{
    W <- sqrt(P*Q/colSums(P*Q, na.rm=TRUE)[col(P)] * P[,base]*Q[,base]/sum(P[,base]*Q[,base], na.rm=TRUE))
  }

  # set to NA if no intersection:
  W[is.na(P/P[,base])] <- NA

  # normalize weights:
  W <- W/colSums(x=W, na.rm=TRUE)[col(W)]

  # compute index:
  res <- colSums(x=W*sqrt(P/P[, base]), na.rm=TRUE)/colSums(x=W*sqrt(P[, base]/P), na.rm=TRUE)

  # set to NA when no intersecting prices were found:
  res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
  # -> colSums()-function returns 0 when everything is NA

  # print output to console:
  return(res)

}
.toernq <- function(P, Q, W, base=1L){

  # compute logarithmic differences:
  R <- log(P/P[, base])

  # compute weights:
  if(missing(Q)){
    Wbase <- matrix(data=W[,base], ncol=ncol(P), nrow=nrow(P))
    Wall <- W
  }else{
    Wbase <- matrix(data=P[,base]*Q[,base], ncol=ncol(P), nrow=nrow(P))
    Wall <- P*Q
  }

  Wbase[is.na(R)] <- NA
  Wall[is.na(R)] <- NA
  W <- 0.5*(Wall/colSums(Wall, na.rm=TRUE)[col(R)] + Wbase/colSums(Wbase, na.rm=TRUE)[col(R)])

  # compute index:
  res <- colSums(x=W*R, na.rm=TRUE)

  # set to NA when no intersecting prices were found:
  res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
  # -> colSums()-function returns 0 when everything is NA

  # re-transform logs:
  return(exp(res))

}
.laspey <- function(P, Q, W, base=1L){

  # compute price ratios:
  R <- P / P[, base]

  # define weights:
  if(missing(Q)){
    W <- W[, base]
  }else{
    W <- P[,base]*Q[,base]
  }

  # align dimensions of weighting matrix:
  W <- W*matrix(data=1, nrow=nrow(P), ncol=ncol(P), dimnames=dimnames(P))

  # set weights to NA when no intersection of prices:
  W[is.na(R)] <- NA

  # normalize weights:
  W <- W/colSums(x=W, na.rm=TRUE)[col(W)]

  # compute index:
  res <- colSums(x=W*R, na.rm=TRUE)

  # set to NA when no intersecting prices were found:
  res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
  # -> colSums()-function returns 0 when everything is NA

  # print output to console:
  return(res)

}
.paasche <- function(P, Q, W, base=1L){

  # compute price ratios:
  R <- 1 / (P / P[, base])

  # compute weights:
  if(missing(Q)){
    W <- W
  }else{
    W <- P*Q
  }

  # set weights to NA when no intersection of prices:
  W[is.na(R)] <- NA

  # normalize weights:
  W <- W/colSums(x=W, na.rm=TRUE)[col(W)]

  # compute index:
  res <- 1/colSums(x=W*R, na.rm=TRUE)

  # set to NA when no intersecting prices were found:
  res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
  # -> colSums()-function returns 0 when everything is NA

  # print output to console:
  return(res)

}
.fisher <- function(P, Q, W, base=1L){

  # compute laspeyres indices:
  l <- .laspey(P=P, Q=Q, W=W, base=base)

  # compute paasche indices:
  p <- .paasche(P=P, Q=Q, W=W, base=base)

  # compute Fisher:
  return(sqrt(l*p))

}

# main function to be called, used for input checking
# and data preparation, which is the same for each
# bilateral index
.bilateral.index <- function(p, r, n, q, w=NULL, type, base=NULL){

  # check 'type':
  .check.char(x=type, min.len=1, max.len=1, na.ok=FALSE)

  # allowed index types:
  type.vals <- c("jevons", "carli", "dutot", "harmonic", "toernq", "laspey", "paasche", "walsh", "fisher")

  # check against allowed index types:
  type <- match.arg(arg=type, choices=type.vals)

  # set index function based on type:
  # NOTE that these are the index2()-functions!
  if(type=="jevons"){index_func <- .jevons2}
  if(type=="carli"){index_func <- .carli2}
  if(type=="dutot"){index_func <- .dutot2}
  if(type=="harmonic"){index_func <- .harmonic2}
  if(type=="toernq"){index_func <- .toernq2}
  if(type=="laspey"){index_func <- .laspey2}
  if(type=="paasche"){index_func <- .paasche2}
  if(type=="walsh"){index_func <- .walsh2}
  if(type=="fisher"){index_func <- .fisher2}

  # weights required for following index types:
  type.weights <- c("toernq", "laspey", "paasche", "walsh", "fisher")

  # error handling for quantity and weights:
  if(type%in%type.weights){

    if((missing(w) || is.null(w)) && (missing(q) || is.null(q))){
      stop(paste0("Non-valid input for type -> 'q' or 'w' required for type='", type, "'"))
    }else{
      # work with quantities or weights:
      if(!(missing(q) || is.null(q))){
        z <- q
        q.avail <- TRUE
      }else{
        z <- w # we will work with q where q are the weights w
        q.avail <- FALSE
      }
    }

  }else{

    z <- rep(1, length(p))
    q.avail <- TRUE

  }

  # input checks:
  .check.num(x=p, int=c(0, Inf))
  .check.num(x=z, null.ok=FALSE, int=c(0, Inf))
  .check.char(x=r)
  .check.char(x=n)
  .check.char(x=base, min.len=1, max.len=1, miss.ok=TRUE, null.ok=TRUE, na.ok=FALSE)
  .check.lengths(x=r, y=n)
  .check.lengths(x=r, y=p)
  .check.lengths(x=r, y=z)

  # set default base if necessary:
  if(is.null(base)){base <- names(which.max(table(r)))[1]} # when base is NULL
  if(!(base%in%r)){ # when base is no valid region
    base <- names(which.max(table(r)))[1]
    warning(paste("Base region not found and reset to", base))
  }

  # gather in data.table:
  pdata <- data.table("r"=factor(r), "n"=factor(n), "p"=as.numeric(p), "z"=as.numeric(z))

  # store initial ordering of region levels:
  r.lvl <- levels(pdata$r)

  # set key:
  setkeyv(x=pdata, cols=c("r", "n"))

  # check for duplicated entries:
  if(anyDuplicated(x=pdata, by=key(pdata)) > 0){

    # average duplicated prices and weights:
    if(q.avail){
      pdata <- pdata[, list("p"=mean(p), "z"=sum(z)), by = c("r", "n")]
    }else{
      pdata <- pdata[, list("p"=mean(p), "z"=mean(z)), by = c("r", "n")]
    }

  }

  # intersection with base region prices and weights:
  pdata <- merge(x=pdata, y=pdata[r==base,], by="n", all=FALSE, suffixes=c("","_base"))

  # compute price index for each region:
  if(q.avail){
    aggdata <- pdata[, index_func(p1=p, q1=z, p0=p_base, q0=z_base), by="r"]
  }else{
    aggdata <- pdata[, index_func(p1=p, w1=z, p0=p_base, w0=z_base), by="r"]
  }

  # ensure that results contain all regions, also in cases
  # where no product matches were found. This is important
  # in cases of incomplete price data:
  aggdata <- merge(x=data.table("r"=levels(pdata$r)), y=aggdata, by="r", all.x=TRUE)

  # coerce to vector:
  res <- setNames(aggdata$V1, aggdata$r)

  # match to initial ordering and unlog:
  res <- res[match(x=r.lvl, table=names(res))]

  # print output to console:
  return(res)

}

# package functions:
walsh <- function(p, r, n, q, w=NULL, base=NULL){

  .bilateral.index(r=r, n=n, p=p, q=q, w=w, type="walsh", base=base)

}
toernq <- function(p, r, n, q, w=NULL, base=NULL){

  .bilateral.index(r=r, n=n, p=p, q=q, w=w, type="toernq", base=base)

}
laspey <- function(p, r, n, q, w=NULL, base=NULL){

  .bilateral.index(r=r, n=n, p=p, q=q, w=w, type="laspey", base=base)

}
paasche <- function(p, r, n, q, w=NULL, base=NULL){

  .bilateral.index(r=r, n=n, p=p, q=q, w=w, type="paasche", base=base)

}
fisher <- function(p, r, n, q, w=NULL, base=NULL){

  .bilateral.index(r=r, n=n, p=p, q=q, w=w, type="fisher", base=base)

}
jevons <- function(p, r, n, base=NULL){

  .bilateral.index(r=r, n=n, p=p, q=NULL, type="jevons", base=base)

}
dutot <- function(p, r, n, base=NULL){

  .bilateral.index(r=r, n=n, p=p, q=NULL, type="dutot", base=base)

}
carli <- function(p, r, n, base=NULL){

  .bilateral.index(r=r, n=n, p=p, q=NULL, type="carli", base=base)

}
harmonic <- function(p, r, n, base=NULL){

  .bilateral.index(r=r, n=n, p=p, q=NULL, type="harmonic", base=base)

}

# END
