# START

# Title:    Bilateral price indices
# Author:   Sebastian Weinand
# Date:     16 November 2023

# see pages 603-628 of the Export and Import Price Index Manual
# https://www.imf.org/external/np/sta/xipim/pdf/xipim.pdf
# for a comprehensive list of price index formulas

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
.cswd2 <- function(p1, q1=NULL, w1=NULL, p0, q0=NULL, w0=NULL){

  # compute carli indices:
  Pc <- spin:::.carli2(p1, q1, w1, p0, q0, w0)

  # compute harmonic indices:
  Ph <- spin:::.harmonic2(p1, q1, w1, p0, q0, w0)

  # compute Fisher:
  return(sqrt(Pc*Ph))

}
.medgeworth2 <- function(p1, q1, w1=NULL, p0, q0, w0=NULL){

  # define weights:
  w <- p0*(q0+q1)/sum(p0*(q0+q1))

  # normalize weights:
  w <- w/sum(w)

  # compute index:
  res <- weighted.mean(x=p1/p0, w=w)

  # return output:
  return(res)

}
.theil2 <- function(p1, q1, w1=NULL, p0, q0, w0=NULL){

  # define weights:
  w <- ((p0*q0+p1*q1)/2*(p0*q0)*(p1*q1))^(1/3)

  # normalize weights:
  w <- w/sum(w)

  # compute index:
  res <- exp(weighted.mean(x=log(p1/p0), w=w))

  # return output:
  return(res)

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
.geo.laspey2 <- function(p1, q1, w1, p0, q0, w0){

  # define weights:
  if(missing(q0) | missing(q1)){
    w <- w0/sum(w0)
  }else{
    w <- p0*q0/sum(p0*q0)
  }

  # normalize weights:
  w <- w/sum(w)

  # compute index:
  res <- exp(weighted.mean(x=log(p1/p0), w=w))

  # return output:
  return(res)

}
.geo.paasche2 <- function(p1, q1, w1, p0, q0, w0){

  # define weights:
  if(missing(q0) | missing(q1)){
    w <- w1/sum(w1)
  }else{
    w <- p1*q1/sum(p1*q1)
  }

  # normalize weights:
  w <- w/sum(w)

  # compute index:
  res <- 1/exp(weighted.mean(x=log(p0/p1), w=w))

  # return output:
  return(res)

}
.palgrave2 <- function(p1, q1, w1, p0, q0, w0){

  # define weights:
  if(missing(q0) | missing(q1)){
    w <- w1/sum(w1)
  }else{
    w <- p1*q1/sum(p1*q1)
  }

  # normalize weights:
  w <- w/sum(w)

  # compute index:
  res <- weighted.mean(x=p1/p0, w=w)

  # return output:
  return(res)

}
.svartia2 <- function(p1, q1, w1, p0, q0, w0){

  # define weights:
  if(missing(q0) | missing(q1)){
    w0 <- w0/sum(w0)
    w1 <- w1/sum(w1)
  }else{
    w1 <- p1*q1/sum(p1*q1)
    w0 <- p0*q0/sum(p0*q0)
  }

  # define weights:
  w <- ifelse(abs(w1-w0)>1e-7, (w1-w0)/(log(w1)-log(w0)), w0)

  # normalize weights:
  w <- w/sum(w)

  # compute index:
  res <- exp(weighted.mean(x=log(p1/p0), w=w))

  # return output:
  return(res)

}
.drobisch2 <- function(p1, q1, w1, p0, q0, w0){

  # compute laspeyres indices:
  l <- spin:::.laspey2(p1, q1, w1, p0, q0, w0)

  # compute paasche indices:
  p <- spin:::.paasche2(p1, q1, w1, p0, q0, w0)

  # compute Fisher:
  return((l+p)/2)

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
.cswd <- function(P, Q=NULL, W=NULL, base=1L){

  # compute carli indices:
  Pc <- spin:::.carli(P=P, Q=Q, W=W, base=base)

  # compute harmonic indices:
  Ph <- spin:::.harmonic(P=P, Q=Q, W=W, base=base)

  # compute Fisher:
  return(sqrt(Ph*Pc))

}
.medgeworth <- function(P, Q, W=NULL, base=1L){

  # compute price ratios:
  R <- P / P[, base]

  # define weights:
  W <- P[,base]*(Q[,base]+Q)

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
.theil <- function(P, Q, W=NULL, base=1L){

  # compute price ratios:
  R <- log(P/P[, base])

  # define weights:
  W <- (((P[,base]*Q[,base]+P*Q)/2)*(P[,base]*Q[,base])*(P*Q))^(1/3)

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
  return(exp(res))

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
.geo.laspey <- function(P, Q, W, base=1L){

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
  res <- exp(colSums(x=W*log(R), na.rm=TRUE))

  # set to NA when no intersecting prices were found:
  res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
  # -> colSums()-function returns 0 when everything is NA

  # print output to console:
  return(res)

}
.geo.paasche <- function(P, Q, W, base=1L){

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
  res <- 1/exp(colSums(x=W*log(R), na.rm=TRUE))

  # set to NA when no intersecting prices were found:
  res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
  # -> colSums()-function returns 0 when everything is NA

  # print output to console:
  return(res)

}
.palgrave <- function(P, Q, W, base=1L){

  # compute price ratios:
  R <- P / P[, base]

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
  res <- colSums(x=W*R, na.rm=TRUE)

  # set to NA when no intersecting prices were found:
  res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
  # -> colSums()-function returns 0 when everything is NA

  # print output to console:
  return(res)

}
.svartia <- function(P, Q, W, base=1L){

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
  Wbase <- Wbase/colSums(Wbase, na.rm=TRUE)[col(R)]
  Wall <- Wall/colSums(Wall, na.rm=TRUE)[col(R)]
  W <- (Wall-Wbase)/(log(Wall)-log(Wbase))
  W <- ifelse(abs(Wall-Wbase)>1e-7, W, Wbase)
  W <- W/colSums(W, na.rm=TRUE)[col(R)]

  # compute index:
  res <- colSums(x=W*R, na.rm=TRUE)

  # set to NA when no intersecting prices were found:
  res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
  # -> colSums()-function returns 0 when everything is NA

  # re-transform logs:
  return(exp(res))

}
.drobisch <- function(P, Q, W, base=1L){

  # compute laspeyres indices:
  l <- spin:::.laspey(P=P, Q=Q, W=W, base=base)

  # compute paasche indices:
  p <- spin:::.paasche(P=P, Q=Q, W=W, base=base)

  # compute Fisher:
  return((l+p)/2)

}

# main function to be called, used for input checking
# and data preparation, which is the same for each
# bilateral index
.bilateral.index <- function(p, r, n, q, w=NULL, type, base=NULL, settings=list()){

  # set default if missing:
  if(missing(q)) q <- NULL
  if(missing(w)) w <- NULL

  # set default settings if missing:
  if(is.null(settings$connect)) settings$connect <- TRUE
  if(is.null(settings$chatty)) settings$chatty <- TRUE

  # non-exported settings:
  if(is.null(settings$check.inputs)) settings$check.inputs <- TRUE
  if(is.null(settings$missings)) settings$missings <- TRUE
  if(is.null(settings$duplicates)) settings$duplicates <- TRUE
  settings$norm.weights <- TRUE

  # input checks:
  if(settings$check.inputs){

    # main inputs:
    .check.num(x=p, int=c(0, Inf))
    .check.char(x=r)
    .check.char(x=n)
    .check.num(x=q, null.ok=TRUE, int=c(0, Inf))
    .check.num(x=w, null.ok=TRUE, int=c(0, Inf))
    .check.char(x=type, min.len=1, max.len=1, na.ok=FALSE)
    .check.char(x=base, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE)
    .check.lengths(x=r, y=n)
    .check.lengths(x=r, y=p)
    .check.lengths(x=r, y=q)
    .check.lengths(x=r, y=w)

    # settings:
    .check.log(x=settings$connect, min.len=1, max.len=1, na.ok=FALSE)
    .check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)

  }

  # allowed index types:
  type.vals <- spin:::pindices[type=="bilateral", ]

  # check against allowed index types:
  type <- match.arg(arg=type, choices=type.vals$name)

  # error handling for quantity and weights:
  if(settings$check.inputs && type%in%type.vals[uses_q==TRUE & uses_w==TRUE, name] && is.null(q) && is.null(w)){
    stop(paste0("Non-valid input for type='", type, "' -> 'q' or 'w' required"), call.=FALSE)
  }
  if(settings$check.inputs && type%in%type.vals[uses_q==TRUE & uses_w==FALSE, name] && is.null(q)){
    stop(paste0("Non-valid input for type='", type, "' -> 'q' required"), call.=FALSE)
  }

  # set index function based on type:
  # NOTE that these are the index2()-functions!
  index_func <- switch(type,
                       jevons = spin:::.jevons2,
                       carli = spin:::.carli2,
                       dutot = spin:::.dutot2,
                       harmonic = spin:::.harmonic2,
                       cswd = spin:::.cswd2,
                       theil = spin:::.theil2,
                       medgeworth = spin:::.medgeworth2,
                       toernq = spin:::.toernq2,
                       laspey = spin:::.laspey2,
                       paasche = spin:::.paasche2,
                       fisher = spin:::.fisher2,
                       walsh = spin:::.walsh2,
                       svartia = spin:::.svartia2,
                       palgrave = spin:::.palgrave2,
                       drobisch = spin:::.drobisch2)

  # initialize data:
  pdata <- spin:::arrange(p=p, r=r, n=n, q=q, w=w, base=base, settings=settings)

  # set base region:
  base <- spin:::set.base(r=pdata$r, base=base, null.ok=FALSE, settings=settings)

  # intersection with base region prices and weights:
  pdata <- merge(x=pdata, y=pdata[r==base,], by="n", all=FALSE, suffixes=c("","_base"))

  # compute price index for each region:
  if(is.null(q)){
    aggdata <- pdata[, index_func(p1=p, w1=z, p0=p_base, w0=z_base), by="r"]
  }else{
    aggdata <- pdata[, index_func(p1=p, q1=z, p0=p_base, q0=z_base), by="r"]
  }

  # ensure that results contain all regions, also in cases
  # where no product matches were found. This is important
  # in cases of incomplete price data:
  r.lvl <- levels(factor(r))
  aggdata <- merge(x=data.table("r"=r.lvl), y=aggdata, by="r", all.x=TRUE)

  # coerce to vector:
  res <- setNames(aggdata$V1, aggdata$r)

  # match to initial ordering and unlog:
  res <- res[match(x=r.lvl, table=names(res))]

  # print output to console:
  return(res)

}

# package functions:
jevons <- function(p, r, n, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, w=NULL, q=NULL, type="jevons", base=base, settings=settings)

}
dutot <- function(p, r, n, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, w=NULL, q=NULL, type="dutot", base=base, settings=settings)

}
carli <- function(p, r, n, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, w=NULL, q=NULL, type="carli", base=base, settings=settings)

}
harmonic <- function(p, r, n, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, w=NULL, q=NULL, type="harmonic", base=base, settings=settings)

}
cswd <- function(p, r, n, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, w=NULL, q=NULL, type="cswd", base=base, settings=settings)

}
medgeworth <- function(p, r, n, q, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, q=q, w=NULL, type="medgeworth", base=base, settings=settings)

}
theil <- function(p, r, n, q, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, q=q, w=NULL, type="theil", base=base, settings=settings)

}
walsh <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, q=q, w=w, type="walsh", base=base, settings=settings)

}
toernq <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, q=q, w=w, type="toernq", base=base, settings=settings)

}
laspey <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, q=q, w=w, type="laspey", base=base, settings=settings)

}
paasche <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, q=q, w=w, type="paasche", base=base, settings=settings)

}
fisher <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, q=q, w=w, type="fisher", base=base, settings=settings)

}
palgrave <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, q=q, w=w, type="palgrave", base=base, settings=settings)

}
svartia <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, q=q, w=w, type="svartia", base=base, settings=settings)

}
drobisch <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  .bilateral.index(r=r, n=n, p=p, q=q, w=w, type="drobisch", base=base, settings=settings)

}

# END
