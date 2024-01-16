# START

# Title:    Bilateral price indices
# Author:   Sebastian Weinand
# Date:     16 January 2024

# see pages 603-628 of the Export and Import Price Index Manual
# https://www.imf.org/external/np/sta/xipim/pdf/xipim.pdf
# for a comprehensive list of price index formulas

# helper functions for comparison of two regions if
# item sets are already matched. these will be called
# by their corresponding package functions
Pmatched <- list(

  "jevons" = function(p1, q1=NULL, w1=NULL, p0, q0=NULL, w0=NULL){

    return(exp(mean(log(p1/p0))))

  },

  "dutot" = function(p1, q1=NULL, w1=NULL, p0, q0=NULL, w0=NULL){

    return(mean(p1)/mean(p0))

  },

  "carli" = function(p1, q1=NULL, w1=NULL, p0, q0=NULL, w0=NULL){

    return(mean(p1/p0))

  },

  "harmonic" = function(p1, q1=NULL, w1=NULL, p0, q0=NULL, w0=NULL){

    return(1/mean(p0/p1))

  },

  "medgeworth" = function(p1, q1, w1=NULL, p0, q0, w0=NULL){

    # define weights:
    w <- p0*(q0+q1)/sum(p0*(q0+q1))

    # normalize weights:
    w <- w/sum(w)

    # compute index:
    res <- weighted.mean(x=p1/p0, w=w)

    # return output:
    return(res)

  },

  "lowe" = function(p1, p0, pb=NULL, qb, wb=NULL){

    # define weights:
    w <- p0*qb/sum(p0*qb)

    # normalize weights:
    w <- w/sum(w)

    # compute index:
    res <- weighted.mean(x=p1/p0, w=w)

    # return output:
    return(res)

  },

  "young" = function(p1, p0, pb, qb, wb){

    # define weights:
    if(missing(qb)){
      w <- wb/sum(wb)
    }else{
      w <- (pb*qb)/sum(pb*qb)
    }

    # compute index:
    res <- weighted.mean(x=p1/p0, w=w)

    # return output:
    return(res)

  },

  "laspeyres" = function(p1, q1, w1, p0, q0, w0){

    # derive normalized weights:
    if(missing(q0)){
      w <- w0/sum(w0)
    }else{
      w <- p0*q0/sum(p0*q0)
    }

    # compute index:
    res <- weighted.mean(x=p1/p0, w=w)

    # return output:
    return(res)

  },

  "paasche" = function(p1, q1, w1, p0, q0, w0){

    # derive normalized weights:
    if(missing(q1)){
      w <- w1/sum(w1)
    }else{
      w <- p1*q1/sum(p1*q1)
    }

    # compute index:
    res <- 1/weighted.mean(x=p0/p1, w=w)

    # return output:
    return(res)

  },

  "palgrave" = function(p1, q1, w1, p0, q0, w0){

    # derive normalized weights:
    if(missing(q1)){
      w <- w1/sum(w1)
    }else{
      w <- p1*q1/sum(p1*q1)
    }

    # compute index:
    res <- weighted.mean(x=p1/p0, w=w)

    # return output:
    return(res)

  },

  "walsh" = function(p1, q1, w1, p0, q0, w0){

    # set weights:
    if(missing(q0) | missing(q1)){
      w0 <- w0/sum(w0)
      w1 <- w1/sum(w1)
    }else{
      w0 <- (p0*q0)/sum(p0*q0)
      w1 <- (p1*q1)/sum(p1*q1)
    }

    # compute weights:
    w <- sqrt(w0*w1)

    # normalize weights:
    w <- w/sum(w)

    # compute index:
    res <- weighted.mean(x=sqrt(p1/p0), w=w) / weighted.mean(x=sqrt(p0/p1), w=w)

    # return output:
    return(res)

  },

  "geolaspeyres" = function(p1, q1, w1, p0, q0, w0){

    # derive normalized weights:
    if(missing(q0)){
      w <- w0/sum(w0)
    }else{
      w <- p0*q0/sum(p0*q0)
    }

    # compute index:
    res <- exp(weighted.mean(x=log(p1/p0), w=w))

    # return output:
    return(res)

  },

  "geopaasche" = function(p1, q1, w1, p0, q0, w0){

    # derive normalized weights:
    if(missing(q1)){
      w <- w1/sum(w1)
    }else{
      w <- p1*q1/sum(p1*q1)
    }

    # compute index:
    res <- exp(weighted.mean(x=log(p1/p0), w=w))

    # return output:
    return(res)

  },

  "geowalsh" = function(p1, q1, w1, p0, q0, w0){

    # set weights:
    if(missing(q0) | missing(q1)){
      w0 <- w0/sum(w0)
      w1 <- w1/sum(w1)
    }else{
      w0 <- (p0*q0)/sum(p0*q0)
      w1 <- (p1*q1)/sum(p1*q1)
    }

    # compute weights:
    w <- sqrt(w0*w1)

    # normalize weights:
    w <- w/sum(w)

    # compute index:
    res <- exp(weighted.mean(x=log(p1/p0), w=w))

    # return output:
    return(res)

  },

  "theil" = function(p1, q1, w1, p0, q0, w0){

    # set weights:
    if(missing(q0) | missing(q1)){
      w0 <- w0/sum(w0)
      w1 <- w1/sum(w1)
    }else{
      w0 <- (p0*q0)/sum(p0*q0)
      w1 <- (p1*q1)/sum(p1*q1)
    }

    # compute weights:
    w <- (w0*w1*(w0+w1)/2)^(1/3)

    # normalize weights:
    w <- w/sum(w)

    # compute index:
    res <- exp(weighted.mean(x=log(p1/p0), w=w))

    # return output:
    return(res)

  },

  "toernqvist" = function(p1, q1, w1, p0, q0, w0){

    # set weights:
    if(missing(q0) | missing(q1)){
      w0 <- w0/sum(w0)
      w1 <- w1/sum(w1)
    }else{
      w0 <- (p0*q0)/sum(p0*q0)
      w1 <- (p1*q1)/sum(p1*q1)
    }

    # compute weights:
    w <- 0.5*(w0/sum(w0) + w1/sum(w1))

    # normalize weights:
    w <- w/sum(w)

    # compute index:
    res <- exp(weighted.mean(x=log(p1/p0), w=w))

    # return output:
    return(res)

  },

  "svartia" = function(p1, q1, w1, p0, q0, w0){

    # set weights:
    if(missing(q0) | missing(q1)){
      w0 <- w0/sum(w0)
      w1 <- w1/sum(w1)
    }else{
      w0 <- (p0*q0)/sum(p0*q0)
      w1 <- (p1*q1)/sum(p1*q1)
    }

    # compute weights:
    w <- ifelse(abs(w1-w0)>1e-7, (w1-w0)/(log(w1)-log(w0)), w0)

    # normalize weights:
    w <- w/sum(w)

    # compute index:
    res <- exp(weighted.mean(x=log(p1/p0), w=w))

    # return output:
    return(res)

  },

  "uvalue" = function(p1, q1, w1=NULL, p0, q0, w0=NULL){

    # expenditures:
    e0 <- sum(p0*q0)
    e1 <- sum(p1*q1)

    # adjustment factors:
    z <- 1
    af0 <- sum(q0*z)
    af1 <- sum(q1*z)

    # index:
    return((e1/af1) / (e0/af0))

  },

  "banerjee" = function(p1, q1, w1=NULL, p0, q0, w0=NULL){

    # expenditures:
    e0 <- sum(p0*q0)
    e1 <- sum(p1*q1)

    # adjustment factors:
    z <- (p0+p1)/2
    af0 <- sum(q0*z)
    af1 <- sum(q1*z)

    # index:
    return((e1/af1) / (e0/af0))

  },

  "davies" = function(p1, q1, w1=NULL, p0, q0, w0=NULL){

    # expenditures:
    e0 <- sum(p0*q0)
    e1 <- sum(p1*q1)

    # adjustment factors:
    z <- sqrt(p0*p1)
    af0 <- sum(q0*z)
    af1 <- sum(q1*z)

    # index:
    return((e1/af1) / (e0/af0))

  },

  "lehr" = function(p1, q1, w1=NULL, p0, q0, w0=NULL){

    # expenditures:
    e0 <- sum(p0*q0)
    e1 <- sum(p1*q1)

    # adjustment factors:
    z <- (p0*q0+p1*q1)/(q0+q1)
    af0 <- sum(q0*z)
    af1 <- sum(q1*z)

    # index:
    return((e1/af1) / (e0/af0))

  }

)

Pmatched$cswd <- function(p1, q1=NULL, w1=NULL, p0, q0=NULL, w0=NULL){

  # compute carli indices:
  Pc <- Pmatched$carli(p1=p1, q1=q1, w1=w1, p0=p0, q0=q0, w0=w0)

  # compute harmonic indices:
  Ph <- Pmatched$harmonic(p1=p1, q1=q1, w1=w1, p0=p0, q0=q0, w0=w0)

  # compute index:
  return(sqrt(Pc*Ph))

}
Pmatched$fisher <- function(p1, q1, w1, p0, q0, w0){

  # compute laspeyres indices:
  Pl <- Pmatched$laspey(p1=p1, q1=q1, w1=w1, p0=p0, q0=q0, w0=w0)

  # compute paasche indices:
  Pp <- Pmatched$paasche(p1=p1, q1=q1, w1=w1, p0=p0, q0=q0, w0=w0)

  # compute index:
  return(sqrt(Pl*Pp))

}
Pmatched$drobisch <- function(p1, q1, w1, p0, q0, w0){

  # compute laspeyres indices:
  Pl <- Pmatched$laspey(p1=p1, q1=q1, w1=w1, p0=p0, q0=q0, w0=w0)

  # compute paasche indices:
  Pp <- Pmatched$paasche(p1=p1, q1=q1, w1=w1, p0=p0, q0=q0, w0=w0)

  # compute Fisher:
  return((Pl+Pp)/2)

}

# helper functions for comparison of multiple regions to
# one base region subsetting to intersecting items these
# will be called by index.pairs() and thus also geks(),
# where they are faster.
Pmatrix <- list(

  "jevons" = function(P, Q=NULL, W=NULL, base=1L, qbase=NULL){

    # compute index:
    res <- exp(colMeans(x=log(P/P[, base]), na.rm=TRUE))

    # set to NA when no intersecting prices were found:
    res[is.nan(res)] <- NA

    # re-transform logs:
    return(res)

  },

  "dutot" = function(P, Q=NULL, W=NULL, base=1L, qbase=NULL){

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

  },

  "carli" = function(P, Q=NULL, W=NULL, base=1L, qbase=NULL){

    # compute index:
    res <- colMeans(x=P/P[, base], na.rm=TRUE)

    # set to NA when no intersecting prices were found:
    res[is.nan(res)] <- NA

    # print output to console:
    return(res)

  },

  "harmonic" = function(P, Q=NULL, W=NULL, base=1L, qbase=NULL){

    # compute index:
    res <- 1/colMeans(x=1/(P/P[, base]), na.rm=TRUE)

    # set to NA when no intersecting prices were found:
    res[is.nan(res)] <- NA

    # print output to console:
    return(res)

  },

  "medgeworth" = function(P, Q, W=NULL, base=1L, qbase=NULL){

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

  },

  "lowe" = function(P, Q, W=NULL, base=1L, qbase=1L){

    # compute price ratios:
    R <- P/P[, base]

    # define weights:
    if(is.null(qbase)){
      W <- P[,base]*rowSums(Q, na.rm=TRUE)
    }else{
      W <- P[,base]*Q[,qbase]
    }

    # align dimensions of weighting matrix:
    W <- matrix(data=W, nrow=nrow(P), ncol=ncol(P), dimnames=dimnames(P))

    # set weights to NA when no intersection of prices:
    W[is.na(R)] <- NA

    # normalize weights:
    W <- W/colSums(x=W, na.rm=TRUE)[col(W)]

    # compute index:
    res <- colSums(x=W*R, na.rm=TRUE)

    # set to NA when no intersecting prices were found:
    res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
    # -> colSums()-function returns 0 when everything is NA

    # return output:
    return(res)

  },

  "young" = function(P, Q, W=NULL, base=1L, qbase=1L){

    # compute price ratios:
    R <- P/P[, base]

    # define weights:
    if(is.null(qbase)){
      W <- rowMeans(P, na.rm=TRUE)*rowSums(Q, na.rm=TRUE)
    }else{
      W <- P[,qbase]*Q[,qbase]
    }

    # align dimensions of weighting matrix:
    W <- matrix(data=W, nrow=nrow(P), ncol=ncol(P), dimnames=dimnames(P))

    # set weights to NA when no intersection of prices:
    W[is.na(R)] <- NA

    # normalize weights:
    W <- W/colSums(x=W, na.rm=TRUE)[col(W)]

    # compute index:
    res <- colSums(x=W*R, na.rm=TRUE)

    # set to NA when no intersecting prices were found:
    res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
    # -> colSums()-function returns 0 when everything is NA

    # return output:
    return(res)

  },

  "laspeyres" = function(P, Q, W, base=1L, qbase=NULL){

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

  },

  "paasche" = function(P, Q, W, base=1L, qbase=NULL){

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

  },

  "palgrave" = function(P, Q, W, base=1L, qbase=NULL){

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

  },

  "walsh" = function(P, Q, W, base=1L, qbase=NULL){

    # compute price ratios:
    R <- P/P[, base]

    # set weights:
    if(missing(Q)){
      Wbase <- matrix(data=W[,base], ncol=ncol(P), nrow=nrow(P))
      Wall <- W
    }else{
      Wbase <- matrix(data=P[,base]*Q[,base], ncol=ncol(P), nrow=nrow(P))
      Wall <- P*Q
    }

    # normalize weights:
    Wbase[is.na(R)] <- NA
    Wbase <- Wbase/colSums(Wbase, na.rm=TRUE)[col(R)]
    Wall[is.na(R)] <- NA
    Wall <- Wall/colSums(Wall, na.rm=TRUE)[col(R)]

    # compute new weights:
    W <- sqrt(Wbase*Wall)
    W <- W/colSums(x=W, na.rm=TRUE)[col(W)]

    # compute index:
    res <- colSums(x=W*sqrt(R), na.rm=TRUE)/colSums(x=W*sqrt(1/R), na.rm=TRUE)

    # set to NA when no intersecting prices were found:
    res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
    # -> colSums()-function returns 0 when everything is NA

    # print output to console:
    return(res)

  },

  "geolaspeyres" = function(P, Q, W, base=1L, qbase=NULL){

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

  },

  "geopaasche" = function(P, Q, W, base=1L, qbase=NULL){

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

  },

  "geowalsh" = function(P, Q, W, base=1L, qbase=NULL){

    # compute price ratios:
    R <- log(P/P[, base])

    # set weights:
    if(missing(Q)){
      Wbase <- matrix(data=W[,base], ncol=ncol(P), nrow=nrow(P))
      Wall <- W
    }else{
      Wbase <- matrix(data=P[,base]*Q[,base], ncol=ncol(P), nrow=nrow(P))
      Wall <- P*Q
    }

    # normalize weights:
    Wbase[is.na(R)] <- NA
    Wbase <- Wbase/colSums(Wbase, na.rm=TRUE)[col(R)]
    Wall[is.na(R)] <- NA
    Wall <- Wall/colSums(Wall, na.rm=TRUE)[col(R)]

    # compute new weights:
    W <- sqrt(Wbase*Wall)
    W <- W/colSums(x=W, na.rm=TRUE)[col(W)]

    # compute index:
    res <- exp(colSums(x=W*R, na.rm=TRUE))

    # set to NA when no intersecting prices were found:
    res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
    # -> colSums()-function returns 0 when everything is NA

    # print output to console:
    return(res)

  },

  "theil" = function(P, Q, W, base=1L, qbase=NULL){

    # compute price ratios:
    R <- log(P/P[, base])

    # set weights:
    if(missing(Q)){
      Wbase <- matrix(data=W[,base], ncol=ncol(P), nrow=nrow(P))
      Wall <- W
    }else{
      Wbase <- matrix(data=P[,base]*Q[,base], ncol=ncol(P), nrow=nrow(P))
      Wall <- P*Q
    }

    # normalize weights:
    Wbase[is.na(R)] <- NA
    Wbase <- Wbase/colSums(Wbase, na.rm=TRUE)[col(R)]
    Wall[is.na(R)] <- NA
    Wall <- Wall/colSums(Wall, na.rm=TRUE)[col(R)]

    # compute new weights:
    W <- (Wall*Wbase*(Wall+Wbase)/2)^(1/3)
    W <- W/colSums(x=W, na.rm=TRUE)[col(W)]

    # compute index:
    res <- exp(colSums(x=W*R, na.rm=TRUE))

    # set to NA when no intersecting prices were found:
    res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
    # -> colSums()-function returns 0 when everything is NA

    # print output to console:
    return(res)

  },

  "toernqvist" = function(P, Q, W, base=1L, qbase=NULL){

    # compute price ratios:
    R <- log(P/P[, base])

    # set weights:
    if(missing(Q)){
      Wbase <- matrix(data=W[,base], ncol=ncol(P), nrow=nrow(P))
      Wall <- W
    }else{
      Wbase <- matrix(data=P[,base]*Q[,base], ncol=ncol(P), nrow=nrow(P))
      Wall <- P*Q
    }

    # normalize weights:
    Wbase[is.na(R)] <- NA
    Wbase <- Wbase/colSums(Wbase, na.rm=TRUE)[col(R)]
    Wall[is.na(R)] <- NA
    Wall <- Wall/colSums(Wall, na.rm=TRUE)[col(R)]

    # compute new weights:
    W <- 0.5*(Wbase+Wall)
    W <- W/colSums(x=W, na.rm=TRUE)[col(W)]

    # compute index:
    res <- exp(colSums(x=W*R, na.rm=TRUE))

    # set to NA when no intersecting prices were found:
    res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
    # -> colSums()-function returns 0 when everything is NA

    # print output to console:
    return(res)

  },

  "svartia" = function(P, Q, W, base=1L, qbase=NULL){

    # compute logarithmic differences:
    R <- log(P/P[, base])

    # set weights:
    if(missing(Q)){
      Wbase <- matrix(data=W[,base], ncol=ncol(P), nrow=nrow(P))
      Wall <- W
    }else{
      Wbase <- matrix(data=P[,base]*Q[,base], ncol=ncol(P), nrow=nrow(P))
      Wall <- P*Q
    }

    # normalize weights:
    Wbase[is.na(R)] <- NA
    Wbase <- Wbase/colSums(Wbase, na.rm=TRUE)[col(R)]
    Wall[is.na(R)] <- NA
    Wall <- Wall/colSums(Wall, na.rm=TRUE)[col(R)]

    # compute new weights:
    W <- (Wall-Wbase)/(log(Wall)-log(Wbase))
    W <- ifelse(abs(Wall-Wbase)>1e-7, W, Wbase)
    W <- W/colSums(W, na.rm=TRUE)[col(W)]

    # compute index:
    res <- exp(colSums(x=W*R, na.rm=TRUE))

    # set to NA when no intersecting prices were found:
    res[is.nan(res) | colSums(x=!is.na(W*P), na.rm=FALSE)<=0] <- NA
    # -> colSums()-function returns 0 when everything is NA

    # print output to console:
    return(res)

  },

  "uvalue" = function(P, Q, W=NULL, base=1L, qbase=NULL){

    # price and quantity matrices for base:
    Pbase <- matrix(data=P[, base], ncol=ncol(P), nrow=nrow(P))
    Qbase <- matrix(data=Q[, base], ncol=ncol(Q), nrow=nrow(Q))

    # set NAs to avoid that calculations are based on different sets:
    P[is.na(Pbase*Qbase)] <- NA
    Q[is.na(Pbase*Qbase)] <- NA
    Pbase[is.na(P*Q)] <- NA
    Qbase[is.na(P*Q)] <- NA

    # expenditures:
    E0 <- colSums(Pbase*Qbase, na.rm=TRUE)
    E1 <- colSums(P*Q, na.rm=TRUE)

    # adjustment factors:
    Z <- 1
    AF0 <- colSums(Qbase*Z, na.rm=TRUE)
    AF1 <- colSums(Q*Z, na.rm=TRUE)

    # index:
    return((E1/AF1) / (E0/AF0))

  },

  "banerjee" = function(P, Q, W=NULL, base=1L, qbase=NULL){

    # price and quantity matrices for base:
    Pbase <- matrix(data=P[, base], ncol=ncol(P), nrow=nrow(P))
    Qbase <- matrix(data=Q[, base], ncol=ncol(Q), nrow=nrow(Q))

    # set NAs to avoid that calculations are based on different sets:
    P[is.na(Pbase*Qbase)] <- NA
    Q[is.na(Pbase*Qbase)] <- NA
    Pbase[is.na(P*Q)] <- NA
    Qbase[is.na(P*Q)] <- NA

    # expenditures:
    E0 <- colSums(Pbase*Qbase, na.rm=TRUE)
    E1 <- colSums(P*Q, na.rm=TRUE)

    # adjustment factors:
    Z <- (Pbase+P)/2
    AF0 <- colSums(Qbase*Z, na.rm=TRUE)
    AF1 <- colSums(Q*Z, na.rm=TRUE)

    # index:
    return((E1/AF1) / (E0/AF0))

  },

  "davies" = function(P, Q, W=NULL, base=1L, qbase=NULL){

    # price and quantity matrices for base:
    Pbase <- matrix(data=P[, base], ncol=ncol(P), nrow=nrow(P))
    Qbase <- matrix(data=Q[, base], ncol=ncol(Q), nrow=nrow(Q))

    # set NAs to avoid that calculations are based on different sets:
    P[is.na(Pbase*Qbase)] <- NA
    Q[is.na(Pbase*Qbase)] <- NA
    Pbase[is.na(P*Q)] <- NA
    Qbase[is.na(P*Q)] <- NA

    # expenditures:
    E0 <- colSums(Pbase*Qbase, na.rm=TRUE)
    E1 <- colSums(P*Q, na.rm=TRUE)

    # adjustment factors:
    Z <- sqrt(Pbase*P)
    AF0 <- colSums(Qbase*Z, na.rm=TRUE)
    AF1 <- colSums(Q*Z, na.rm=TRUE)

    # index:
    return((E1/AF1) / (E0/AF0))

  },

  "lehr" = function(P, Q, W=NULL, base=1L, qbase=NULL){

    # price and quantity matrices for base:
    Pbase <- matrix(data=P[, base], ncol=ncol(P), nrow=nrow(P))
    Qbase <- matrix(data=Q[, base], ncol=ncol(Q), nrow=nrow(Q))

    # set NAs to avoid that calculations are based on different sets:
    P[is.na(Pbase*Qbase)] <- NA
    Q[is.na(Pbase*Qbase)] <- NA
    Pbase[is.na(P*Q)] <- NA
    Qbase[is.na(P*Q)] <- NA

    # expenditures:
    E0 <- colSums(Pbase*Qbase, na.rm=TRUE)
    E1 <- colSums(P*Q, na.rm=TRUE)

    # adjustment factors:
    Z <- (Pbase*Qbase+P*Q)/(Qbase+Q)
    AF0 <- colSums(Qbase*Z, na.rm=TRUE)
    AF1 <- colSums(Q*Z, na.rm=TRUE)

    # index:
    return((E1/AF1) / (E0/AF0))

  }

)

Pmatrix$cswd <- function(P, Q=NULL, W=NULL, base=1L, qbase=NULL){

  # compute carli indices:
  Pc <- Pmatrix$carli(P=P, Q=Q, W=W, base=base)

  # compute harmonic indices:
  Ph <- Pmatrix$harmonic(P=P, Q=Q, W=W, base=base)

  # compute index:
  return(sqrt(Ph*Pc))

}
Pmatrix$fisher <- function(P, Q, W, base=1L, qbase=NULL){

  # compute laspeyres indices:
  Pl <- Pmatrix$laspey(P=P, Q=Q, W=W, base=base)

  # compute paasche indices:
  Pp <- Pmatrix$paasche(P=P, Q=Q, W=W, base=base)

  # compute Fisher:
  return(sqrt(Pl*Pp))

}
Pmatrix$drobisch <- function(P, Q, W, base=1L, qbase=NULL){

  # compute laspeyres indices:
  Pl <- Pmatrix$laspey(P=P, Q=Q, W=W, base=base)

  # compute paasche indices:
  Pp <- Pmatrix$paasche(P=P, Q=Q, W=W, base=base)

  # compute index:
  return((Pl+Pp)/2)

}

# main function to be called, used for input checking
# and data preparation, which is the same for each
# bilateral index
bilateral.index <- function(p, r, n, q, w=NULL, type, base=NULL, settings=list()){

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
    check.num(x=p, int=c(0, Inf))
    check.char(x=r)
    check.char(x=n)
    check.num(x=q, null.ok=TRUE, int=c(0, Inf))
    check.num(x=w, null.ok=TRUE, int=c(0, Inf))
    check.char(x=type, min.len=1, max.len=Inf, na.ok=FALSE)
    check.char(x=base, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE)
    check.lengths(x=r, y=n)
    check.lengths(x=r, y=p)
    check.lengths(x=r, y=q)
    check.lengths(x=r, y=w)

    # settings:
    check.log(x=settings$connect, min.len=1, max.len=1, na.ok=FALSE)
    check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
    check.char(x=settings$qbase, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE)

  }

  # allowed index types:
  type.vals <- pindices[type=="bilateral", ]

  # check against allowed index types:
  type <- match.arg(arg=type, choices=type.vals$name, several.ok=TRUE)

  # error handling for quantity and weights:
  if(settings$check.inputs){

    if(any(type%in%type.vals[uses_q==TRUE & uses_w==TRUE, name]) && is.null(q) && is.null(w)){
      stop(paste0("Non-valid input -> 'q' or 'w' required but both missing"), call.=FALSE)
    }

    if(any(type%in%type.vals[uses_q==TRUE & uses_w==FALSE, name]) && is.null(q)){
      stop(paste0("Non-valid input -> 'q' required but missing"), call.=FALSE)
    }

  }

  # initialize data:
  pdata <- arrange(p=p, r=r, n=n, q=q, w=w, base=base, settings=settings)

  # set base region:
  base <- set.base(r=pdata$r, base=base, null.ok=FALSE, settings=settings)

  # intersection with base region prices and weights:
  pdata <- merge(x=pdata, y=pdata[r==base,], by="n", all=FALSE, suffixes=c("","_base"))

  # compute lowe and young indices:
  out.lowe <- out.young <- NULL
  if(any(c("lowe","young")%in%type)){

    # set qbase-region:
    settings$qbase <- set.base(r=pdata$r, base=settings$qbase, null.ok=TRUE, qbase=TRUE, settings=settings)

    # add data:
    if(is.null(settings$qbase)){
      pdata.qbase <- pdata[, list("p"=mean(p),"q"=sum(q)), by="n"]
    }else{
      pdata.qbase <- pdata[r==settings$qbase, list(n,p,q)]
    }
    pdata <- merge(x=pdata, y=pdata.qbase, by="n", all=FALSE, suffixes=c("","_qbase"))

    if("lowe"%in%type){
      out.lowe <- pdata[, Pmatched$lowe(p1=p, p0=p_base, qb=q_qbase), by="r"]
    }

    if("young"%in%type){
      out.young <- pdata[, Pmatched$young(p1=p, p0=p_base, pb=p_qbase, qb=q_qbase), by="r"]
    }

  }

  # compute remaining indices:
  type.sub <- setdiff(type, c("lowe","young"))

  # set "matched index function" based on type:
  indexfn <- Pmatched[match(x=type.sub, table=names(Pmatched))]

  # loop over all indices:
  out <- vector(mode="list", length=length(type.sub))
  for(j in seq_along(type.sub)){

    # compute price index for each region:
    if(is.null(q)){
      out[[j]] <- pdata[, indexfn[[j]](p1=p, w1=w, p0=p_base, w0=w_base), by="r"]
    }else{
      out[[j]] <- pdata[, indexfn[[j]](p1=p, q1=q, p0=p_base, q0=q_base), by="r"]
    }

  }

  # gather data:
  names(out) <- type.sub
  out <- rbindlist(l=c(list("lowe"=out.lowe, "young"=out.young), out), use.names=TRUE, fill=TRUE, idcol="index")
  out <- as.matrix(x=dcast(data=out, formula=index~r, value.var="V1", fill=NA), rownames=TRUE)
  out <- out[match(x=type, table=rownames(out)), , drop=FALSE]

  # ensure that results contain all regions, also in cases
  # where no product matches were found. This is important
  # in cases of incomplete price data:
  r.lvl <- levels(factor(r))
  out <- out[ ,match(x=r.lvl, table=colnames(out)), drop=FALSE]
  colnames(out) <- r.lvl

  # print output to console:
  return(out)

}

# package functions:
jevons <- function(p, r, n, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, w=NULL, q=NULL, type="jevons", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
dutot <- function(p, r, n, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, w=NULL, q=NULL, type="dutot", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
carli <- function(p, r, n, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, w=NULL, q=NULL, type="carli", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
harmonic <- function(p, r, n, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, w=NULL, q=NULL, type="harmonic", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
cswd <- function(p, r, n, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, w=NULL, q=NULL, type="cswd", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
medgeworth <- function(p, r, n, q, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=NULL, type="medgeworth", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
uvalue <- function(p, r, n, q, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=NULL, type="uvalue", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
davies <- function(p, r, n, q, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=NULL, type="davies", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
banerjee <- function(p, r, n, q, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=NULL, type="banerjee", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
lehr <- function(p, r, n, q, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=NULL, type="lehr", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
lowe <- function(p, r, n, q, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=NULL, type="lowe", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
young <- function(p, r, n, q, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=NULL, type="young", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
laspeyres <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=w, type="laspey", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
paasche <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=w, type="paasche", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
palgrave <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=w, type="palgrave", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
fisher <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=w, type="fisher", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
drobisch <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=w, type="drobisch", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
walsh <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=w, type="walsh", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
geolaspeyres <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=w, type="geolaspey", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
geopaasche <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=w, type="geopaasche", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
geowalsh <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=w, type="geowalsh", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
theil <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=w, type="theil", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
toernqvist <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=w, type="toernq", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}
svartia <- function(p, r, n, q, w=NULL, base=NULL, settings=list()){

  res <- bilateral.index(r=r, n=n, p=p, q=q, w=w, type="svartia", base=base, settings=settings)
  res <- stats::setNames(as.vector(res), colnames(res))
  return(res)

}

# END
