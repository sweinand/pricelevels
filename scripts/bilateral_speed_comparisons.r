# START

# Title:    Calculation of bilateral indices and pais
# Subtitle: Speed comparisons
# Author:   Sebastian Weinand
# Date:     28 August 2023

# the package contains two versions of each index method:
# (V1) one using already matched item prices of exactly two
#   regions. Examples are .jevons2() or laspey2(). These
#   functions are currently called by the corresponding
#   bilateral index function, e.g., jevons() or laspey().
# (V2) one using matrix computations to intersect among items
#   of all regions available. Examples are .jevons() or
#   .laspey(). These functions are currently called by
#   index.pairs() and thus also by geks().
# Two versions of each function is actually not advantageous,
# but comes with speed improvements as shown in the following.

# load packages:
library(data.table)
library(spin)


# Bilateral index ---------------------------------------------------------


# compare package implementation following V1 against
# an implementation of jevons() and laspey() following V2

# sample data:
set.seed(1)
dt <- prices(R=500, N=1000, gaps=0.1)
dt[, "quantity" := sample(x=50:1000, size=.N, replace=TRUE)]
dt[, "weight" := price*quantity/sum(price*quantity), by="region"]
setnames(dt, c("r","n","p","q","w"))

# matrix version of jevons:
jevons.mat <- function(p, r, n, base){

  # reshape data to matrices:
  P <- as.matrix(
    x=data.table::dcast(data=dt, formula=n~r, fun.aggregate=mean, value.var="p", fill=NA),
    rownames="n")

  # output:
  return(spin:::.jevons(P=P, Q=NULL))

}

system.time(PJ1 <- dt[, jevons.mat(p=p, r=r, n=n, base="r001")])
system.time(PJ2 <- dt[, spin::jevons(p=p, r=r, n=n, base="r001")])
all.equal(PJ1, PJ2)

# matrix version of laspey:
laspey.mat <- function(p, r, n, q, w, base){

  # reshape data to matrices:
  P <- as.matrix(
    x=data.table::dcast(data=dt, formula=n~r, fun.aggregate=mean, value.var="p", fill=NA),
    rownames="n")

  Q <- as.matrix(
    x=data.table::dcast(data=dt, formula=n~r, fun.aggregate=sum, value.var="q", fill=NA),
    rownames="n")

  W <- as.matrix(
    x=data.table::dcast(data=dt, formula=n~r, fun.aggregate=mean, value.var="w", fill=NA),
    rownames="n")

  # output:
  if(!is.null(Q)){
    return(spin:::.laspey(P=P, Q=Q, W=NULL))
  }else{
    return(spin:::.laspey(P=P, Q=NULL, W=W))
  }

}

system.time(PL1 <- dt[, laspey.mat(p=p, r=r, n=n, q=q, base="r001")])
system.time(PL2 <- dt[, spin::laspey(p=p, r=r, n=n, q=q, base="r001")])
all.equal(PL1, PL2)

system.time(PL3 <- dt[, laspey.mat(p=p, r=r, n=n, w=w, base="r001")])
system.time(PL4 <- dt[, spin::laspey(p=p, r=r, n=n, w=w, base="r001")])
all.equal(PL3, PL4)

all.equal(PL1, PL3)


# Index pairs -------------------------------------------------------------


# compare package implementation following V2 against
# an implementation of index.pairs() following V1
# -> the higher the number of regions, the more computational
#    time is needed by index.pairs2() due to the merging of
#    data in each iteration

# using the index formulas jevons2(),...
index.pairs2 <- function(p, q, r, n, w=NULL, type="jevons", all.pairs=TRUE, as.dt=FALSE){

  # allowed index types:
  type.vals <- c("jevons", "carli", "dutot", "harmonic", "toernq", "laspey", "paasche", "walsh", "fisher")

  # check against allowed index types:
  type <- match.arg(arg=type, choices=type.vals)

  # set index function based on type:
  if(type=="jevons"){index_func <- spin:::.jevons2}
  if(type=="carli"){index_func <- spin:::.carli2}
  if(type=="dutot"){index_func <- spin:::.dutot2}
  if(type=="harmonic"){index_func <- spin:::.harmonic2}
  if(type=="toernq"){index_func <- spin:::.toernq2}
  if(type=="laspey"){index_func <- spin:::.laspey2}
  if(type=="paasche"){index_func <- spin:::.paasche2}
  if(type=="walsh"){index_func <- spin:::.walsh2}
  if(type=="fisher"){index_func <- spin:::.fisher2}

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

  # compute all possible index pairs:
  out <- vector(mode="list", length=length(r.lvl))
  for(j in seq_along(r.lvl)){

    # intersection with base region prices and weights:
    pdata.tmp <- merge(x=pdata, y=pdata[r==r.lvl[j],], by="n", all=FALSE, suffixes=c("","_base"))

    # compute price index for each region:
    if(q.avail){
      out[[j]] <- pdata.tmp[, list("base"=r.lvl[j], "index"=index_func(p1=p, q1=z, p0=p_base, q0=z_base)), by="r"]
    }else{
      out[[j]] <- pdata.tmp[, list("base"=r.lvl[j], "index"=index_func(p1=p, w1=z, p0=p_base, w0=z_base)), by="r"]
    }

  }

  # rbind:
  out <- rbindlist(l=out, use.names=TRUE, fill=TRUE)

  # convert to dataframe:
  if(as.dt){

    # drop missing values:
    out <- out[!is.na(index),]

    # set column order:
    setcolorder(x=out, neworder=c("r", "base", "index"))

    # set key:
    setkeyv(x=out, cols=c("r", "base"))

  }else{

    out <- as.matrix(
      x=dcast(data=out, formula=base~r, value.var="index"),
      rownames="base")

  }

  # print output to console:
  return(out)

}

# sample data:
set.seed(1)
dt <- prices(R=200, N=1000, gaps=0.1)
dt[, "quantity" := sample(x=50:1000, size=.N, replace=TRUE)]
dt[, "weight" := price*quantity/sum(price*quantity), by="region"]
setnames(dt, c("r","n","p","q","w"))

# jevons:
system.time(PJ1 <- dt[, index.pairs2(p=p, r=r, n=n, type="jevons")])
system.time(PJ2 <- dt[, spin::index.pairs(p=p, r=r, n=n, type="jevons")])
all.equal(PJ1, PJ2)

# laspeyres:
system.time(PL1 <- dt[, index.pairs2(p=p, r=r, n=n, q=q, type="laspey")])
system.time(PL2 <- dt[, spin::index.pairs(p=p, r=r, n=n, q=q, type="laspey")])
all.equal(PL1, PL2)

system.time(PL3 <- dt[, index.pairs2(p=p, r=r, n=n, w=w, type="laspey")])
system.time(PL4 <- dt[, spin::index.pairs(p=p, r=r, n=n, w=w, type="laspey")])
all.equal(PL3, PL4)

all.equal(PL1, PL3)

# END
