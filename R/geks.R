# START

# Title:  Bilateral index pairs and GEKS method
# Author: Sebastian Weinand
# Date:   2023-08-27

# compute bilateral index pairs:
index.pairs <- function(p, r, n, q=NULL, w=NULL, type="jevons", all.pairs=TRUE, as.dt=FALSE){

  # check 'type':
  .check.char(x=type, min.len=1, max.len=1, na.ok=FALSE)

  # allowed index types:
  type.vals <- c("jevons", "carli", "dutot", "harmonic", "toernq", "laspey", "paasche", "walsh", "fisher")

  # check against allowed index types:
  type <- match.arg(arg=type, choices=type.vals)

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
  .check.log(x=all.pairs, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
  .check.log(x=as.dt, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
  .check.lengths(x=r, y=n)
  .check.lengths(x=r, y=p)
  .check.lengths(x=r, y=z)

  # gather in data.table:
  pdata <- data.table("r"=factor(r), "n"=factor(n), "p"=as.numeric(p), "z"=as.numeric(z))

  # convert prices into matrix:
  P <- as.matrix(
    x=dcast(data=pdata, formula=n~r, fun.aggregate=mean, value.var="p", fill=NA),
    rownames="n")

  # convert weights or quantities into matrix, where duplicated weights
  # are averaged, duplicated quantites added up:
  Z <- as.matrix(
    x=dcast(
      data=pdata,
      formula=n~r,
      fun.aggregate=function(j){if(q.avail) sum(j) else mean(j)},
      value.var="z",
      fill=NA),
    rownames="n")

  # number of regions or time periods:
  R <- ncol(P)

  # container to store price levels:
  res <- matrix(data=NA_real_, nrow=R, ncol=R, dimnames=list(colnames(P), colnames(P)))

  # slow, but complete or quick, but only non-redundant:
  if(!all.pairs){

    if(q.avail){
      for(i in 1:R){
        idx <- i:R # tighten column selection in each iteration
        res[i, idx] <- index_func(P=P[, idx, drop=FALSE], Q=Z[, idx, drop=FALSE], base=1L)
      }
    }else{
      for(i in 1:R){
        idx <- i:R # tighten column selection in each iteration
        res[i, idx] <- index_func(P=P[, idx, drop=FALSE], W=Z[, idx, drop=FALSE], base=1L)
      }
    }

  }else{

    if(q.avail){
      for(i in 1:R) res[i, ] <- index_func(P=P, Q=Z, base=i)
    }else{
      for(i in 1:R) res[i, ] <- index_func(P=P, W=Z, base=i)
    }

  }

  # convert to dataframe:
  if(as.dt){

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
geks <- function(p, r, n, q=NULL, w=NULL, type="jevons", base=NULL){

  # input checks:
  .check.char(x=base, min.len=1, max.len=1, miss.ok=TRUE, null.ok=TRUE, na.ok=FALSE)
  # -> input checks of other arguments are performed in index_pairs()

  # compute bilateral price index numbers:
  pmat <- index.pairs(r=r, n=n, p=p, q=q, w=w, type=type, all.pairs=TRUE, as.dt=FALSE)

  # define position of base region in matrix:
  if(is.null(base) | missing(base)){
    idx <- 1L
  }else{
    if(base%in%colnames(pmat)){
      idx <- which(base == colnames(pmat))
    }else{
      idx <- 1L
      warning(paste("Base region not found and reset to", colnames(pmat)[idx]))
    }
  }

  # compute multilateral GEKS index numbers:
  out <- colMeans(x=log(pmat*pmat[idx,]), na.rm=TRUE)

  # scale if necessary:
  if(is.null(base)){out <- scale(x=out, center=TRUE, scale=FALSE)[,1]}

  # unlog price levels:
  return(exp(out))

}

# -> IDEA:
#    o GEKS: two-step procedure. Therefore, estimated standard errors (SE) are
#      zero in the second (regression) step when prices are fully available
#    o CPD: one step procedure. Therefore, SE are always greater than zero.
#    -> split the SE of CPD in two components, similar to GEKS: (i) SE-component
#       for the aggregation of prices into index numbers, and (ii) SE-component
#       for uncertainty due to gaps in the price data

# END
