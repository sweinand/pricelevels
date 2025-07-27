# START

# Title:  Price data characteristics
# Author: Sebastian Weinand
# Date:   22 July 2025

# flag if data is connected:
is.connected <- function(r, n){

  # input checks:
  check.char(x=r, min.len=0)
  check.char(x=n, min.len=0)
  check.lengths(x=r, y=n)

  # coerce input vectors to character:
  r <- as.character(r)
  n <- as.character(n)

  # drop missing values:
  nas <- stats::complete.cases(r, n)
  r <- r[nas]
  n <- n[nas]

  # unique regions and products:
  R <- unique(r)
  N <- unique(n)

  # regions in which first product is present:
  R1 <- unique(r[n==n[1]])
  rdiff <- length(R)-length(R1)

  # loop until there are no additions to the set of regions:
  while(length(R1)<length(R) & rdiff>0L){

    # regions in which first and all other products are present:
    R1tmp <- unique(r[n%in%n[r%in%R1]])

    # check if new regions were added:
    rdiff <- length(R1tmp)-length(R1)

    # new set of regions:
    R1 <- R1tmp

  }

  # if all regions are present in R1, the data is connected;
  # otherwise, there are multiple, separated blocks:
  out <- all(R%in%R1) & length(R)>0L
  # if there is only one region or only one product,
  # the data is always connected

  # print output to console:
  return(out)

}

# divide data into blocks of connected regions:
neighbors <- function(r, n, simplify=FALSE){

  # input checks:
  check.char(x=r, min.len=0)
  check.char(x=n, min.len=0)
  check.log(x=simplify, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
  check.lengths(x=r, y=n)

  # coerce input vectors to character:
  r <- as.character(r)
  n <- as.character(n)

  # create copy of initial region vector:
  r0 <- r

  # drop missing values:
  nas <- stats::complete.cases(r, n)
  r <- r[nas]
  n <- n[nas]

  # unique regions and products:
  R <- unique(r)
  N <- unique(n)

  # set starting parameters:
  j <- 1 # iteration of loop
  Rtmp <- NULL # regions in j-th group
  out <- list() # container to store regional groups

  # loop as long as regions are present in the vector:
  while(length(r) > 0){

    # regions in which first product is present:
    R1 <- unique(r[n==n[1]])
    rdiff <- length(R)-length(R1)

    # loop until there are no additions to the set of regions:
    while(length(R1)<unique(length(r)) & rdiff>0L){

      # regions in which first and all other products are present:
      R1tmp <- unique(r[n%in%n[r%in%R1]])

      # check if new regions were added:
      rdiff <- length(R1tmp)-length(R1)

      # new set of regions:
      R1 <- R1tmp

    }

    # store neighboring regions:
    out[[j]] <- R1
    # if there is only one region or only one product,
    # the data is always connected

    # regions which are processed, i.e. already assigned to a group and
    # therefore not available for further processing:
    Rtmp <- c(Rtmp, R1)

    # subset to remaining regions an products:
    idx <- r%in%Rtmp # all regions when starting
    r <- r[!idx]
    n <- n[!idx]

    # increment by one:
    j <- j+1

  }

  # divide into groups of connected regions:
  if(simplify){

    # store groups of neighboring regions as named vector:
    ngbs <- unlist(x=out, use.names=FALSE)
    if(length(ngbs)>0L) names(ngbs) <- rep(x=1:length(out), times=lengths(out))

    # remove NAs in order to prevent matching between NAs:
    ngbs <- ngbs[!is.na(ngbs)]

    # match groups of neighboring regions with individual regions:
    out <- names(ngbs)[match(x=r0, table=ngbs)]
    out <- as.integer(out)

  }

  # return output:
  return(out)

}

# connect data:
connect <- function(r, n){

  # @description:
  # simple wrapper of neighbors() for connecting
  # price data by keeping the only the connected
  # observations with maximum number of observations

  # @value:
  # logical indicating the observations to be kept

  # divide into connected region groups:
  ngbs <- neighbors(r=r, n=n, simplify=TRUE)

  # frequency count of observations by group:
  ngbs.tab <- table(ngbs, useNA="no")

  # logical indicating the observations to be kept:
  return(ngbs%in%names(which.max(ngbs.tab)))

}

# compute number of index pairs:
pairs <- function(r, n){

  # input checks:
  check.char(x=r, min.len=0)
  check.char(x=n, min.len=0)
  check.lengths(x=r, y=n)

  # coerce input vectors to character:
  r <- as.character(r)
  n <- as.character(n)

  # drop missing values:
  nas <- stats::complete.cases(r, n)
  r <- r[nas]
  n <- n[nas]

  # regional occurrences by product:
  X0 <- as.matrix(x=table(n, r, useNA="no"), drop=FALSE)

  # matrix of direct region pairs:
  X1 <- crossprod(x=X0)

  # remove redundant pairs (e.g., A to B versus B to A, or A to A):
  X1[lower.tri(X1, diag=FALSE)] <- 0L
  diag(X1) <- 0L

  # compute number of region pairs:
  return(sum(X1>0L))

}

# compute number of gaps:
gaps <- function(r, n, relative=TRUE){

  # input checks:
  check.char(x=r, min.len=0)
  check.char(x=n, min.len=0)
  check.log(x=relative, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
  check.lengths(x=r, y=n)

  # coerce input vectors to character:
  r <- as.character(r)
  n <- as.character(n)

  # number of regions by product:
  freqtab <- as.matrix(table(n, r, useNA="no"), drop=FALSE)

  # subset to products which are priced in multiple regions:
  freqtab <- freqtab[rowSums(freqtab)>1L, , drop=FALSE]

  # compute gaps in price data ignoring duplicated observations:
  if(nrow(freqtab)>0L){
    if(relative){
      out <- 1-sum(freqtab>0L)/prod(dim(freqtab))
    }else{
      out <- prod(dim(freqtab))-sum(freqtab>0L)
    }
  }else{
    out <- NA_real_
  }

  # numeric vector:
  out <- as.vector(x=out, mode="numeric")

  # return output:
  return(out)

}

# summarize data properties:
properties <- function(r, n){

  # input checks:
  check.char(x=r, min.len=0)
  check.char(x=n, min.len=0)
  check.lengths(x=r, y=n)

  # coerce input vectors to character:
  dt <- data.table("r"=as.character(r), "n"=as.character(n))

  # drop missing values:
  dt <- dt[stats::complete.cases(r, n), ]

  # drop duplicated observations in line with the data
  # processing of index formulas:
  dt <- unique(x=dt, by=c("r","n"))

  # add region group identifier:
  dt[, "group" := neighbors(r=r, n=n, simplify=TRUE)]

  # unique regions, share of gaps and number of region pairs per group:
  out <- dt[, list("regions"=list(unique(r)), "gaps"=gaps(r=r, n=n), "pairs"=pairs(r=r, n=n)), by="group"]

  # add group size:
  regions <- NULL # avoid global bindings note when checking
  out[, "size" := lengths(regions)]

  # add number of unique products and observations with interregional content:
  out <- merge(x=out,
               y=dt[, .N, by=c("group","n")][N>1L, list("nprods"=.N, "nobs"=sum(N)), by="group"],
               by="group", all.x=TRUE, sort=FALSE)

  # adjust values:
  out[pairs<1L, c("nprods","nobs") := 0L]

  # set column order and key:
  neword <- c("group","size","regions","pairs","nprods","nobs","gaps")
  data.table::setcolorder(x=out, neworder=neword)
  data.table::setindex(x=out, NULL)
  data.table::setkeyv(x=out, cols="group")

  # return output:
  return(out[])

}

# END
