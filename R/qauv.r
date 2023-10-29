# START

# Title:    Quality-adjusted unit value methods
# Author:   Sebastian Weinand
# Date:     29 October 2023

# print output for quality-adjusted unit value indices:
print.qauv <- function(x){
  print(x$par)
  invisible(x)
}

# solve interrelated equations:
.solveq <- function(p, r, n, q, w, P.FUN, v.FUN, base=NULL, simplify=TRUE, settings=list()){

  # set default if missing:
  if(missing(q)) q <- NULL

  # input checks:
  .check.num(x=p, int=c(0, Inf))
  .check.char(x=r)
  .check.char(x=n)
  .check.num(x=w, null.ok=TRUE, int=c(0, Inf))
  .check.num(x=q, null.ok=TRUE, int=c(0, Inf))
  .check.char(x=base, miss.ok=TRUE, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE)
  .check.log(x=simplify, miss.ok=TRUE, min.len=1, max.len=1, na.ok=FALSE)
  .check.lengths(x=r, y=n)
  .check.lengths(x=r, y=p)
  .check.lengths(x=r, y=q)
  .check.lengths(x=r, y=w)

  # define settings:
  if(is.null(settings$method)) settings$method <- "iterative"
  if(is.null(settings$tol)) settings$tol <- 1e-9
  if(is.null(settings$max.iter)) settings$max.iter <- 99L

  # check settings:
  .check.num(x=settings$tol, min.len=1, max.len=1, na.ok=FALSE, int=c(0,Inf))
  .check.num(x=settings$max.iter, min.len=1, max.len=1, na.ok=FALSE, int=c(0,Inf))

  # set quantities or weights if available:
  if(is.null(q) && is.null(w)){
    stop(paste0("Non-valid input for type -> 'q' or 'w' required"))
  }else{
    if(is.null(q)) z <- w else z <- q
  }

  # gather in data.table:
  pdata <- data.table("r"=as.character(r), "n"=as.character(n), "p"=as.numeric(p), "z"=as.numeric(z))

  # if both q and w are provided, q will be checked:
  pdata <- pdata[complete.cases(r, n, p, z), ]

  # stop if no observations left:
  if(nrow(pdata)<=0L){
    stop("No complete cases available. All data pairs contain at least one NA.", call.=FALSE)
  }

  # stop if non-connected data:
  if(pdata[, !spin::is.connected(r=r, n=n)]){
    stop("Regions not connected -> see spin::neighbors() for details.", call.=FALSE)
  }

  # check for duplicated entries:
  if(anyDuplicated(x=pdata, by=c("r","n"))>0L){

    # average duplicated prices and weights, sum duplicated quantities:
    if(is.null(q)){
      pdata <- pdata[, list("p"=mean(p), "z"=mean(z)), by=c("r","n")]
    }else{
      pdata <- pdata[, list("p"=mean(p), "z"=sum(z)), by=c("r","n")]
    }

    # print warning:
    warning("Duplicated observations found and aggregated.", call.=FALSE)

  }

  # compute expenditure share weights for each region:
  if(!is.null(q)){
    pdata[, "w" := (p*z)/sum(p*z, na.rm=TRUE), by="r"]
  }else{
    pdata[, "w" := z]
  }

  # set base region:
  if(!base%in%pdata$r && !is.null(base)){
    # reset base region and print warning:
    base <- names(which.max(table(pdata$r)))[1]
    warning(paste("Base region not found and reset to", base), call.=FALSE)
  }

  # Diewert (1999) solution for geary-khamis:
  if(settings$method=="solve"){

    # define matrices:
    Q <- pdata[, tapply(X=q, INDEX=list(r, n), FUN=mean, default=0)]
    E <- pdata[, tapply(X=p*q, INDEX=list(r, n), FUN=mean, default=0)]
    E <- E/rowSums(E)
    C <- diag(1/colSums(Q), ncol=ncol(Q), nrow=ncol(Q))%*%t(E)%*%Q

    # introduce normalization:
    z <- c(1, rep(0, ncol(C)-1))
    R <- matrix(data=0, ncol=ncol(C), nrow=ncol(C))
    R[1,] <- 1

    # solve for b:
    b <- as.vector(solve(diag(x=1, ncol=ncol(C), nrow=nrow(C))-C+R)%*%z)
    names(b) <- colnames(C)

    # compute price levels:
    Ptmp <- pdata[, P.FUN(p=p, q=q, r=r, v=b[match(x=n, names(b))])]

  }

  # iterative search procedure:
  if(settings$method=="iterative"){

    Ptmp <- rep(1, nrow(pdata))
    i <- 0
    check <- TRUE
    while(check && i<=settings$max.iter){

      # compute price levels:
      Ptmp0 <- Ptmp
      vtmp <- pdata[, v.FUN(p=p, q=q, w=w, n=n, P=Ptmp)]
      Ptmp <- pdata[, P.FUN(p=p, q=q, w=w, r=r, v=vtmp)]

      # check differences to previous price levels:
      check <- any(abs(Ptmp-Ptmp0)>settings$tol)
      i <- i+1

    }

    # print warning if maximum iterations exceeded:
    if(check && i>settings$max.iter){
      warning("Iterative procedure stopped at 'max.iter' without reaching convergence.", call.=FALSE)
    }

  }

  # price levels at convergence:
  P <- split(x=Ptmp, f=names(Ptmp))
  P <- sapply(X=P, "[[", 1L)

  # normalization:
  if(is.null(base)){
    P <- P/mean(P)
  }else{
    P <- P/P[names(P)==base]
  }

  if(simplify){

    res <- P

  }else{

    # average product prices using normalized price levels:
    v <- pdata[, v.FUN(p=p, q=q, w=w, n=n, P=P[match(x=r, table=names(P))])]
    v <- split(x=v, f=names(v))
    v <- sapply(X=v, "[[", 1L)

    # define output:
    res <- list("par"=c("v"=v, "P"=P))
    if(settings$method=="iterative"){
      Ptol <- Ptmp-Ptmp0
      Ptol <- sapply(X=split(x=Ptol, f=names(Ptol)), "[[", 1L)
      res <- c(res, "niter"=i, list("tol"=c("P"=Ptol)))
    }
    res <- structure(res, class="qauv")

  }

  # return output:
  return(res)

}

# geary-khamis:
gk <- function(p, r, n, q, base=NULL, simplify=TRUE, settings=list()){

  # see CPI Manual (2020, p. 448)
  # https://www.ilo.org/wcmsp5/groups/public/---dgreports/---stat/documents/publication/wcms_761444.pdf

  # quantities must be there for geary-khamis, no weights:
  .check.num(x=q, miss.ok=FALSE, null.ok=FALSE, int=c(0, Inf))

  # match argument, if NULL first possible choice is selected:
  settings$method <- match.arg(arg=settings$method, choices=c("iterative","solve"))

  # definition of average product prices:
  v.def <- function(p, q, w=NULL, n, P){
    res <- ave(x=q*p/P, n, FUN=sum) / ave(x=q, n, FUN=sum)
    names(res) <- n
    return(res)
  }

  # definition of price levels:
  P.def <- function(p, q, w=NULL, r, v){
    res <- ave(x=p*q, r, FUN=sum) / ave(x=v*q, r, FUN=sum)
    names(res) <- r
    return(res)
  }

  # compute index:
  res <- .solveq(p=p, r=r, n=n, q=q, w=NULL, P.FUN=P.def, v.FUN=v.def, base=base, simplify=simplify, settings=settings)

  # return output:
  return(res)

}

# ikle-dikhanov-balk:
idb <- function(p, r, n, q, w=NULL, base=NULL, simplify=TRUE, settings=list()){

  # see CPI Manual (2020, p. 448)
  # https://www.ilo.org/wcmsp5/groups/public/---dgreports/---stat/documents/publication/wcms_761444.pdf

  # match argument:
  settings$method <- match.arg(arg=settings$method, choices=c("iterative"))

  # definition of average product prices:
  v.def <- function(p, q=NULL, w, n, P){
    res <- 1 / (ave(x=w*(P/p), n, FUN=sum) / ave(x=w, n, FUN=sum))
    names(res) <- n
    return(res)
  }

  # definition of price levels:
  P.def <- function(p, q=NULL, w, r, v){
    res <- 1/ave(x=w*(v/p), r, FUN=sum)
    # res <- ave(x=p*q, r, FUN=sum) / ave(x=v*q, r, FUN=sum)
    names(res) <- r
    return(res)
  }

  # compute index:
  res <- .solveq(p=p, r=r, n=n, q=q, w=w, P.FUN=P.def, v.FUN=v.def, base=base, simplify=simplify, settings=settings)

  # return output:
  return(res)

}

# rao:
rao <- function(p, r, n, q, w=NULL, base=NULL, simplify=TRUE, settings=list()){

  # see Hajargasht (2022, p. 612)
  # https://link.springer.com/book/10.1007/978-981-19-2023-3

  # match argument:
  settings$method <- match.arg(arg=settings$method, choices=c("iterative"))

  # definition of average product prices:
  v.def <- function(p, q=NULL, w, n, P){
    res <- exp(ave(x=w*log(p/P), n, FUN=sum))^(1/ave(x=w, n, FUN=sum))
    names(res) <- n
    return(res)
  }

  # definition of price levels:
  P.def <- function(p, q=NULL, w, r, v){
    res <- exp(ave(x=w*log(p/v), r, FUN=sum))
    names(res) <- r
    return(res)
  }

  # compute index:
  res <- .solveq(p=p, r=r, n=n, q=q, w=w, P.FUN=P.def, v.FUN=v.def, base=base, simplify=simplify, settings=settings)

  # return output:
  return(res)

}

# geradi:
geradi <- function(p, r, n, q, w=NULL, base=NULL, simplify=TRUE, settings=list()){

  # see Balk (1996, p. 208)
  # https://www.scb.se/contentassets/ca21efb41fee47d293bbee5bf7be7fb3/a-comparison-of-ten-methods-for-multilateral-international-price-and-volume-comparison.pdf

  # match argument:
  settings$method <- match.arg(arg=settings$method, choices=c("iterative"))

  # definition of average product prices:
  v.def <- function(p, q=NULL, w=NULL, n, P){
    res <- exp(ave(x=log(p), n, FUN=mean))
    names(res) <- n
    return(res)
  }

  # definition of price levels:
  P.def <- function(p, q=NULL, w, r, v){
    res <- 1/ave(x=w*(v/p), r, FUN=sum)
    # res <- ave(x=p*q, r, FUN=sum) / ave(x=v*q, r, FUN=sum)
    names(res) <- r
    return(res)
  }

  # compute index:
  res <- .solveq(p=p, r=r, n=n, q=q, w=w, P.FUN=P.def, v.FUN=v.def, base=base, simplify=simplify, settings=settings)

  # return output:
  return(res)

}

# END
