# START

# Title:    Multilateral systems of equations
# Author:   Sebastian Weinand
# Date:     15 January 2024

# print output for class 'multeq':
print.multeq <- function(x, ...){
  print(x$par)
  invisible(x)
}

# solve interrelated equations:
solvemulteq <- function(p, r, n, q, w, base=NULL, simplify=TRUE, P.FUN, v.FUN, type, settings=list()){

  # set default if missing:
  if(missing(q)) q <- NULL
  if(missing(w)) w <- NULL

  # set default settings if missing:
  if(is.null(settings$connect)) settings$connect <- TRUE
  if(is.null(settings$chatty)) settings$chatty <- TRUE
  if(is.null(settings$solve)) settings$solve <- "iterative"
  if(is.null(settings$tol)) settings$tol <- 1e-9
  if(is.null(settings$max.iter)) settings$max.iter <- 99L

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
    .check.num(x=w, null.ok=TRUE, int=c(0, Inf))
    .check.num(x=q, null.ok=TRUE, int=c(0, Inf))
    .check.char(x=base, miss.ok=TRUE, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE)
    .check.char(x=type, min.len=1, max.len=1, na.ok=FALSE)
    .check.log(x=simplify, miss.ok=TRUE, min.len=1, max.len=1, na.ok=FALSE)
    .check.lengths(x=r, y=n)
    .check.lengths(x=r, y=p)
    .check.lengths(x=r, y=q)
    .check.lengths(x=r, y=w)

    # settings:
    .check.log(x=settings$connect, min.len=1, max.len=1, na.ok=FALSE)
    .check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
    .check.char(x=settings$solve, min.len=1, max.len=1, na.ok=FALSE)
    .check.num(x=settings$tol, min.len=1, max.len=1, na.ok=FALSE, int=c(0,Inf))
    .check.num(x=settings$max.iter, min.len=1, max.len=1, na.ok=FALSE, int=c(0,Inf))

  }

  # set type:
  type <- match.arg(arg=type, choices=list("gk","idb","gerardi","rao"))

  # set solve method:
  if(type=="gk"){
    settings$solve <- match.arg(arg=settings$solve, choices=c("iterative","matrix"))
  }else{
    settings$solve <- match.arg(arg=settings$solve, choices="iterative")
  }

  # error handling for quantity and weights:
  if(settings$check.inputs && is.null(q) && is.null(w)){
    stop(paste0("Non-valid input for type='", type, "' -> 'q' or 'w' required"), call.=FALSE)
  }
  if(settings$check.inputs && type=="gk" && is.null(q)){
    stop(paste0("Non-valid input for type='", type, "' -> 'q' required"), call.=FALSE)
  }

  # initialize data:
  pdata <- spin:::arrange(p=p, r=r, n=n, q=q, w=w, base=base, settings=settings)

  # set base region:
  base <- spin:::set.base(r=pdata$r, base=base, null.ok=TRUE, settings=settings)

  # Diewert (1999) solution for geary-khamis:
  if(type=="gk" && settings$solve=="matrix"){

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
  if(settings$solve=="iterative"){

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
    if(check && i>settings$max.iter && settings$chatty){
      warning("Iterative procedure stopped at 'max.iter' without reaching convergence.", call.=FALSE)
    }

  }

  # price levels at convergence:
  P <- split(x=Ptmp, f=names(Ptmp))
  P <- sapply(X=P, "[[", 1L)

  # normalization:
  if(is.null(base)){
    if(type=="rao"){
      P <- P/exp(mean(log(P)))
    }else{
      P <- P/mean(P)
    }
  }else{
    P <- P/P[names(P)==base]
  }

  if(simplify){

    # match to initial ordering:
    r.lvl <- levels(factor(r))
    res <- P[match(x=r.lvl, table=names(P))]
    names(res) <- r.lvl

  }else{

    # average product prices using normalized price levels:
    v <- pdata[, v.FUN(p=p, q=q, w=w, n=n, P=P[match(x=r, table=names(P))])]
    v <- split(x=v, f=names(v))
    v <- sapply(X=v, "[[", 1L)

    # define output:
    res <- list("par"=c("v"=v, "P"=P))
    if(settings$solve=="iterative"){
      Ptol <- Ptmp-Ptmp0
      Ptol <- sapply(X=split(x=Ptol, f=names(Ptol)), "[[", 1L)
      res <- c(res, "niter"=i, list("tol"=c("P"=Ptol)))
    }
    res <- structure(res, class="multeq")

  }

  # return output:
  return(res)

}

# geary-khamis:
gkhamis <- function(p, r, n, q, base=NULL, simplify=TRUE, settings=list()){

  # see CPI Manual (2020, p. 448)
  # https://www.ilo.org/wcmsp5/groups/public/---dgreports/---stat/documents/publication/wcms_761444.pdf

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
  res <- spin:::solvemulteq(
    p=p, r=r, n=n, q=q, w=NULL,
    base=base, simplify=simplify, settings=settings,
    P.FUN=P.def, v.FUN=v.def, type="gk")

  # return output:
  return(res)

}

# ikle-dikhanov-balk:
idb <- function(p, r, n, q, w=NULL, base=NULL, simplify=TRUE, settings=list()){

  # see CPI Manual (2020, p. 448)
  # https://www.ilo.org/wcmsp5/groups/public/---dgreports/---stat/documents/publication/wcms_761444.pdf

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
  res <- spin:::solvemulteq(
    p=p, r=r, n=n, q=q, w=w,
    base=base, simplify=simplify, settings=settings,
    P.FUN=P.def, v.FUN=v.def, type="idb")

  # return output:
  return(res)

}

# rao:
rao <- function(p, r, n, q, w=NULL, base=NULL, simplify=TRUE, settings=list()){

  # see Hajargasht (2022, p. 612)
  # https://link.springer.com/book/10.1007/978-981-19-2023-3

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
  res <- spin:::solvemulteq(
    p=p, r=r, n=n, q=q, w=w,
    base=base, simplify=simplify, settings=settings,
    P.FUN=P.def, v.FUN=v.def, type="rao")

  # return output:
  return(res)

}

# gerardi:
gerardi <- function(p, r, n, q, w=NULL, base=NULL, simplify=TRUE, settings=list()){

  # see Balk (1996, p. 208)
  # https://www.scb.se/contentassets/ca21efb41fee47d293bbee5bf7be7fb3/a-comparison-of-ten-methods-for-multilateral-international-price-and-volume-comparison.pdf

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
  res <- spin:::solvemulteq(
    p=p, r=r, n=n, q=q, w=w,
    base=base, simplify=simplify, settings=settings,
    P.FUN=P.def, v.FUN=v.def, type="gerardi")

  # return output:
  return(res)

}

# END
