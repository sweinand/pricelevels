# START

# Title:    Multilateral systems of equations
# Author:   Sebastian Weinand
# Date:     18 May 2024

# print output for class 'multeq':
print.multeq <- function(x, ...){
  print(x$par)
  invisible(x)
}

# solve interrelated equations:
solve_multeq <- function(p, r, n, q, w, base=NULL, simplify=TRUE, P.FUN, v.FUN, type, settings=list()){

  # set default if missing:
  if(missing(q)) q <- NULL
  if(missing(w)) w <- NULL

  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("pricelevels.chatty")
  if(is.null(settings$connect)) settings$connect <- getOption("pricelevels.connect")
  if(is.null(settings$plot)) settings$plot <- getOption("pricelevels.plot")
  if(is.null(settings$solve)) settings$solve <- "iterative"
  if(is.null(settings$tol)) settings$tol <- 1e-9
  if(is.null(settings$max.iter)) settings$max.iter <- 99L

  # non-exported settings:
  if(is.null(settings$check.inputs)) settings$check.inputs <- getOption("pricelevels.check.inputs")
  if(is.null(settings$missings)) settings$missings <- getOption("pricelevels.missings")
  if(is.null(settings$duplicates)) settings$duplicates <- getOption("pricelevels.duplicates")
  if(is.null(settings$norm.weights)) settings$norm.weights <- TRUE

  # input checks:
  if(settings$check.inputs){

    # main inputs:
    check.num(x=p, int=c(0, Inf))
    check.char(x=r)
    check.char(x=n)
    check.num(x=w, null.ok=TRUE, int=c(0, Inf))
    check.num(x=q, null.ok=TRUE, int=c(0, Inf))
    check.char(x=base, miss.ok=TRUE, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE)
    check.char(x=type, min.len=1, max.len=1, na.ok=FALSE)
    check.log(x=simplify, miss.ok=TRUE, min.len=1, max.len=1, na.ok=FALSE)
    check.lengths(x=r, y=n)
    check.lengths(x=r, y=p)
    check.lengths(x=r, y=q)
    check.lengths(x=r, y=w)

    # settings:
    check.log(x=settings$connect, min.len=1, max.len=1, na.ok=FALSE)
    check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
    check.char(x=settings$solve, min.len=1, max.len=1, na.ok=FALSE)
    check.num(x=settings$tol, min.len=1, max.len=1, na.ok=FALSE, int=c(0,Inf))
    check.num(x=settings$max.iter, min.len=1, max.len=1, na.ok=FALSE, int=c(0,Inf))

  }

  # allowed index typey:
  type.vals <- c("gkhamis","ikle","rao","rhajargasht")
  type.vals <- pindices[pindices$name%in%type.vals,]

  # check against allowed index types:
  type <- match.arg(arg=type, choices=type.vals$name, several.ok=FALSE)

  # set solve method:
  if(type=="gkhamis"){
    settings$solve <- match.arg(arg=settings$solve, choices=c("iterative","matrix"))
  }else{
    settings$solve <- match.arg(arg=settings$solve, choices="iterative")
  }

  # error handling for quantity and weights:
  if(settings$check.inputs){

    if(any(type%in%"gkhamis") && is.null(q) && !is.null(w)){
      stop(paste0("Non-valid input -> 'q' required but 'w' provided"), call.=FALSE)
    }

  }

  # initialize data:
  pdata <- arrange(p=p, r=r, n=n, q=q, w=w, base=base, settings=settings)
  if(is.null(q) && is.null(w)) pdata[, c("q","w"):=1]

  # set base region:
  base <- set.base(r=pdata$r, base=base, null.ok=TRUE, settings=settings)

  # Diewert (1999) solution for geary-khamis:
  if(type=="gkhamis" && settings$solve=="matrix"){

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
    if(type%in%"rao"){
      P <- P/exp(mean(log(P)))
    }else{
      P <- P/mean(P)
    }
  }else{
    P <- P/P[names(P)==base]
  }

  if(simplify || settings$plot){

    # match to initial ordering:
    r.lvl <- levels(factor(r))
    res <- P[match(x=r.lvl, table=names(P))]
    names(res) <- r.lvl

  }

  if(settings$plot){

    # compute price ratios:
    pdata[, "ratio":=ratios(p=p, r=r, n=n, base=base, static=TRUE, settings=list(chatty=FALSE))]
    pdata[, "region":=factor(r, levels=r.lvl)]
    plot.pricelevels(data=pdata, P=res)

  }

  if(!simplify){

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
gkhamis <- function(p, r, n, q=NULL, base=NULL, simplify=TRUE, settings=list()){

  # see
  # CPI Manual (2020, p. 448)
  # https://www.ilo.org/wcmsp5/groups/public/---dgreports/---stat/documents/publication/wcms_761444.pdf
  # Rao and Hajargasht (2016, p. 417)
  # https://www.sciencedirect.com/science/article/abs/pii/S0304407615002882

  # definition of average product prices:
  v.def <- function(p, q, w=NULL, n, P){
    res <- stats::ave(x=q*p/P, n, FUN=sum) / stats::ave(x=q, n, FUN=sum)
    names(res) <- n
    return(res)
  }

  # definition of price levels:
  P.def <- function(p, q, w=NULL, r, v){
    res <- stats::ave(x=p*q, r, FUN=sum) / stats::ave(x=v*q, r, FUN=sum)
    names(res) <- r
    return(res)
  }

  # compute index:
  res <- solve_multeq(
    p=p, r=r, n=n, q=q, w=NULL,
    base=base, simplify=simplify, settings=settings,
    P.FUN=P.def, v.FUN=v.def, type="gkhamis")

  # return output:
  return(res)

}

# ikle:
ikle <- function(p, r, n, q=NULL, w=NULL, base=NULL, simplify=TRUE, settings=list()){

  # see
  # CPI Manual (2020, p. 448)
  # https://www.ilo.org/wcmsp5/groups/public/---dgreports/---stat/documents/publication/wcms_761444.pdf
  # Rao and Hajargasht (2016, p. 417)
  # https://www.sciencedirect.com/science/article/abs/pii/S0304407615002882

  # definition of average product prices:
  v.def <- function(p, q=NULL, w, n, P){
    res <- 1 / (stats::ave(x=w*(P/p), n, FUN=sum) / stats::ave(x=w, n, FUN=sum))
    names(res) <- n
    return(res)
  }

  # definition of price levels:
  P.def <- function(p, q=NULL, w, r, v){
    res <- 1 / (stats::ave(x=w*(v/p), r, FUN=sum) / stats::ave(x=w, r, FUN=sum))
    # res <- ave(x=p*q, r, FUN=sum) / ave(x=v*q, r, FUN=sum)
    names(res) <- r
    return(res)
  }

  # compute index:
  res <- solve_multeq(
    p=p, r=r, n=n, q=q, w=w,
    base=base, simplify=simplify, settings=settings,
    P.FUN=P.def, v.FUN=v.def, type="ikle")

  # return output:
  return(res)

}

# rao:
rao <- function(p, r, n, q=NULL, w=NULL, base=NULL, simplify=TRUE, settings=list()){

  # see
  # Hajargasht (2022, p. 612)
  # https://link.springer.com/book/10.1007/978-981-19-2023-3
  # Rao and Hajargasht (2016, p. 417)
  # https://www.sciencedirect.com/science/article/abs/pii/S0304407615002882

  # definition of average product prices:
  v.def <- function(p, q=NULL, w, n, P){
    res <- exp(stats::ave(x=w*log(p/P), n, FUN=sum) / stats::ave(x=w, n, FUN=sum))
    names(res) <- n
    return(res)
  }

  # definition of price levels:
  P.def <- function(p, q=NULL, w, r, v){
    res <- exp(stats::ave(x=w*log(p/v), r, FUN=sum) / stats::ave(x=w, r, FUN=sum))
    names(res) <- r
    return(res)
  }

  # compute index:
  res <- solve_multeq(
    p=p, r=r, n=n, q=q, w=w,
    base=base, simplify=simplify, settings=settings,
    P.FUN=P.def, v.FUN=v.def, type="rao")

  # return output:
  return(res)

}

# rao-hajargasht arithmetic index:
rhajargasht <- function(p, r, n, q=NULL, w=NULL, base=NULL, simplify=TRUE, settings=list()){

  # Rao and Hajargasht (2016, p. 417)
  # https://www.sciencedirect.com/science/article/abs/pii/S0304407615002882

  # definition of average product prices:
  v.def <- function(p, q=NULL, w, n, P){
    res <- stats::ave(x=w*(p/P), n, FUN=sum) / stats::ave(x=w, n, FUN=sum)
    names(res) <- n
    return(res)
  }

  # definition of price levels:
  P.def <- function(p, q=NULL, w, r, v){
    res <- stats::ave(x=w*(p/v), r, FUN=sum) / stats::ave(x=w, r, FUN=sum)
    names(res) <- r
    return(res)
  }

  # compute index:
  res <- solve_multeq(
    p=p, r=r, n=n, q=q, w=w,
    base=base, simplify=simplify, settings=settings,
    P.FUN=P.def, v.FUN=v.def, type="rhajargasht")

  # return output:
  return(res)

}

# # gerardi:
# gerardi <- function(p, r, n, q, w=NULL, base=NULL, simplify=TRUE, settings=list()){
#
#   # see Balk (1996, p. 208)
#   # https://www.scb.se/contentassets/ca21efb41fee47d293bbee5bf7be7fb3/a-comparison-of-ten-methods-for-multilateral-international-price-and-volume-comparison.pdf
#
#   # definition of average product prices:
#   v.def <- function(p, q=NULL, w=NULL, n, P){
#     res <- exp(stats::ave(x=log(p), n, FUN=mean))
#     names(res) <- n
#     return(res)
#   }
#
#   # definition of price levels:
#   P.def <- function(p, q=NULL, w, r, v){
#     res <- 1 / (stats::ave(x=w*(v/p), r, FUN=sum) / stats::ave(x=w, r, FUN=sum))
#     # res <- ave(x=p*q, r, FUN=sum) / ave(x=v*q, r, FUN=sum)
#     names(res) <- r
#     return(res)
#   }
#
#   # compute index:
#   res <- solve_multeq(
#     p=p, r=r, n=n, q=q, w=w,
#     base=base, simplify=simplify, settings=settings,
#     P.FUN=P.def, v.FUN=v.def, type="gerardi")
#
#   # return output:
#   return(res)
#
# }

# END
