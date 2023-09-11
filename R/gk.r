# START

# Title:    Gery-Khamis index
# Author:   Sebastian Weinand
# Date:     11 September 2023

# geary-khamis index:
gk <- function(p, r, n, q, base=NULL, simplify=TRUE, settings=list()){

  # input checks:
  .check.num(x=p, int=c(0, Inf))
  .check.char(x=r)
  .check.char(x=n)
  .check.num(x=q, int=c(0, Inf))
  .check.char(x=base, miss.ok=TRUE, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE)
  .check.log(x=simplify, miss.ok=TRUE, min.len=1, max.len=1, na.ok=FALSE)
  .check.lengths(x=r, y=n)
  .check.lengths(x=r, y=p)
  .check.lengths(x=r, y=q)

  # define settings:
  if(is.null(settings$method)) settings$method <- "solve"
  if(is.null(settings$tol)) settings$tol <- 1e-6
  if(is.null(settings$max.iter)) settings$max.iter <- 99L

  # match argument:
  settings$method <- match.arg(arg=settings$method, choices=c("solve","iterative"))
  .check.num(x=settings$tol, min.len=1, max.len=1, na.ok=FALSE, int=c(0,Inf))
  .check.num(x=settings$max.iter, min.len=1, max.len=1, na.ok=FALSE, int=c(0,Inf))

  # definition of average product prices:
  v.def <- function(p, q, n, P){
    res <- ave(x=q*p/P, n, FUN=sum) / ave(x=q, n, FUN=sum)
    names(res) <- n
    return(res)
  }

  # definition of price levels:
  P.def <- function(p, q, r, v){
    res <- ave(x=p*q, r, FUN=sum) / ave(x=v*q, r, FUN=sum)
    names(res) <- r
    return(res)
  }

  # gather in data.table:
  pdata <- data.table("r"=as.character(r), "n"=as.character(n), "p"=as.numeric(p), "q"=as.numeric(q))

  # if both q and w are provided, q will be checked:
  pdata <- pdata[complete.cases(r, n, p, q), ]

  # stop if no observations left:
  if(nrow(pdata)<=0L){
    stop("No complete cases available. All data pairs contain at least one NA.")
  }

  # check for duplicated entries:
  if(anyDuplicated(x=pdata, by=c("r","n"))>0L){

    # average duplicated prices and sum duplicated quantities:
    pdata <- pdata[, list("p"=mean(p), "q"=sum(q)), by=c("r","n")]

    # print warning:
    warning("Duplicated observations found and aggregated.", call.=FALSE)

  }

  # set base region:
  if(!base%in%pdata$r && !is.null(base)){
    # reset base region and print warning:
    base <- names(which.max(table(pdata$r)))[1]
    warning(paste("Base region not found and reset to", base), call.=FALSE)
  }

  # Diewert (1999) solution:
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
    Ptmp <- pdata[, P.def(p=p, q=q, r=r, v=b[match(x=n, names(b))])]

  }

  # iterative search procedure:
  if(settings$method=="iterative"){

    Ptmp <- rep(1, nrow(pdata))
    i <- 0
    check <- TRUE
    while(check && i<=settings$max.iter){

      # compute price levels:
      Ptmp0 <- Ptmp
      vtmp <- pdata[, v.def(p=p, q=q, n=n, P=Ptmp)]
      Ptmp <- pdata[, P.def(p=p, q=q, r=r, v=vtmp)]

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
    v <- pdata[, v.def(p=p, q=q, n=n, P=P[match(x=r, table=names(P))])]
    v <- split(x=v, f=names(v))
    v <- sapply(X=v, "[[", 1L)

    # gather output:
    res <- c("v"=v, "P"=P)

  }

  # return output:
  return(res)

}

# END
