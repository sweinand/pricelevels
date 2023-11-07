# START

# Title:  Linear and nonlinear CPD regression
# Author: Sebastian Weinand
# Date:   6 November 2023

# CPD method:
cpd <- function(p, r, n, q=NULL, w=NULL, base=NULL, simplify=TRUE, settings=list()){

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

  # input checks:
  if(settings$check.inputs){

    # main inputs:
    .check.num(x=p, int=c(0, Inf))
    .check.char(x=r)
    .check.char(x=n)
    .check.num(x=w, null.ok=TRUE, int=c(0, Inf))
    .check.num(x=q, null.ok=TRUE, int=c(0, Inf))
    .check.char(x=base, miss.ok=TRUE, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE)
    .check.log(x=simplify, miss.ok=TRUE, min.len=1, max.len=1, na.ok=FALSE)
    .check.lengths(x=r, y=n)
    .check.lengths(x=r, y=p)
    .check.lengths(x=r, y=w)
    .check.lengths(x=r, y=q)

    # settings:
    .check.log(x=settings$connect, min.len=1, max.len=1, na.ok=FALSE)
    .check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)

  }

  # initialize data:
  pdata <- spin:::arrange(p=p, r=r, n=n, q=q, w=w, base=base, settings=settings)

  # set base region:
  base <- spin:::set.base(r=pdata$r, base=base, null.ok=TRUE, settings=settings)
  if(!is.null(base)) pdata[, "r" := relevel(x=r, ref=base)]

  # change coefficient names:
  setnames(x=pdata, old=c("r","n"), new=c("lnP","pi"))

  # CPD regression formula:
  cpd_mod <- log(p) ~ pi + lnP - 1

  # CASE: one product, multiple regions:
  if(nlevels(pdata$pi) <= 1){

    # update formula:
    cpd_mod <- update.formula(old = cpd_mod, new = . ~ lnP + 1)
    # include intercept to express regional price levels
    # relative to base level

  }

  # CASE: one region, one or multiple products:
  if(nlevels(pdata$lnP) <= 1){

    # update formula:
    cpd_mod <- update.formula(old = cpd_mod, new = . ~ 0)
    # no regional comparison of prices possible in this case
    # -> empty model definition

  }else{

    # update contrasts:
    if(is.null(base)){

      contrasts(x=pdata$lnP) <- contr.sum(levels(pdata$lnP))
      colnames(contrasts(x=pdata$lnP)) <- levels(pdata$lnP)[-nlevels(pdata$lnP)]

    }else{

      contrasts(x=pdata$lnP) <- contr.treatment(levels(pdata$lnP))
      colnames(contrasts(x=pdata$lnP)) <- levels(pdata$lnP)[-1]

    }

  }

  # estimate CPD regression model:
  if(is.null(w) && is.null(q)){
    cpd_reg_out <- lm(formula=cpd_mod, data=pdata, singular.ok=FALSE)
  }else{
    cpd_reg_out <- lm(formula=cpd_mod, data=pdata, weights=w, singular.ok=FALSE)
  }

  # simplify to price levels only or not:
  if(simplify){

    # extract estimated regional price levels:
    out <- dummy.coef(cpd_reg_out)[["lnP"]]
    # usage of 'dummy.coef' requires dummy variables of class 'factor'

    # set price level if there is only one region:
    if(is.null(out)) out <- setNames(0, levels(pdata$lnP))

    # match to initial ordering:
    r.lvl <- levels(factor(r))
    out <- out[match(x=r.lvl, table=names(out))]
    names(out) <- r.lvl

    # unlog price levels:
    out <- exp(out)

  }else{

    # keep lm-object:
    out <- cpd_reg_out
    names(out$coefficients) <- gsub("^(pi|lnP)", "\\1.", names(out$coefficients))

  }

  # print output to console:
  out

}

# helper function to derive NLCPD parameter start values:
.nlcpd_self_start <- function(p, r, n, w, w.delta, base=NULL, strategy="s1"){

  # gather data:
  dt <- data.table(p, r, n, w)
  n.lev <- levels(dt$n)
  r.lev <- levels(dt$r)

  # add delta weights:
  dt[, "w_delta" := w.delta[match(x=n, table=names(w.delta))]]

  if(strategy == "s1"){

    pi <- dt[, weighted.mean(log(p), w), by="n"]
    pi <- setNames(pi$V1, pi$n)
    pi <- pi[match(n.lev, names(pi))]
    lnP <- dt[, weighted.mean(log(p), w), by="r"]
    lnP <- setNames(lnP$V1, lnP$r)
    lnP <- lnP[match(r.lev, names(lnP))]
    if(is.null(base)){
      lnP <- lnP[-length(lnP)]-mean(lnP)
    }else{
      lnP <- lnP[-1]-lnP[1] # nlcpd() relevels according to base, hence base is first position
    }
    delta <- rep(1, length(pi)-1)
    names(delta) <- n.lev[-1]

  }

  if(strategy %in% c("s2", "s3")){

    mod.cpd <- dt[, spin::cpd(p=p, r=r, n=n, w=w, base=base, simplify=FALSE)]
    beta.cpd <- dummy.coef(mod.cpd)
    lnP <- beta.cpd$lnP
    pi <- beta.cpd$pi
    # if only one product:
    if(is.null(pi)) pi <- setNames(beta.cpd[["(Intercept)"]], n.lev)

    if(strategy == "s2"){
      delta <- rep(1, length(pi)-1)
      names(delta) <- n.lev[-1]
    }

    if(strategy == "s3"){
      dt$lnP <- lnP[match(dt$r, names(lnP))]
      dt$pi <- pi[match(dt$n, names(pi))]
      dt.delta <- dt[ , list("d"=sum(lnP*(log(p)-pi)/sum(lnP^2)), "w_delta"=w_delta[1]), by="n"]
      delta <- setNames(dt.delta[, d/sum(d*w_delta)], dt.delta$n)
      delta <- delta[match(n.lev, names(delta))]
      delta <- delta[-1]
    }

    if(is.null(base)) lnP <- lnP[-length(lnP)] else lnP <- lnP[-1]

  }

  list("lnP"=lnP, "pi"=pi, "delta"=delta)

}

# helper function to check NLCPD parameter start values:
.nlcpd_start_check <- function(x, r, n, len=NULL){

  input <- deparse(substitute(x))
  msg_prefix <- paste("Non-valid input for", input, "->")
  n <- factor(n)
  r <- factor(r)

  # check list:
  if(!is.list(x)) stop(paste(msg_prefix, "must be a list"), call. = FALSE)
  if(length(x) != 3L) stop(paste(msg_prefix, "must be of length 3L"), call. = FALSE)
  if(!all(c("lnP","pi","delta") %in% names(x))) stop(paste(msg_prefix, "list names must be 'lnP', 'pi', 'delta'"), call. = FALSE)

  # check elements:
  if(length(x$lnP)>0) spin:::.check.num(x=x$lnP, min.len=0, max.len=Inf, miss.ok=FALSE, null.ok=FALSE, na.ok=FALSE, int=c(-Inf,Inf))
  if(length(x$pi)>0) spin:::.check.num(x=x$pi, min.len=0, max.len=Inf, miss.ok=FALSE, null.ok=FALSE, na.ok=FALSE, int=c(-Inf,Inf))
  if(length(x$delta)>0) spin:::.check.num(x=x$delta, min.len=0, max.len=Inf, miss.ok=FALSE, null.ok=FALSE, na.ok=FALSE, int=c(-Inf,Inf))

  # check element names:
  if(is.null(names(x$lnP))) stop(paste(msg_prefix, "'lnP' must have names"), call. = FALSE)
  if(is.null(names(x$pi))) stop(paste(msg_prefix, "'pi' must have names"), call. = FALSE)
  if(is.null(names(x$delta))) stop(paste(msg_prefix, "'delta' must have names"), call. = FALSE)

  # subset to matches:
  x$lnP <- x$lnP[names(x$lnP)%in%levels(r)]
  x$pi <- x$pi[names(x$pi)%in%levels(n)]
  x$delta <- x$delta[names(x$delta)%in%levels(n)]

  # check matching lengths:
  if(length(x$lnP)<len[["lnP"]]) stop(paste(msg_prefix, "'names(par.start$lnP) %in% levels(r)' must be of length greater or equal to", len[["lnP"]]), call.=FALSE)
  if(length(x$pi)<len[["pi"]]) stop(paste(msg_prefix, "'names(par.start$pi) %in% levels(n)' must be of length greater or equal to", len[["pi"]]), call.=FALSE)
  if(length(x$delta)<len[["delta"]]) stop(paste(msg_prefix, "'names(par.start$delta) %in% levels(n)' must be of length greater or equal to", len[["delta"]]), call.=FALSE)

  # subset to required lengths:
  x$lnP <- x$lnP[1:len[["lnP"]]]
  x$pi <- x$pi[1:len[["pi"]]]
  x$delta <- x$delta[1:len[["delta"]]]

  # return output:
  return(x)

}

# helper function for NLCPD optimization:
.nlcpd_optimize <- function(par, n, r, w=NULL, w.delta, base=NULL){

  # note that regression weights w are not used within this function
  # it is only the delta weights w.delta that are processed

  # data:
  dt <- data.table("n" = factor(n), "r" = factor(r))
  N <- nlevels(dt$n) # number of products
  R <- nlevels(dt$r) # number of regions

  # split start values by parameter:
  lnP <- par[grepl("^lnP\\.", names(par))]
  pi <- par[grepl("^pi\\.", names(par))]
  delta <- par[grepl("^delta\\.", names(par))]

  # set base region:
  if(base%in%levels(dt$r) && !is.null(base)) dt[, "r" := relevel(x=r, ref=base)]

  # adjust parameter names for matching:
  names(lnP) <- gsub("^lnP\\.", "", names(lnP))
  lnP.base <- setdiff(levels(dt$r), names(lnP))
  if(!is.null(base) && lnP.base!=base){
    stop("Base region in 'base' does not match 'start$lnP'.")
  }

  # compute base region's price level according to normalization:
  if(is.null(base)){
    lnP <- setNames(c(lnP, -sum(lnP)), c(names(lnP), lnP.base))
  }else{
    lnP <- setNames(c(0, lnP), c(lnP.base, names(lnP)))
  }

  # compute base delta according to normalization:
  names(delta) <- gsub("^delta\\.", "", names(delta))
  delta.base <- setdiff(levels(dt$n), names(delta))
  w.delta <- w.delta[match(x=c(delta.base, names(delta)), table=names(w.delta))] # align if deltas are ordered randomly
  delta <- setNames(c((1-sum(w.delta[-1]*delta))/w.delta[1], delta), c(delta.base, names(delta)))

  # adjust parameter names for matching:
  names(pi) <- gsub("^pi\\.", "", names(pi))

  # add parameter values:
  dt$lnP <- lnP[match(dt$r, names(lnP))]
  dt$pi <- pi[match(dt$n, names(pi))]
  dt$delta <- delta[match(dt$n, names(delta))]

  return(dt[, pi + delta*lnP])

}

# helper function for Jacobi matrix:
.nlcpd_jacobi <- function(par, n, r, w=NULL, w.delta, base=NULL){

  # this is exactly the same coding as in .nlcpd_optimize()
  # until the matrices are calculated!

  # note that regression weights w are not used within this function
  # it is only the delta weights w.delta that are processed

  # data:
  dt <- data.table("n" = factor(n), "r" = factor(r))
  N <- nlevels(dt$n) # number of products
  R <- nlevels(dt$r) # number of regions

  # split start values by parameter:
  lnP <- par[grepl("^lnP\\.", names(par))]
  pi <- par[grepl("^pi\\.", names(par))]
  delta <- par[grepl("^delta\\.", names(par))]

  # set base region:
  if(base%in%levels(dt$r) && !is.null(base)) dt[, "r" := relevel(x=r, ref=base)]

  # adjust parameter names for matching:
  names(lnP) <- gsub("^lnP\\.", "", names(lnP))
  lnP.base <- setdiff(levels(dt$r), names(lnP))
  if(!is.null(base) && lnP.base!=base){
    stop("Base region in 'base' does not match 'start$lnP'.")
  }

  # compute base region's price level according to normalization:
  if(is.null(base)){
    lnP <- setNames(c(lnP, -sum(lnP)), c(names(lnP), lnP.base))
  }else{
    lnP <- setNames(c(0, lnP), c(lnP.base, names(lnP)))
  }

  # compute base delta according to normalization:
  names(delta) <- gsub("^delta\\.", "", names(delta))
  delta.base <- setdiff(levels(dt$n), names(delta))
  w.delta <- w.delta[match(x=c(delta.base, names(delta)), table=names(w.delta))]
  delta <- setNames(c((1-sum(w.delta[-1]*delta))/w.delta[1], delta), c(delta.base, names(delta)))

  # adjust parameter names for matching:
  names(pi) <- gsub("^pi\\.", "", names(pi))

  # add parameter values:
  dt$lnP <- lnP[match(dt$r, names(lnP))]
  dt$pi <- pi[match(dt$n, names(pi))]
  dt$delta <- delta[match(dt$n, names(delta))]

  # (A) jacobian of pi:
  if(N>1){
    A <- model.matrix(~n-1, data=dt, xlev=list("n"=names(pi)))
  }else{
    A <- matrix(data=1, nrow=nrow(dt), ncol=1)
  }
  colnames(A) <- paste0("pi.", names(pi))

  # (B) jacobian of lnP:
  if(is.null(base)) contr.lnP <- "contr.sum" else contr.lnP <- "contr.treatment"
  B <- dt$delta*model.matrix(~r, data=dt, xlev=list("r"=names(lnP)), contrasts.arg = list("r"=contr.lnP))[,-1]
  colnames(B) <- paste0("lnP.", setdiff(names(lnP), lnP.base))

  # (C) jacobian of delta:
  # the following appraoch is like weighted contrast sum, i.e.
  # the weighted sum of parameters is 0. in the nlcpd regression
  # the weighted sum of deltas is 1, and not 0. However, because
  # the jacobian contains partial derivatives, this has no impact,
  # as the only difference arises in a constant term which cancels
  # out by taking derivatives
  if(N>1){
    C <- model.matrix(~n-1, data=dt, xlev=list("n"=names(delta)))
    # delta.base is always at first position. hence, also weight of
    # delta.base is at first position due to matching above
    C <- dt$lnP*(C[,-1]-C[,1]%*%matrix(data=w.delta[-1]/w.delta[1], nrow=1L))
    # old approach:
    # C <- model.matrix(~n, data=dt, xlev=list("n"=c(setdiff(names(delta), delta.base), delta.base)), contrasts.arg = list("n"="contr.sum"))[,-1]
    # C[C<0] <- (-1)*matrix(data=w[-1]/w[1], ncol=length(w[-1]), nrow=length(C[C<0])/length(w[-1]), byrow=TRUE)
    # C <- dt$lnP*C
    colnames(C) <- paste0("delta.", setdiff(names(delta), delta.base))
  }else{
    C <- NULL
  }

  # return full jacobian:
  J <- cbind(A,B,C)
  return(J)

}

# NLCPD method:
nlcpd <- function(p, r, n, q=NULL, w=NULL, base=NULL, simplify=TRUE, settings=list(), ...){

  # set default if missing:
  if(missing(q)) q <- NULL
  if(missing(w)) w <- NULL

  # set default settings if missing:
  if(is.null(settings$connect)) settings$connect <- TRUE
  if(is.null(settings$chatty)) settings$chatty <- TRUE
  if(is.null(settings$use.jac)) settings$use.jac <- FALSE
  if(is.null(settings$par.start)) settings$par.start <- NULL
  if(is.null(settings$self.start)) settings$self.start <- "s1"
  if(is.null(settings$w.delta)) settings$w.delta <- NULL

  # non-exported settings:
  if(is.null(settings$check.inputs)) settings$check.inputs <- TRUE
  if(is.null(settings$missings)) settings$missings <- TRUE
  if(is.null(settings$duplicates)) settings$duplicates <- TRUE

  # input checks:
  if(settings$check.inputs){

    # main inputs:
    .check.num(x=p, int=c(0, Inf))
    .check.char(x=r)
    .check.char(x=n)
    .check.num(x=w, null.ok=TRUE, int=c(0, Inf))
    .check.num(x=q, null.ok=TRUE, int=c(0, Inf))
    .check.char(x=base, min.len=1, max.len=1, miss.ok=TRUE, null.ok=TRUE, na.ok=FALSE)
    .check.log(x=simplify, min.len=1, max.len=1, miss.ok=TRUE, na.ok=FALSE)
    .check.lengths(x=r, y=n)
    .check.lengths(x=r, y=p)
    .check.lengths(x=r, y=w)
    .check.lengths(x=r, y=q)

    # settings:
    .check.log(x=settings$connect, min.len=1, max.len=1, na.ok=FALSE)
    .check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
    .check.log(x=settings$use.jac, min.len=1, max.len=1, na.ok=FALSE)
    .check.char(x=settings$self.start, min.len=1, max.len=1, na.ok=FALSE)
    # settings$w.delta and settings$par.start are checked later

  }

  # overwrite defaults by ellipsis elements:
  defaults <- formals(minpack.lm::nls.lm)
  dots <- list(...)
  defaults[names(dots)] <- dots

  # residual function to be minimzed:
  resid_fun <- function(par, p, r, n, w, w.delta, base=NULL){
    sqrt(w)*(log(p) - .nlcpd_optimize(par=par, n=n, r=r, w=w, w.delta=w.delta, base=base))
  }

  # jacobi function:
  if(settings$use.jac){
    jacobi_fun <- function(par, p=NULL, r, n, w, w.delta, base=NULL){
      sqrt(w)*(-1)*.nlcpd_jacobi(par=par, n=n, r=r, w=w, w.delta=w.delta, base=base)
    }
  }else{
    jacobi_fun <- defaults$jac
  }

  # initialize data:
  pdata <- spin:::arrange(p=p, r=r, n=n, q=q, w=w, base=base, settings=settings)

  # set base region:
  base <- spin:::set.base(r=pdata$r, base=base, null.ok=TRUE, settings=settings)
  if(!is.null(base)) pdata[, "r" := relevel(x=r, ref=base)]

  # number of regions:
  R <- nlevels(pdata$r)

  # output if there is only one region:
  if(R<=1){

    if(simplify){
      out <- setNames(1, levels(pdata$r))
    }else{
      out <- NULL
    }

  }else{

    # number of observations per product:
    nfreq <- pdata[, table(as.character(n))] # should be no factor due to unused levels

    # coerce to factor with ordering of products such that those with
    # one observation are not residually derived:
    pdata[, "n" := factor(x=n, levels=names(sort(nfreq, decreasing=TRUE)))]

    # number of products
    N <- nlevels(pdata$n)

    # set delta weights if missing:
    if(is.null(settings$w.delta)){
      w.delta <- pdata[, tapply(X=w, INDEX=n, FUN=mean)]
      w.delta <- w.delta/sum(w.delta) # normalisation of weights
    }else{
      if(is.null(names(settings$w.delta))) stop("Non-valid input for 'settings$w.delta' -> vector must have names")
      if(!all(levels(pdata$n)%in%names(settings$w.delta), na.rm=TRUE)) stop("Non-valid input for 'settings$w.delta' -> weights for all products 'levels(n)' required")
      if(abs(sum(settings$w.delta)-1)>1e-5 && settings$chatty) warning("Sum of 'settings$w.delta' not 1")
      w.delta <- settings$w.delta
    }

    # set start parameters if not given by user:
    if(is.null(settings$par.start)){
      settings$self.start <- match.arg(arg=settings$self.start, choices=paste0("s", 1:3))
      start <- with(pdata, .nlcpd_self_start(p=p, r=r, n=n, w=w, w.delta=w.delta, base=base, strategy=settings$self.start))
    }else{
      start <- with(pdata, .nlcpd_start_check(x=settings$par.start, r=r, n=n, len=c("lnP"=R-1, "pi"=N, "delta"=N-1)))
    }

    # # input checks on start:
    # .check.nlcpd.start(x=start, r=pdata$r, n=pdata$n, min.len=c("lnP"=R-1, "pi"=N, "delta"=N-1))

    # reorder start parameters:
    start <- start[c("pi", "lnP", "delta")] # important if use.jac=TRUE
    par.start <- unlist(start, use.names=TRUE)

    # set lower and/or upper bounds on delta parameter
    # for products with only one observations. with
    # one observations only, delta can not be estimated
    # properly and estimated price levels will no longer
    # be transitive:
    if(any(nfreq<=1, na.rm=TRUE)){

      # match to parameter:
      nfreq <- nfreq[nfreq<=1]
      if(length(nfreq)>0) names(nfreq) <- paste0("delta.", names(nfreq))
      m <- match(x=names(nfreq), table=names(par.start))

      # set lower bounds:
      if(is.null(defaults$lower)){
        defaults$lower <- rep(x=-Inf, times=length(par.start))
        defaults$lower[m] <- 1
      }

      # set upper bounds:
      if(is.null(defaults$upper)){
        defaults$upper <- rep(x=Inf, times=length(par.start))
        defaults$upper[m] <- 1

      }

    }

    # estimate NLCPD model:
    nlcpd_reg_out <- minpack.lm::nls.lm(
      par = par.start,
      fn = resid_fun,
      jac = jacobi_fun,
      lower = defaults$lower,
      upper = defaults$upper,
      control = defaults$control,
      p = pdata$p,
      r = pdata$r,
      n = pdata$n,
      w = pdata$w,
      w.delta = w.delta,
      base = base)

    # simplify to price levels only or not:
    if(simplify){

      # extract estimated regional price levels:
      out <- coef(nlcpd_reg_out)
      out <- out[grepl("^lnP\\.", names(out))]
      names(out) <- gsub(pattern="^lnP\\.", replacement="", x=names(out))

      # add price level of base region:
      r.miss <- setdiff(x=levels(pdata$r), y=names(out))
      if(is.null(base)) out.miss <- -sum(out) else out.miss <- 0
      names(out.miss) <- r.miss
      out <- c(out, out.miss)

      # match to initial ordering and unlog:
      r.lvl <- levels(factor(r))
      out <- exp(out)[match(x=r.lvl, table=names(out))]
      names(out) <- r.lvl

    }else{

      # keep object:
      out <- nlcpd_reg_out

      # add delta weights:
      out$w.delta <- w.delta

    }

  }

  # print output to console:
  return(out)

}

# END
