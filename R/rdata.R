# START

# Title:    Sampling of random price and quantity data
# Author:   Sebastian Weinand
# Date:     16 January 2024

# non-exported helper functions:
rpi <- function(n, mean=exp(1), sd=exp(1), min=log(1), max=Inf){

  # sample random logarithmic pi-parameter

  ### @args:
  # n         integer, number of products
  # mean      integer, mean of distribution, 1 by default
  # sd        integer, standard deviation of distribution
  # min, max  intergers, range of sampling

  # truncated normal distribution:
  # see https://stats.stackexchange.com/questions/113230/generate-random-numbers-following-a-distribution-within-an-interval
  if(min>max) stop("Error: Empty interval.")
  lb <- stats::pnorm(min, mean, sd)
  ub <- stats::pnorm(max, mean, sd)
  U <- stats::runif(n=n, min=lb, max=ub)
  stats::qnorm(p=U, mean=mean, sd=sd)

}
rdelta <- function(n, mean=1, sd, w=NULL){

  # sample random delta-parameter

  ### @args:
  # n     integer, number of products
  # mean  integer, mean of distribution, 1 by default
  # sd    integer, standard deviation of distribution
  # w     numeric, vector of weights, or NULL

  # set mean and sd if negative standard deviation provided:
  if(sd<=0){mean <- 1; sd <- 0}

  # set weights if w=NULL:
  if(is.null(w)) w <- rep(1, n)
  w <- w/sum(w)

  # initialize check sum:
  dw.sum <- 0
  i <- 1

  # loop until check sum is in between 1/2 and 2.
  # if dw.sum is allowed to be out of this interval
  # it impacts sampled d-values by normalization
  # too strong. resulting delta-values could
  # take unwanted extreme values:
  while(i<100 && (dw.sum < 1/2 || dw.sum > 2)){
    d <- stats::rnorm(n=n, mean=mean, sd=sd)
    dw.sum <- sum(d*w)
  }

  if(i<100){
    # normalize:
    res <- d / dw.sum
  }else{
    warning("No valid delta-parameters found -> reset to delta=1")
    res <- rep(1, n)
  }

  # return output:
  return(res)

}
min_obs <- function(r0, n0, pairs=FALSE, exclude=NULL){

  # sample minimum connected data

  # @args:
  # r0          unique regions
  # n0          unique products
  # pairs       logical indicating if at least two observations
  #             for each product should be available in different
  #             regions (see notes)
  # exclude     list of regions and products to be kept

  # @notes:
  # if exclude=NULL and there needs to be at least one
  # observation per product (pairs=FALSE), the minimum
  # number of observations is defined by the formula
  # 1*R+(N-1)*1 = R+N-1.
  # if exclude=NULL and there needs to be at least two
  # observations in different regions for each product
  # (pairs=TRUE), the minimum number of observations
  # is defined by the formula 2*N+max(0,R-N-1). hence,
  # if N>=(R-1), this formula simplifies to R+N-1

  # number of regions and products:
  R <- length(r0)
  N <- length(n0)

  # random number defining how many products are used
  # to build connected data:
  s <- min(N, max(1,R-1))
  if(!pairs) s <- sample(x=1:s, size=1)

  # ensure that all 1:s products are available at least once,
  # fill if not all regions are captured:
  g <- sample(c(1:s, sample(x=1:s, size=max(0,R-1-s), replace=TRUE)))

  # add first to cover all R regions:
  if(R>1) g <- c(1,g)

  # split into groups:
  out <- split(x=r0, f=g)

  if(length(out) > 1L){
    # add one connecting region in each loop:
    for(i in 2:length(out)){
      out[[i]] <- c(out[[i]], sample(x=out[[i-1]], size=1L))
    }
  }

  # products n1 and regions r1 required for connectivity:
  n1 <- rep(sample(x=n0, size=length(out)), lengths(out))
  r1 <- unlist(out)

  # products n2 and regions r2 only added to satisfy R and N:
  n2 <- setdiff(n0, n1)
  if(pairs && R>1){
    r2 <- lapply(X=n2, FUN=function(z) sample(x=r0, size=2, replace=FALSE))
    n2 <- rep(x=n2, times=lengths(r2))
    r2 <- unlist(x=r2, use.names=FALSE)
  }else{
    r2 <- sample(x=r0, size=length(n2), replace=TRUE)
  }


  # minimum data stock:
  data.min <- data.table("n"=c(n1,n2), "r"=c(r1,r2), stringsAsFactors=FALSE)

  # keep specific regions and/or products:
  if(!is.null(exclude)){

    # drop NA-NA-combinations because this would translate into
    # 'keep all observations':
    data.excl <- as.data.table(exclude)
    data.excl <- data.excl[rowSums(!is.na(data.excl))>0,]

    # resolve NAs for products:
    data.excl.r <- data.excl[is.na(n), list("n"=n0), by="r"]

    # resolve NAs for regions:
    data.excl.n <- data.excl[is.na(r), list("r"=r0), by="n"]

    # gather exclusions:
    data.excl <- unique(
      rbindlist(
        l=list(data.excl[!(is.na(r) | is.na(n)),], data.excl.n, data.excl.r),
        use.names=TRUE,
        fill=TRUE))

  }else{

    data.excl <- NULL

  }

  # combine minimum data:
  data.min <- unique(rbind(data.min, data.excl))

  # print output:
  return(data.min)

}
rsales <- function(p, q, amount=0, max.rebate=1/4, max.qi=2){

  # simulate random random sales

  ### @args:
  # p, q          vector of prices and quantities
  # sales         percentage amount of sales
  # max.rebate    maximum allowed percentage price reduction for sales
  # maxqi         maximum allowed percentage quantity increase for sales

  # input checks:
  check.num(x=p, int=c(0, Inf))
  check.num(x=q, int=c(0, Inf))
  check.num(x=amount, null.ok=FALSE, int=c(0, 1))
  check.num(x=max.rebate, null.ok=FALSE, int=c(0.001, 1))
  check.num(x=max.qi, null.ok=FALSE, int=c(1, Inf))
  check.lengths(x=p, y=q)

  # sales flag:
  sf <- sample(x=c(TRUE,FALSE), size=length(p), replace=TRUE, prob=c(amount, 1-amount))

  # sales price reduction factors and quantity increase factors:
  spf <- stats::runif(n=sum(sf), min=(1-max.rebate), max=0.999)
  sqf <- stats::runif(n=sum(sf), min=1, max=max.qi)

  # adjust prices and quantities:
  p[sf] <- p[sf]*spf
  q[sf] <- q[sf]*sqf

  # output:
  res <- list("price_is_sale"=sf, "price"=p, "quantity"=q)

  # return output:
  return(res)


}

# sample random gaps in price data while keeping connectedness:
rgaps <- function(r, n, amount=0, prob=NULL, pairs=FALSE, exclude=NULL){

  ### @args:
  # r         vector of regions
  # n         vector of products
  # amount    numeric value defining the percentage amount of gaps
  # prob      vector of probability weights, same length as r and n.
  #           higher weights make gaps occur more likely at this position
  # pairs     logical indicating if at least two observations
  #           for each product should be available in different
  #           regions
  # exclude   list of two vectors of regions and products where
  #           no gaps should occur

  # input checks:
  check.char(x=r)
  check.char(x=n)
  check.num(x=amount, miss.ok=TRUE, null.ok=FALSE, int=c(0, 1))
  check.num(x=prob, miss.ok=TRUE, null.ok=TRUE, int=c(0, Inf))
  check.log(x=pairs, miss.ok=TRUE, min.len=1, max.len=1, na.ok=FALSE)
  check.lengths(x=r, y=n)
  check.lengths(x=r, y=prob)

  # check input for "exclude":
  if(is.null(exclude) | missing(exclude)){
    exclude <- NULL
  }else{
    if(!is.data.frame(exclude)) stop("'exclude' must be a data.frame or data.table.")
  }

  # coerce input vectors to character:
  r <- as.character(r)
  n <- as.character(n)

  # unique elements:
  r0 <- unique(r)
  n0 <- unique(n)

  # number of unique regions and products:
  R <- length(r0)
  N <- length(n0)

  # check if missing values shall be included:
  if(amount>0){

    # check if there are already missing prices:
    if(length(r)<N*R){

      # print warning:
      warning("Missing prices already present in the data: no further processing.")

      # output:
      out <- rep(x=FALSE, times=length(r))

    }else{

      # minimum number of observations required:
      if(pairs){n.obs.min <- 2*N+max(0,R-N-1)}else{n.obs.min <- R+N-1}

      # check number of observations:
      if((1-n.obs.min/(N*R)-amount) < 0){

        # adjust absolute number of missing prices to maximum allowed:
        nmiss <- N*R-n.obs.min

        # print warning if missing was set to high:
        warning(paste0("'amount' reduced to maximum allowed for these data: ", round(1-n.obs.min/(N*R), 2), "."))

      }else{

        # transform relative to absolute amount of missing prices:
        nmiss <- floor(round(amount*N*R, 2))

      }

      # derive random composition of minimum data:
      data.min <- min_obs(r0=r0, n0=n0, pairs=pairs, exclude=exclude)

      # vector with identifiers of initial ordering:
      id0 <- paste0("r", r, "n", n)

      # vector with minimum identifiers:
      id.min <- paste0("r", data.min$r, "n", data.min$n)

      # match such that TRUE shows those observations that could be missing prices:
      out <- match(x=id0, table=id.min, nomatch=0) <= 0
      # alternatively, use data.table's chmatch()-function. be careful
      # when using merge() because initial row ordering could get
      # destroyed. moreover, base merge is very slow.

      # number of observations to be re-sampled:
      nsamp <- sum(out, na.rm = TRUE)

      # sample gaps:
      g <- vector(length=nsamp)
      g[sample(x=1:nsamp, size=min(nsamp, nmiss), prob=prob[out])] <- TRUE
      out[out] <- g

    }

  }else{

    # output:
    out <- rep(x=FALSE, times=length(r))

  }

  # print output to console:
  return(as.logical(out))

}

# sample random expenditure share weights:
rweights <- function(r, b, type=~1){

  ### @args:
  # r      vector of regions
  # b      vector of product groups
  # type   formula, defining the weights:
  #        o type=~1    # constant weights
  #        o type=~b    # product group-specific weights constant among regions
  #        o type=~b+r  # product group-specific weights varying among regions

  # input checks:
  check.char(x=r)
  check.char(x=b)
  check.lengths(x=r, y=b)
  if(!inherits(type, "formula")) stop("type must be a formula", call.=FALSE)

  # parse variables from formula:
  rhs.vars <- labels(stats::terms(type))

  # gather initial data and set default weights:
  dt <- data.table(r, b, stringsAsFactors=FALSE)

  # full data with no gaps:
  dt.full <- as.data.table(
    expand.grid(
      "r"=unique(dt$r, na.rm=TRUE),
      "b"=unique(dt$b, na.rm=TRUE)))

  # set default weights:
  dt.full[, "w" := 1]

  # sample weights according to formula:
  if(length(rhs.vars) > 0){

    # dt.full[, "w" := runif(n=1, min=1, max=100), by="b"]
    dt.full[, "w" := stats::rlnorm(n=1), by="b"]

    if(all(c("r","b") %in% rhs.vars)){
      dt.full[, "w_adj":= stats::rnorm(.N, mean=1, sd=0.1)]
      dt.full[, "w" := pmax(0, w*w_adj)] # non-negative weights
    }
  }

  # normalize weights:
  dt.full[, "w" := w/sum(w), by="r"]

  # add weights to initial data:
  dt <- merge(x=dt, y=dt.full, by=c("r","b"), all.x=TRUE, sort=FALSE)

  # return output:
  return(dt$w)

}

# sample random price and quantity data:
rdata <- function(R, B, N, gaps=0, weights=~b+r, sales=0, settings=list()){

  ### @args:
  # R           integer, number of regions r=1,...,R
  # B           integer, number of product groups/basic headings b=1,...,B
  # N           integer, number of products i=1,...,N[b] per product group b
  # gaps        see rgaps()
  # weights     see rweights()
  # sales       relative amount of sales
  # settings    list of
  #             o gaps.prob: see argument 'prob' in rgaps()
  #             o gaps.pairs: see argument 'pairs' in rgaps()
  #             o gaps.exclude: see argument 'exclude' in rgaps()
  #             o sales.max.rebate: see argument 'max.rebate' in rsales()
  #             o sales.maxqi: see argument 'max.qi' in rsales()
  #             o par.add: logical, should true parameter values be added to data
  #             o par.sd: numeric, named vector of standard deviations of
  #               parameters and error term. standard deviation of delta
  #               currently cant be set if weights=~n+r
  #             o round: round prices to two decimals or keep all decimals

  # input checks:
  check.num(x=R, min.len=1, max.len=1, na.ok=FALSE, int=c(1,Inf))
  check.num(x=B, min.len=1, max.len=1, na.ok=FALSE, int=c(1,Inf))
  check.num(x=N, min.len=1, max.len=B, na.ok=FALSE, int=c(1,Inf))

  # set settings if necessary:
  if(is.null(settings$gaps.prob)) settings$gaps.prob <- NULL
  if(is.null(settings$gaps.pairs)) settings$gaps.pairs <- FALSE
  if(is.null(settings$gaps.exclude)) settings$gaps.exclude <- NULL
  if(is.null(settings$sales.max.rebate)) settings$sales.max.rebate <- 1/4
  if(is.null(settings$sales.max.qi)) settings$sales.max.qi <- 2
  if(is.null(settings$par.add)) settings$par.add <- FALSE
  if(is.null(settings$round)) settings$round <- TRUE
  sd.defaults <- c("lnP"=1/10, "pi"=exp(1), "delta"=sqrt(1/2), "error"=1/100)
  if(is.null(settings$par.sd)) settings$par.sd <- sd.defaults
  settings$par.sd <- c(settings$par.sd, sd.defaults[!names(sd.defaults) %in% names(settings$par.sd)])
  settings$par.sd <- as.list(settings$par.sd)

  # number of regions, product groups and products:
  R <- as.integer(R)
  B <- as.integer(B)
  NB <- as.integer(N) # number of products per basic heading
  if(length(NB)<=1) NB <- rep(x=NB, times=B)
  N <- sum(NB) # number of products over all basic headings

  # region, group, and product identifier:
  r <- formatC(x=1:R, width=log10(R)+1, format="d", flag="0")
  b <- formatC(x=1:B, width=log10(B)+1, format="d", flag="0")
  n <- formatC(x=1:N, width=log10(N)+1, format="d", flag="0")

  # data:
  dt <- data.table(
    "group"=factor(rep(b, times=R*NB)),
    "region"=factor(unlist(lapply(X=NB, FUN=function(z) rep(x=r, times=z)))),
    "product"=factor(rep(n, each=R)))

  # sample expenditure share weights for product groups:
  dt[, "weight" := rweights(r=region, b=group, type=weights)]

  # true product prices:
  piB <- rpi(n=B, mean=1, sd=settings$par.sd$pi, min=-1, max=10)
  pi <- unlist(mapply(FUN=rpi, n=NB, mean=piB, sd=0.15, min=-1, max=10, SIMPLIFY=FALSE))
  names(pi) <- n

  # true regional price spreads across products:
  # w <- dt[, list("w"=weight[1]), by=c("group","product")][, list(product, "w"=w/uniqueN(product)), by="group"]$w
  w <- dt[, list("w"=mean(weight)), by="product"]$w
  delta <- rdelta(n=N, mean=1, sd=settings$par.sd$delta, w=w) # sum(w*delta)=1
  names(delta) <- n
  # currently, the deltas can be very different for products
  # within the same product group

  # # true regional price spreads across product groups:
  # w <- unique(x=dt, by=c("group","region"))[, list("w"=mean(weight)), by="group"]$w
  # delta <- rdelta(n=B, mean=1, sd=settings$par.sd$delta, w=w)
  # names(delta) <- b

  # true regional price levels:
  lnP <- stats::rnorm(n=R, mean=0, sd=settings$par.sd$lnP)
  lnP <- lnP-mean(lnP) # sum(lnP)=0
  names(lnP) <- r

  # add true parameters to data:
  dt$delta <- delta[match(x=dt$product, table=names(delta))]
  dt$lnP <- lnP[match(x=dt$region, table=names(lnP))]
  dt$pi <- pi[match(x=dt$product, table=names(pi))]

  # add logarithmic prices and random noise:
  dt[, "price" := as.vector(pi + delta*lnP)]
  dt[, "error" := stats::rnorm(n=.N, mean=0, sd=settings$par.sd$error)]

  # unlog prices:
  dt[, "price" := exp(price + error)]
  # boxplot(price~paste(group,product,sep=":"), data=dt)

  # random product shares within each product group:
  dt[, "prod_share" := as.numeric(sample(x=1:3, size=.N, replace=TRUE)), by=c("region","group")]
  dt[, "prod_share" := prod_share/sum(prod_share), by=c("region","group")]

  # sample overall turnover for each region:
  dt[, "turnover" := stats::runif(n=1, min=max(price/(weight*prod_share)), max=max(c(price/(weight*prod_share), 9999999))), by="region"]

  # subdivide turnover by product group and product (in one step):
  dt[, "prod_turnover" := turnover*weight*prod_share]

  # derive non-negative quantities:
  dt[, "quantity" := pmax(0, prod_turnover/price)]

  # introduce sales:
  dt[, "sale" := FALSE]
  if(sales>0){
    dt[, c("sale","price","quantity") := rsales(p=price, q=quantity, amount=sales, max.rebate=settings$sales.max.rebate, max.qi=settings$sales.max.qi)]
  }

  # introduce gaps:
  if(gaps>0){
    dt <- dt[!rgaps(r=region, n=product, amount=gaps, prob=settings$gaps.prob, pairs=settings$gaps.pairs, exclude=settings$gaps.exclude), ]
  }

  # round prices and quantites:
  dt[, "quantity" := round(x=quantity, digits=0)]
  if(settings$round) dt[, "price" := round(x=price, digits=2L)]
  # if the error variance is set to zero and there are no gaps,
  # cpd() and nlcpd() should provide estimates identical to
  # the true values. however, there will be small differences
  # when the prices are rounded

  # select columns and set key:
  dt <- subset(x=dt, select=c("group","weight","region","product","sale","price","quantity"))
  setkeyv(x=dt, cols=c("group","product","region"))

  # add parameter values to data:
  out <- list(
    "param"=list("lnP"=lnP, "pi"=pi,"delta"=delta),
    "data"=dt[])

  # subset to data:
  if(!settings$par.add) out <- out$data

  # print output to console:
  return(out)

}

# END
