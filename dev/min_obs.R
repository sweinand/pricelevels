# START

# Title:  Comparison of two strategies for sampling minimum connected data
# Author: Sebastian Weinand
# Date:   2022-03-10

# load package:
library(data.table)

# random region and products:
R <- 15 # number of regions
N <- 20 # number of products

r <- factor(rep(1:R, N)) # region identifier
n <- factor(rep(1:N, each=R)) # product identifier

r0 <- levels(r) # unique regions
n0 <- levels(n) # unique products


# (1) Approach by while-loop ----------------------------------------------


r.con <- sample(r0, 1) # sample initial connecting region
r.samp <- setdiff(r0, r.con) # remaining region sample
out <- list()
i <- 0
while(length(r.samp)>0){
  
  i <- i+1 # increment by one
  
  if(i>=length(n0)){
    s <- length(r.samp) # sample size equal to all remaining regions
  }else{
    s <- min(1,length(r.samp)):length(r.samp)
    s <- sample(x=s, size=1) # random sample size
  }
  
  out[[i]] <- c(r.con, sample(x=r.samp, size=s, replace=FALSE)) # i-th region group
  r.con <- sample(x=out[[i]], size=1) # sample one connecting region
  r.samp <- setdiff(r.samp, out[[i]]) # remaining regions to be sampled
  
}

# # more products needed than available:
# if(length(out)-length(n0)>0) n.add <- sample(x=n0, size=length(out)-length(n0), replace=TRUE) else n.add <- NULL

# products n1 and regions r1 required for connectivity:
n1 <- rep(sample(x=n0, size=length(out)), lengths(out))
r1 <- unlist(out)

# products n2 and regions r2 only added to satisfy R and N:
n2 <- setdiff(n0, n1)
r2 <- sample(x=r0, size=length(n2), replace=TRUE)

# minimum data stock:
data.min0 <- data.table("n"=c(n1,n2), "r"=c(r1,r2), stringsAsFactors=FALSE)

# some checks:
table(data.min0$n, data.min0$r)
out
all.equal(nrow(data.min0), N+R-1)
data.min0[, spindex::is_connected(r=r, n=n)] # check


# (2) Approach by for-loop ------------------------------------------------


s <- sample(x=1:min(N, max(1,R-1)), size=1) # connected through s products
g <- sample(x=c(1:s, sample(x=1:s, size=max(0, R-1-s), replace=TRUE)))
if(R>1) g <- c(1,g)
out <- split(x=r0, f=g)
if(length(out)>1){
  for(i in 2:length(out)){
    out[[i]] <- c(out[[i]], sample(x=out[[i-1]], size=1L)) # add one connecting region
  }
}

# products n1 and regions r1 required for connectivity:
n1 <- rep(sample(x=n0, size=length(out)), lengths(out))
r1 <- unlist(out)

# products n2 and regions r2 only added to satisfy R and N:
n2 <- setdiff(n0, n1)
r2 <- sample(x=r0, size=length(n2), replace=TRUE)

# minimum data stock:
data.min0 <- data.table("n"=c(n1,n2), "r"=c(r1,r2), stringsAsFactors=FALSE)

# some checks:
table(data.min0$n, data.min0$r)
out
all.equal(nrow(data.min0), N+R-1)
data.min0[, spindex::is_connected(r=r, n=n)] # check


# Functions ---------------------------------------------------------------


# clear workspace:
rm(list=setdiff(ls(), c("r","n","r0","n0")))

# helper function:
for_loop <- function(r0, n0, exclude=NULL){
  
  # @args:
  # r0        unique regions
  # n0        unique products
  # exclude   list of regions and products to be kept
  
  # number of regions and products:
  R <- length(r0)
  N <- length(n0)
  
  # random number defining how many products are used
  # to build connected data:
  s <- sample(1:min(N, max(1,R-1)),1) 
  
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
  r2 <- sample(x=r0, size=length(n2), replace=TRUE)
  
  # minimum data stock:
  data.min0 <- data.frame("n"=c(n1,n2), "r"=c(r1,r2), stringsAsFactors=FALSE)
  
  # keep regions if wanted by the user:
  data.minR <- expand.grid("r"=exclude$r, "n"=n0, stringsAsFactors=FALSE)
  
  # keep products if wanted by the user:
  data.minN <- expand.grid("r"=r0, "n"=exclude$n, stringsAsFactors=FALSE)
  
  # combine minimum data:
  data.min <- unique(rbind(data.min0, data.minR, data.minN, stringsAsFactors=FALSE))
  
  # print output:
  return(data.min)
  
}

# helper function:
while_loop <- function(r0, n0, exclude = NULL){
  
  # number of regions and products:
  R <- length(r0)
  N <- length(n0)
  
  r.con <- sample(r0, 1) # sample initial connecting region
  r.samp <- setdiff(r0, r.con) # remaining region sample
  out <- list()
  i <- 0
  while(length(r.samp)>0){
    
    i <- i+1 # increment by one
    
    if(i>=length(n0)){
      s <- length(r.samp) # sample size equal to all remaining regions
    }else{
      s <- sample(x=min(1,length(r.samp)):length(r.samp), size=1) # random sample size
    }
    
    out[[i]] <- c(r.con, sample(x=r.samp, size=s, replace=FALSE)) # i-th region group
    r.con <- sample(x=out[[i]], size=1) # sample one connecting region
    r.samp <- setdiff(r.samp, out[[i]]) # remaining regions to be sampled
    
  }
  
  # # more products needed than available:
  # if(length(out)-length(n0)>0) n.add <- sample(x=n0, size=length(out)-length(n0), replace=TRUE) else n.add <- NULL
  
  # products n1 and regions r1 required for connectivity:
  n1 <- rep(sample(x=n0, size=length(out)), lengths(out))
  r1 <- unlist(out)
  
  # products n2 and regions r2 only added to satisfy R and N:
  n2 <- setdiff(n0, n1)
  r2 <- sample(x=r0, size=length(n2), replace=TRUE)
  
  # minimum data stock:
  data.min0 <- data.frame("n"=c(n1,n2), "r"=c(r1,r2), stringsAsFactors=FALSE)
  
  # keep regions if wanted by the user:
  data.minR <- expand.grid("r"=exclude$r, "n"=n0, stringsAsFactors=FALSE)
  
  # keep products if wanted by the user:
  data.minN <- expand.grid("r"=r0, "n"=exclude$n, stringsAsFactors=FALSE)
  
  # combine minimum data:
  data.min <- unique(rbind(data.min0, data.minR, data.minN, stringsAsFactors=FALSE))
  
  #
  return(data.min)
  
}

# check if connected:
with(for_loop(r0=r0, n0=n0, exclude=NULL), spindex::is_connected(r=r,n=n))
with(while_loop(r0=r0, n0=n0, exclude=NULL), spindex::is_connected(r=r,n=n))

# for_loop() uses the whole range of possibilities, e.g. also
# cases where (R-1) loops are necessary. in contrast, while_loop()
# most of the time use only a few products to ensure connectivity:
summary(replicate(n=1000, expr=sum(rowSums(table(for_loop(r0=r0, n0=n0, exclude=NULL)))>1)))
summary(replicate(n=1000, expr=sum(rowSums(table(while_loop(r0=r0, n0=n0, exclude=NULL)))>1)))
# consequently, while_loop() is computationally a bit faster.
# however, the behaviour of for_loop() is preferable.
system.time(replicate(n=1000, expr=for_loop(r0=r0, n0=n0, exclude=NULL)))
system.time(replicate(n=1000, expr=while_loop(r0=r0, n0=n0, exclude=NULL)))

# END