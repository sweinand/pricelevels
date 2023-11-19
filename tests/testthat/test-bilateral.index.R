# START


# Data with one region only -----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=1, B=1, N=4)

expect_equal(
  dt[, jevons(p=price, r=region, n=product, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, laspey(p=price, r=region, n=product, w=weight, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, paasche(p=price, r=region, n=product, q=quantity, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, theil(p=price, r=region, n=product, q=quantity, base="1")],
  c("1"=1)
)


# Data with one product only ----------------------------------------------


# example data with one product only:
set.seed(123)
dt <- rdata(R=4, B=1, N=1)

expect_no_error(
  dt[, jevons(p=price, r=region, n=product, base="1")],
)

expect_no_error(
  dt[, laspey(p=price, r=region, n=product, w=weight, base="1")]
)

expect_no_error(
  dt[, paasche(p=price, r=region, n=product, q=quantity, base="1")]
)

expect_no_error(
  dt[, theil(p=price, r=region, n=product, q=quantity, base="1")]
)


# Data with gaps ----------------------------------------------------------


# example data:
dt <- data.table(
  "region"=as.character(rep(1:3, times=4)),
  "product"=as.character(rep(1:4, each=3)),
  "price"=c(10,12,9, 5,5,4, 2,2,2, 17,20,17),
  "quantity"=c(1000,800,1500, 2000,1500,2300, 5000,4000,8000, 200,150,300)
)
dt <- dt[-c(1,2,8),]
dt[, "share" := price*quantity/sum(price*quantity), by="region"]

# base region 1:
dt1 <- merge(x=dt, y=dt[region=="1",], by="product", suffixes=c("",".base"))

# manual computations:
PJe <- dt1[, exp(mean(log(price/price.base))), by="region"]
PJe <- setNames(PJe$V1, PJe$region)

PCa <- dt1[, mean(price/price.base), by="region"]
PCa <- setNames(PCa$V1, PCa$region)

PHa <- dt1[, 1/mean(price.base/price), by="region"]
PHa <- setNames(PHa$V1, PHa$region)

PDu <- dt1[, mean(price)/mean(price.base), by="region"]
PDu <- setNames(PDu$V1, PDu$region)

PCSWD <- sqrt(PHa*PCa)

PLa <- dt1[, sum(price*quantity.base)/sum(price.base*quantity.base), by="region"]
PLa <- setNames(PLa$V1, PLa$region)

PPa <- dt1[, sum(price*quantity)/sum(price.base*quantity), by="region"]
PPa <- setNames(PPa$V1, PPa$region)

PFi <- sqrt(PLa*PPa)

PDr <- (PLa+PPa)/2

PPal <- dt1[, sum(share/sum(share)*(price/price.base)), by="region"]
PPal <- setNames(PPal$V1, PPal$region)

PWa <- dt1[, sum(price*sqrt(quantity*quantity.base))/sum(price.base*sqrt(quantity*quantity.base)), by="region"]
PWa <- setNames(PWa$V1, PWa$region)

PTo <- dt1[, exp(sum(0.5*(price*quantity/sum(price*quantity)+price.base*quantity.base/sum(price.base*quantity.base))*log(price/price.base))), by="region"]
PTo <- setNames(PTo$V1, PTo$region)

PMe <- dt1[, sum(price*(quantity+quantity.base))/sum(price.base*(quantity+quantity.base)), by="region"]
PMe <- setNames(PMe$V1, PMe$region)

dt1[, "w":= ((share/sum(share)+share.base/sum(share.base))/2*(share/sum(share))*(share.base/sum(share.base)))^(1/3), by="region"]
PTh <- dt1[, exp(sum(w/sum(w)*log(price/price.base))), by="region"]
PTh <- setNames(PTh$V1, PTh$region)
dt1[, "w":=NULL]

dt1[, "w":= ifelse(abs(share-share.base)<1e-6, share.base, (share/sum(share)-share.base/sum(share.base))/(log(share/sum(share))-log(share.base/sum(share.base)))), by="region"]
PSv <- dt1[, exp(sum(w/sum(w)*log(price/price.base))), by="region"]
PSv <- setNames(PSv$V1, PSv$region)
dt1[, "w":=NULL]

# unweighted indices:
expect_equal(dt[, jevons(p=price, r=region, n=product, base="1")], PJe)
expect_equal(dt[, carli(p=price, r=region, n=product, base="1")], PCa)
expect_equal(dt[, dutot(p=price, r=region, n=product, base="1")], PDu)
expect_equal(dt[, harmonic(p=price, r=region, n=product, base="1")], PHa)
expect_equal(dt[, cswd(p=price, r=region, n=product, base="1")], PCSWD)

# weighted indices:
expect_equal(dt[, laspey(p=price, r=region, n=product, q=quantity, base="1")], PLa)
expect_equal(dt[, paasche(p=price, r=region, n=product, q=quantity, base="1")], PPa)
expect_equal(dt[, fisher(p=price, r=region, n=product, q=quantity, base="1")], PFi)
expect_equal(dt[, toernq(p=price, r=region, n=product, q=quantity, base="1")], PTo)
expect_equal(dt[, walsh(p=price, r=region, n=product, q=quantity, base="1")], PWa)
expect_equal(dt[, theil(p=price, r=region, n=product, q=quantity, base="1")], PTh)
expect_equal(dt[, medgeworth(p=price, r=region, n=product, q=quantity, base="1")], PMe)
expect_equal(dt[, palgrave(p=price, r=region, n=product, q=quantity, base="1")], PPal)
expect_equal(dt[, drobisch(p=price, r=region, n=product, q=quantity, base="1")], PDr)
expect_equal(dt[, svartia(p=price, r=region, n=product, q=quantity, base="1")], PSv)

# base region 2:
dt2 <- merge(x=dt, y=dt[region=="2",], by="product", suffixes=c("",".base"))

# manual computations:
PJe <- dt2[, exp(mean(log(price/price.base))), by="region"]
PJe <- setNames(PJe$V1, PJe$region)

PCa <- dt2[, mean(price/price.base), by="region"]
PCa <- setNames(PCa$V1, PCa$region)

PHa <- dt2[, 1/mean(price.base/price), by="region"]
PHa <- setNames(PHa$V1, PHa$region)

PDu <- dt2[, mean(price)/mean(price.base), by="region"]
PDu <- setNames(PDu$V1, PDu$region)

PCSWD <- sqrt(PHa*PCa)

PLa <- dt2[, sum(price*quantity.base)/sum(price.base*quantity.base), by="region"]
PLa <- setNames(PLa$V1, PLa$region)

PPa <- dt2[, sum(price*quantity)/sum(price.base*quantity), by="region"]
PPa <- setNames(PPa$V1, PPa$region)

PFi <- sqrt(PLa*PPa)

PDr <- (PLa+PPa)/2

PPal <- dt2[, sum(share/sum(share)*(price/price.base)), by="region"]
PPal <- setNames(PPal$V1, PPal$region)

PWa <- dt2[, sum(price*sqrt(quantity*quantity.base))/sum(price.base*sqrt(quantity*quantity.base)), by="region"]
PWa <- setNames(PWa$V1, PWa$region)

PTo <- dt2[, exp(sum(0.5*(price*quantity/sum(price*quantity)+price.base*quantity.base/sum(price.base*quantity.base))*log(price/price.base))), by="region"]
PTo <- setNames(PTo$V1, PTo$region)

PMe <- dt2[, sum(price*(quantity+quantity.base))/sum(price.base*(quantity+quantity.base)), by="region"]
PMe <- setNames(PMe$V1, PMe$region)

dt2[, "w":= ((share/sum(share)+share.base/sum(share.base))/2*(share/sum(share))*(share.base/sum(share.base)))^(1/3), by="region"]
PTh <- dt2[, exp(sum(w/sum(w)*log(price/price.base))), by="region"]
PTh <- setNames(PTh$V1, PTh$region)
dt2[, "w":=NULL]

dt2[, "w":= ifelse(abs(share-share.base)<1e-6, share.base, (share/sum(share)-share.base/sum(share.base))/(log(share/sum(share))-log(share.base/sum(share.base)))), by="region"]
PSv <- dt2[, exp(sum(w/sum(w)*log(price/price.base))), by="region"]
PSv <- setNames(PSv$V1, PSv$region)
dt2[, "w":=NULL]

# unweighted indices:
expect_equal(dt[, jevons(p=price, r=region, n=product, base="2")], PJe)
expect_equal(dt[, carli(p=price, r=region, n=product, base="2")], PCa)
expect_equal(dt[, dutot(p=price, r=region, n=product, base="2")], PDu)
expect_equal(dt[, harmonic(p=price, r=region, n=product, base="2")], PHa)
expect_equal(dt[, cswd(p=price, r=region, n=product, base="2")], PCSWD)

# weighted indices:
expect_equal(dt[, laspey(p=price, r=region, n=product, q=quantity, base="2")], PLa)
expect_equal(dt[, paasche(p=price, r=region, n=product, q=quantity, base="2")], PPa)
expect_equal(dt[, fisher(p=price, r=region, n=product, q=quantity, base="2")], PFi)
expect_equal(dt[, toernq(p=price, r=region, n=product, q=quantity, base="2")], PTo)
expect_equal(dt[, walsh(p=price, r=region, n=product, q=quantity, base="2")], PWa)
expect_equal(dt[, theil(p=price, r=region, n=product, q=quantity, base="2")], PTh)
expect_equal(dt[, medgeworth(p=price, r=region, n=product, q=quantity, base="2")], PMe)
expect_equal(dt[, palgrave(p=price, r=region, n=product, q=quantity, base="2")], PPal)
expect_equal(dt[, drobisch(p=price, r=region, n=product, q=quantity, base="2")], PDr)
expect_equal(dt[, svartia(p=price, r=region, n=product, q=quantity, base="2")], PSv)

# check rebasing:
expect_equal(
  dt[, carli(p=price, r=region, n=product, base="1")][1],
  c("1"=1)
)

expect_equal(
  dt[, carli(p=price, r=region, n=product, base="2")][2],
  c("2"=1)
)

expect_equal(
  dt[, laspey(p=price, r=region, n=product, q=quantity, base="1")][1],
  c("1"=1)
)

expect_equal(
  dt[, laspey(p=price, r=region, n=product, q=quantity, base="2")][2],
  c("2"=1)
)

# test quantities versus shares as weights:
dt[, "share" := (price*quantity)/sum(price*quantity), by="region"]

expect_equal(
  dt[, laspey(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, laspey(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, paasche(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, paasche(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, fisher(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, fisher(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, toernq(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, toernq(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, walsh(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, walsh(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, theil(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, theil(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, drobisch(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, drobisch(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, palgrave(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, palgrave(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, svartia(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, svartia(p=price, r=region, n=product, w=share, base="1")]
)


# Settings ----------------------------------------------------------------


expect_error(
  dt[, laspey(p=price, r=region, n=product, q=quantity, settings=list(chatty="abc"))]
)

expect_error(
  dt[, laspey(p=price, r=region, n=product, q=quantity, settings=list(connect="abc"))]
)


# Non-connected data ------------------------------------------------------


# example data:
set.seed(123)
dt1 <- spin::rdata(R=3, B=1, N=5)
dt2 <- spin::rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)

expect_equal(
  dt[, laspey(p=price, r=region, n=product, q=quantity, base="1",
              settings=list(chatty=FALSE, connect=TRUE))][1],
  c("1"=1)
)

expect_equal(
  dt[, laspey(p=price, r=region, n=product, q=quantity, base="1",
              settings=list(chatty=FALSE, connect=TRUE))][4:7],
  setNames(rep(NA_real_, 4), 4:7)
)

expect_equal(
  dt[, laspey(p=price, r=region, n=product, q=quantity, base="4",
              settings=list(chatty=FALSE, connect=TRUE))][1:3],
  setNames(rep(NA_real_, 3), 1:3)
)

expect_equal(
  dt[, laspey(p=price, r=region, n=product, q=quantity, base="4",
              settings=list(chatty=FALSE, connect=TRUE))][4],
  c("4"=1)
)


# Misc --------------------------------------------------------------------


# check consistency of helper function, i.e., if matrix
# function produce the same output as vectorized function

# sample data:
set.seed(1)
dt <- spin::rdata(R=5, B=7, N=1, gaps=0.25)
dt[, "share" := price*quantity/sum(price*quantity), by="region"]
data.table::setnames(dt, c("group","weight","r","n","is_sale","p","q","share"))

# reshape data to matrices:
P <- as.matrix(
  x=data.table::dcast(data=dt, formula=n~r, fun.aggregate=mean, value.var="p", fill=NA),
  rownames="n")

Q <- as.matrix(
  x=data.table::dcast(data=dt, formula=n~r, fun.aggregate=sum, value.var="q", fill=NA),
  rownames="n")

W <- as.matrix(
  x=data.table::dcast(data=dt, formula=n~r, fun.aggregate=mean, value.var="share", fill=NA),
  rownames="n")

# jevons:
system.time(PJ1 <- spin:::.jevons(P=P, Q=Q))
system.time(PJ2 <- dt[, jevons(p=p, r=r, n=n, base="1")])
expect_equal(PJ1, PJ2)

# carli:
system.time(PC1 <- spin:::.carli(P=P, Q=Q))
system.time(PC2 <- dt[, carli(p=p, r=r, n=n, base="1")])
expect_equal(PC1, PC2)

# dutot:
system.time(PD1 <- spin:::.dutot(P=P, Q=Q))
system.time(PD2 <- dt[, dutot(p=p, r=r, n=n, base="1")])
expect_equal(PD1, PD2)

# harmonic:
system.time(PH1 <- spin:::.harmonic(P=P, Q=Q))
system.time(PH2 <- dt[, harmonic(p=p, r=r, n=n, base="1")])
expect_equal(PH1, PH2)

# cswd:
system.time(PCSWD1 <- spin:::.cswd(P=P, Q=Q))
system.time(PCSWD2 <- dt[, cswd(p=p, r=r, n=n, base="1")])
expect_equal(PCSWD1, PCSWD2)

# marshall-edgeworth:
system.time(PMe1 <- spin:::.medgeworth(P=P, Q=Q))
system.time(PMe2 <- dt[, medgeworth(p=p, r=r, n=n, q=q, base="1")])
expect_equal(PMe1, PMe2)

# laspeyres:
system.time(PL1 <- spin:::.laspey(P=P, Q=Q))
system.time(PL2 <- dt[, laspey(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PL1, PL2)

system.time(PL3 <- spin:::.laspey(P=P, W=W))
system.time(PL4 <- dt[, laspey(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PL3, PL4)

# compare weights versus quantities:
expect_equal(PL1, PL3)

# paasche:
system.time(PP1 <- spin:::.paasche(P=P, Q=Q))
system.time(PP2 <- dt[, paasche(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PP1, PP2)

system.time(PP3 <- spin:::.paasche(P=P, W=W))
system.time(PP4 <- dt[, paasche(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PP3, PP4)

# compare weights versus quantities:
expect_equal(PP1, PP3)

# fisher:
system.time(PF1 <- spin:::.fisher(P=P, Q=Q))
system.time(PF2 <- dt[, fisher(p=p, q=q, r=r, n=n,base="1")])
expect_equal(PF1, PF2)

system.time(PF3 <- spin:::.fisher(P=P, W=W))
system.time(PF4 <- dt[, fisher(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PF3, PF4)

# compare weights versus quantities:
expect_equal(PF1, PF3)

# palgrave:
system.time(PPal1 <- spin:::.palgrave(P=P, Q=Q))
system.time(PPal2 <- dt[, palgrave(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PPal1, PPal2)

system.time(PPal3 <- spin:::.palgrave(P=P, W=W))
system.time(PPal4 <- dt[, palgrave(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PPal3, PPal4)

# compare weights versus quantities:
expect_equal(PPal1, PPal3)

# drobisch:
system.time(PDr1 <- spin:::.drobisch(P=P, Q=Q))
system.time(PDr2 <- dt[, drobisch(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PDr1, PDr2)

system.time(PDr3 <- spin:::.drobisch(P=P, W=W))
system.time(PDr4 <- dt[, drobisch(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PDr3, PDr4)

# compare weights versus quantities:
expect_equal(PDr1, PDr3)

# walsh:
system.time(PW1 <- spin:::.walsh(P=P, Q=Q))
system.time(PW2 <- dt[, walsh(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PW1, PW2)

system.time(PW3 <- spin:::.walsh(P=P, W=W))
system.time(PW4 <- dt[, walsh(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PW3, PW4)

# compare weights versus quantities:
expect_equal(PW1, PW3)

# theil:
system.time(PTh1 <- spin:::.theil(P=P, Q=Q))
system.time(PTh2 <- dt[, theil(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PTh1, PTh2)

system.time(PTh3 <- spin:::.theil(P=P, W=W))
system.time(PTh4 <- dt[, theil(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PTh3, PTh4)

# compare weights versus quantities:
expect_equal(PTh1, PTh3)

# toernqvist:
system.time(PT1 <- spin:::.toernq(P=P, Q=Q))
system.time(PT2 <- dt[, toernq(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PT1, PT2)

system.time(PT3 <- spin:::.toernq(P=P, W=W))
system.time(PT4 <- dt[, toernq(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PT3, PT4)

# compare weights versus quantities:
expect_equal(PT1, PT3)

# sato-vartia:
system.time(PSv1 <- spin:::.svartia(P=P, Q=Q))
system.time(PSv2 <- dt[, svartia(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PSv1, PSv2)

system.time(PSv3 <- spin:::.svartia(P=P, W=W))
system.time(PSv4 <- dt[, svartia(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PSv3, PSv4)

# compare weights versus quantities:
expect_equal(PSv1, PSv3)

# END

