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
  dt[, carli(p=price, r=region, n=product, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, dutot(p=price, r=region, n=product, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, harmonic(p=price, r=region, n=product, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, laspey(p=price, r=region, n=product, w=weight, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, paasche(p=price, r=region, n=product, w=weight, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, walsh(p=price, r=region, n=product, w=weight, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, fisher(p=price, r=region, n=product, w=weight, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, toernq(p=price, r=region, n=product, w=weight, base="1")],
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
  dt[, carli(p=price, r=region, n=product, base="1")]
)

expect_no_error(
  dt[, dutot(p=price, r=region, n=product, base="1")],
)

expect_no_error(
  dt[, harmonic(p=price, r=region, n=product, base="1")]
)

expect_no_error(
  dt[, laspey(p=price, r=region, n=product, w=weight, base="1")]
)

expect_no_error(
  dt[, paasche(p=price, r=region, n=product, w=weight, base="1")]
)

expect_no_error(
  dt[, fisher(p=price, r=region, n=product, w=weight, base="1")]
)

expect_no_error(
  dt[, walsh(p=price, r=region, n=product, w=weight, base="1")]
)

expect_no_error(
  dt[, toernq(p=price, r=region, n=product, w=weight, base="1")]
)


# Data with gaps ----------------------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=3, B=1, N=4, gaps=0.2, settings=list(exclude=data.frame("r"=1, "n"=NA)))
dt[, "weight" := rweights(r=region, b=product, type=~b)]

# manual computations:
r <- dt[, list(region, price/price[region=="1"], weight), by="product"]
J <- r[, exp(mean(log(V2), na.rm=TRUE)), by="region"]
J <- setNames(J$V1, J$region)
C <- r[, mean(V2, na.rm=TRUE), by="region"]
C <- setNames(C$V1, C$region)
H <- r[, 1/mean(1/V2, na.rm=TRUE), by="region"]
H <- setNames(H$V1, H$region)
D1 <- dt[product%in%intersect(dt[region=="1", product], dt[region=="1", product]) & region%in%c("1"), mean(price[region=="1"])/mean(price[region=="1"])]
D2 <- dt[product%in%intersect(dt[region=="1", product], dt[region=="2", product]) & region%in%c("1","2"), mean(price[region=="2"])/mean(price[region=="1"])]
D3 <- dt[product%in%intersect(dt[region=="1", product], dt[region=="3", product]) & region%in%c("1","3"), mean(price[region=="3"])/mean(price[region=="1"])]
D <- setNames(c(D1,D2,D3), c("1","2","3"))
L <- r[, weighted.mean(V2, weight, na.rm=TRUE), by="region"]
L <- setNames(L$V1, L$region)
P <- r[, 1/weighted.mean(1/V2, weight, na.rm=TRUE), by="region"]
P <- setNames(P$V1, P$region)
Fi <- sqrt(L*P)
To <- r[, exp(weighted.mean(log(V2), weight, na.rm=TRUE)), by="region"]
To <- setNames(To$V1, To$region)
W <- r[, weighted.mean(sqrt(V2), weight, na.rm=TRUE)/weighted.mean(sqrt(1/V2), weight, na.rm=TRUE), by="region"]
W <- setNames(W$V1, W$region)

# unweighted indices:
expect_equal(dt[, jevons(p=price, r=region, n=product, base="1")], J)
expect_equal(dt[, carli(p=price, r=region, n=product, base="1")], C)
expect_equal(dt[, dutot(p=price, r=region, n=product, base="1")], D)
expect_equal(dt[, harmonic(p=price, r=region, n=product, base="1")], H)

# weighted indices:
expect_equal(dt[, laspey(p=price, r=region, n=product, w=weight, base="1")], L)
expect_equal(dt[, paasche(p=price, r=region, n=product, w=weight, base="1")], P)
expect_equal(dt[, fisher(p=price, r=region, n=product, w=weight, base="1")], Fi)
expect_equal(dt[, toernq(p=price, r=region, n=product, w=weight, base="1")], To)
expect_equal(dt[, walsh(p=price, r=region, n=product, w=weight, base="1")], W)

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


# example data without gaps:
set.seed(123)
dt <- rdata(R=3, B=1, N=4, gaps=0)
dt[, "weight" := rweights(r=region, b=product, type=~b)]

# manual computations:
r <- dt[, list(region, price/price[region=="1"], weight), by="product"]
J <- r[, exp(mean(log(V2))), by="region"]
J <- setNames(J$V1, J$region)
C <- r[, mean(V2), by="region"]
C <- setNames(C$V1, C$region)
H <- r[, 1/mean(1/V2), by="region"]
H <- setNames(H$V1, H$region)
D <- dt[, mean(price), by="region"]
D <- setNames(D$V1/D$V1[D$region=="1"], D$region)
L <- r[, weighted.mean(V2, weight), by="region"]
L <- setNames(L$V1, L$region)
P <- r[, 1/weighted.mean(1/V2, weight), by="region"]
P <- setNames(P$V1, P$region)
Fi <- sqrt(L*P)
To <- r[, exp(weighted.mean(log(V2), weight)), by="region"]
To <- setNames(To$V1, To$region)
W <- r[, weighted.mean(sqrt(V2), weight)/weighted.mean(sqrt(1/V2), weight), by="region"]
W <- setNames(W$V1, W$region)

# unweighted indices:
expect_equal(dt[, jevons(p=price, r=region, n=product, base="1")], J)
expect_equal(dt[, carli(p=price, r=region, n=product, base="1")], C)
expect_equal(dt[, dutot(p=price, r=region, n=product, base="1")], D)
expect_equal(dt[, harmonic(p=price, r=region, n=product, base="1")], H)

# weighted indices:
expect_equal(dt[, laspey(p=price, r=region, n=product, w=weight, base="1")], L)
expect_equal(dt[, paasche(p=price, r=region, n=product, w=weight, base="1")], P)
expect_equal(dt[, fisher(p=price, r=region, n=product, w=weight, base="1")], Fi)
expect_equal(dt[, walsh(p=price, r=region, n=product, w=weight, base="1")], W)
expect_equal(dt[, toernq(p=price, r=region, n=product, w=weight, base="1")], To)


# Check consistency of helper functions -----------------------------------


# check if helper function indices produce the
# same results, i.e., if matrix function produces
# the same output as vectorized function

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

# walsh:
system.time(PW1 <- spin:::.walsh(P=P, Q=Q))
system.time(PW2 <- dt[, walsh(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PW1, PW2)

system.time(PW3 <- spin:::.walsh(P=P, W=W))
system.time(PW4 <- dt[, walsh(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PW3, PW4)

# compare weights versus quantities:
expect_equal(PW1, PW3)

# toernqvist:
system.time(PT1 <- spin:::.toernq(P=P, Q=Q))
system.time(PT2 <- dt[, toernq(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PT1, PT2)

system.time(PT3 <- spin:::.toernq(P=P, W=W))
system.time(PT4 <- dt[, toernq(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PT3, PT4)

# compare weights versus quantities:
expect_equal(PT1, PT3)

# END

