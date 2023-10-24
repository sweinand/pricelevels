# START


# Unweighted indices ------------------------------------------------------


# example data with one region only:
set.seed(123)
data <- rdata(R=1, B=1, N=4)

expect_equal(
  data[, jevons(p=price, r=region, n=product)],
  c("1"=1)
)

expect_equal(
  data[, carli(p=price, r=region, n=product)],
  c("1"=1)
)

expect_equal(
  data[, dutot(p=price, r=region, n=product)],
  c("1"=1)
)

expect_equal(
  data[, harmonic(p=price, r=region, n=product)],
  c("1"=1)
)

# example data with one product only:
set.seed(123)
data <- rdata(R=4, B=1, N=1)

expect_no_error(
  data[, jevons(p=price, r=region, n=product)],
)

expect_no_error(
  data[, carli(p=price, r=region, n=product)]
)

expect_no_error(
  data[, dutot(p=price, r=region, n=product)],
)

expect_no_error(
  data[, harmonic(p=price, r=region, n=product)]
)

# example data without weights and gaps:
set.seed(123)
data <- rdata(R=3, B=1, N=4)

r <- data[, list(region, price/price[region=="1"]), by="product"]
J <- r[, exp(mean(log(V2))), by="region"]
J <- setNames(J$V1, J$region)
C <- r[, mean(V2), by="region"]
C <- setNames(C$V1, C$region)
H <- r[, 1/mean(1/V2), by="region"]
H <- setNames(H$V1, H$region)
D <- data[, mean(price), by="region"]
D <- setNames(D$V1/D$V1[D$region=="1"], D$region)

expect_equal(
  data[, jevons(p=price, r=region, n=product)],
  J
)

expect_equal(
  data[, carli(p=price, r=region, n=product)],
  C
)

expect_equal(
  data[, dutot(p=price, r=region, n=product)],
  D
)

expect_equal(
  data[, harmonic(p=price, r=region, n=product)],
  H
)


# example data without weights and gaps:
set.seed(2)
data <- rdata(R=3, B=1, N=4, gaps=0.2)

r <- data[, list(region, price/price[region=="1"]), by="product"]
J <- r[, exp(mean(log(V2))), by="region"]
J <- setNames(J$V1, J$region)
C <- r[, mean(V2), by="region"]
C <- setNames(C$V1, C$region)
H <- r[, 1/mean(1/V2), by="region"]
H <- setNames(H$V1, H$region)
D1 <- data[product%in%intersect(data[region=="1", product], data[region=="1", product]) & region%in%c("1"), mean(price[region=="1"])/mean(price[region=="1"])]
D2 <- data[product%in%intersect(data[region=="1", product], data[region=="2", product]) & region%in%c("1","2"), mean(price[region=="2"])/mean(price[region=="1"])]
D3 <- data[product%in%intersect(data[region=="1", product], data[region=="3", product]) & region%in%c("1","3"), mean(price[region=="3"])/mean(price[region=="1"])]
D <- setNames(c(D1,D2,D3), c("1","2","3"))

expect_equal(
  data[, jevons(p=price, r=region, n=product)],
  J
)

expect_equal(
  data[, carli(p=price, r=region, n=product)],
  C
)

expect_equal(
  data[, dutot(p=price, r=region, n=product)],
  D
)

expect_equal(
  data[, harmonic(p=price, r=region, n=product)],
  H
)


# Weighted indices --------------------------------------------------------


# example data with one region only:
set.seed(123)
data <- rdata(R=1, B=1, N=4)
data[, "weight" := rweights(r=region, b=product, type=~b)]

expect_equal(
  data[, laspey(p=price, r=region, n=product, w=weight)],
  c("1"=1)
)

expect_equal(
  data[, paasche(p=price, r=region, n=product, w=weight)],
  c("1"=1)
)

expect_equal(
  data[, walsh(p=price, r=region, n=product, w=weight)],
  c("1"=1)
)

expect_equal(
  data[, fisher(p=price, r=region, n=product, w=weight)],
  c("1"=1)
)

expect_equal(
  data[, toernq(p=price, r=region, n=product, w=weight)],
  c("1"=1)
)

# example data with one product only:
set.seed(123)
data <- rdata(R=4, B=1, N=1)
data[, "weight" := rweights(r=region, b=product, type=~b)]

expect_no_error(
  data[, laspey(p=price, r=region, n=product, w=weight)]
)

expect_no_error(
  data[, paasche(p=price, r=region, n=product, w=weight)]
)

expect_no_error(
  data[, fisher(p=price, r=region, n=product, w=weight)]
)

expect_no_error(
  data[, walsh(p=price, r=region, n=product, w=weight)]
)

expect_no_error(
  data[, toernq(p=price, r=region, n=product, w=weight)]
)

# example data with weights:
set.seed(123)
data <- rdata(R=3, B=1, N=4)
data[, "weight" := rweights(r=region, b=product, type=~b)]

r <- data[, list(region, price/price[region=="1"], weight), by="product"]
L <- r[, weighted.mean(V2, weight), by="region"]
L <- setNames(L$V1, L$region)
P <- r[, 1/weighted.mean(1/V2, weight), by="region"]
P <- setNames(P$V1, P$region)
Fi <- sqrt(L*P)
To <- r[, exp(weighted.mean(log(V2), weight)), by="region"]
To <- setNames(To$V1, To$region)
W <- r[, weighted.mean(sqrt(V2), weight)/weighted.mean(sqrt(1/V2), weight), by="region"]
W <- setNames(W$V1, W$region)

expect_equal(
  data[, laspey(p=price, r=region, n=product, w=weight)],
  L
)

expect_equal(
  data[, paasche(p=price, r=region, n=product, w=weight)],
  P
)

expect_equal(
  data[, fisher(p=price, r=region, n=product, w=weight)],
  Fi
)

expect_equal(
  data[, walsh(p=price, r=region, n=product, w=weight)],
  W
)

expect_equal(
  data[, toernq(p=price, r=region, n=product, w=weight)],
  To
)

# example data with weights and gaps:
set.seed(2)
data <- rdata(R=3, B=1, N=4, gaps=0.2)
data[, "weight" := rweights(r=region, b=product, type=~b)]

r <- data[, list(region, price/price[region=="1"], weight), by="product"]
L <- r[, weighted.mean(V2, weight, na.rm=TRUE), by="region"]
L <- setNames(L$V1, L$region)
P <- r[, 1/weighted.mean(1/V2, weight, na.rm=TRUE), by="region"]
P <- setNames(P$V1, P$region)
Fi <- sqrt(L*P)
To <- r[, exp(weighted.mean(log(V2), weight, na.rm=TRUE)), by="region"]
To <- setNames(To$V1, To$region)
W <- r[, weighted.mean(sqrt(V2), weight, na.rm=TRUE)/weighted.mean(sqrt(1/V2), weight, na.rm=TRUE), by="region"]
W <- setNames(W$V1, W$region)

expect_equal(
  data[, laspey(p=price, r=region, n=product, w=weight, base="1")],
  L
)

expect_equal(
  data[, paasche(p=price, r=region, n=product, w=weight, base="1")],
  P
)

expect_equal(
  data[, fisher(p=price, r=region, n=product, w=weight, base="1")],
  Fi
)

expect_equal(
  data[, toernq(p=price, r=region, n=product, w=weight, base="1")],
  To
)

expect_equal(
  data[, walsh(p=price, r=region, n=product, w=weight, base="1")],
  W
)


# Check consistency of helper functions -----------------------------------


# check if helper function indices produce the
# same results, i.e., if matrix function produces
# the same output as vectorized function

# sample data:
set.seed(1)
dt <- spin::rdata(R=5, B=7, N=1, gaps=0.25)
dt[, "w" := price*quantity/sum(price*quantity), by="region"]
data.table::setnames(dt, c("group","weight","r","n","is_sale","p","q","w"))

# reshape data to matrices:
P <- as.matrix(
  x=data.table::dcast(data=dt, formula=n~r, fun.aggregate=mean, value.var="p", fill=NA),
  rownames="n")

Q <- as.matrix(
  x=data.table::dcast(data=dt, formula=n~r, fun.aggregate=sum, value.var="q", fill=NA),
  rownames="n")

W <- as.matrix(
  x=data.table::dcast(data=dt, formula=n~r, fun.aggregate=mean, value.var="w", fill=NA),
  rownames="n")

# jevons:
system.time(PJ1 <- spin:::.jevons(P=P, Q=Q))
system.time(PJ2 <- dt[, jevons(p=p, r=r, n=n, base="1")])
all.equal(PJ1, PJ2)

# carli:
system.time(PC1 <- spin:::.carli(P=P, Q=Q))
system.time(PC2 <- dt[, carli(p=p, r=r, n=n, base="1")])
all.equal(PC1, PC2)

# dutot:
system.time(PD1 <- spin:::.dutot(P=P, Q=Q))
system.time(PD2 <- dt[, dutot(p=p, r=r, n=n, base="1")])
all.equal(PD1, PD2)

# harmonic:
system.time(PH1 <- spin:::.harmonic(P=P, Q=Q))
system.time(PH2 <- dt[, harmonic(p=p, r=r, n=n, base="1")])
all.equal(PH1, PH2)

# laspeyres:
system.time(PL1 <- spin:::.laspey(P=P, Q=Q))
system.time(PL2 <- dt[, laspey(p=p, q=q, r=r, n=n, base="1")])
all.equal(PL1, PL2)

system.time(PL3 <- spin:::.laspey(P=P, W=W))
system.time(PL4 <- dt[, laspey(p=p, w=w, r=r, n=n, base="1")])
all.equal(PL3, PL4)

# compare weights versus quantities:
all.equal(PL1, PL3)

# paasche:
system.time(PP1 <- spin:::.paasche(P=P, Q=Q))
system.time(PP2 <- dt[, paasche(p=p, q=q, r=r, n=n, base="1")])
all.equal(PP1, PP2)

system.time(PP3 <- spin:::.paasche(P=P, W=W))
system.time(PP4 <- dt[, paasche(p=p, w=w, r=r, n=n, base="1")])
all.equal(PP3, PP4)

# compare weights versus quantities:
all.equal(PP1, PP3)

# fisher:
system.time(PF1 <- spin:::.fisher(P=P, Q=Q))
system.time(PF2 <- dt[, fisher(p=p, q=q, r=r, n=n,base="1")])
all.equal(PF1, PF2)

system.time(PF3 <- spin:::.fisher(P=P, W=W))
system.time(PF4 <- dt[, fisher(p=p, w=w, r=r, n=n, base="1")])
all.equal(PF3, PF4)

# compare weights versus quantities:
all.equal(PF1, PF3)

# walsh:
system.time(PW1 <- spin:::.walsh(P=P, Q=Q))
system.time(PW2 <- dt[, walsh(p=p, q=q, r=r, n=n, base="1")])
all.equal(PW1, PW2)

system.time(PW3 <- spin:::.walsh(P=P, W=W))
system.time(PW4 <- dt[, walsh(p=p, w=w, r=r, n=n, base="1")])
all.equal(PW3, PW4)

# compare weights versus quantities:
all.equal(PW1, PW3)

# toernqvist:
system.time(PT1 <- spin:::.toernq(P=P, Q=Q))
system.time(PT2 <- dt[, toernq(p=p, q=q, r=r, n=n, base="1")])
all.equal(PT1, PT2)

system.time(PT3 <- spin:::.toernq(P=P, W=W))
system.time(PT4 <- dt[, toernq(p=p, w=w, r=r, n=n, base="1")])
all.equal(PT3, PT4)

# compare weights versus quantities:
all.equal(PT1, PT3)

# END

