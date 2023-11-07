# START


# Data with one region only -----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=1, B=1, N=4)

expect_equal(
  dt[, cpd(p=price, r=region, n=product)],
  c("1"=1)
)

expect_equal(
  dt[, nlcpd(p=price, r=region, n=product)],
  c("1"=1)
)


# Data with one product only ----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=4, B=1, N=1)

expect_no_error(
  dt[, cpd(p=price, r=region, n=product)],
)

expect_no_error(
  dt[, nlcpd(p=price, r=region, n=product)]
)

expect_equal(
  dt[, cpd(p=price, r=region, n=product)],
  dt[, nlcpd(p=price, r=region, n=product)]
)


# Data with gaps ----------------------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=3, B=1, N=4, gaps=0.1)
dt[, "share" := price*quantity / sum(price*quantity), by="region"]
dt[, "quantity_share" := quantity / sum(quantity), by="region"]

# CPD method:
cpd.est1 <- dt[, cpd(p=price, r=region, n=product, w=share, base="1")]
cpd.est2 <- dt[, cpd(p=price, r=region, n=product, w=share, base=NULL)]
cpd.est3 <- dt[, cpd(p=price, r=region, n=product, w=share, simplify=FALSE)]

expect_equal(is.vector(cpd.est1), TRUE)
expect_equal(is.vector(cpd.est2), TRUE)
expect_equal(is.list(cpd.est3), TRUE)
expect_equal(cpd.est1[1], c("1"=1))
expect_equal(prod(cpd.est2), 1)
expect_equal(cpd.est1, cpd.est2/cpd.est2[1])

# expenditure share weighted:
expect_equal(
  dt[, cpd(p=price, r=region, n=product, q=quantity)],
  dt[, cpd(p=price, r=region, n=product, w=share)]
)

# only quantity weighting:
cpd.q <- exp(c(0, lm(log(price) ~ product+region-1, data=dt, weights=quantity_share)$coef[5:6]))
cpd.q <- setNames(cpd.q, 1:3)
expect_equal(
  dt[, cpd(p=price, r=region, n=product, w=quantity, base="1")],
  cpd.q
)

# NLCPD method:
nlcpd.est <- dt[, nlcpd(p=price, r=region, n=product, w=share, base=NULL)]
nlcpd.est1 <- dt[, nlcpd(p=price, r=region, n=product, w=share, base="1")]
nlcpd.est2 <- dt[, nlcpd(p=price, r=region, n=product, w=share, base="2")]
nlcpd.est3 <- dt[, nlcpd(p=price, r=region, n=product, w=share, simplify=FALSE)]

# basic functionality:
expect_equal(is.vector(nlcpd.est1), TRUE)
expect_equal(is.vector(nlcpd.est2), TRUE)
expect_equal(is.list(nlcpd.est3), TRUE)
expect_equal(nlcpd.est1[1], c("1"=1))
expect_equal(nlcpd.est2[2], c("2"=1))
expect_equal(prod(nlcpd.est), 1)
expect_equal(nlcpd.est1, nlcpd.est2/nlcpd.est2[1], tolerance=1e-6)
expect_equal(nlcpd.est1, nlcpd.est/nlcpd.est[1], tolerance=1e-6)

# expenditure share weighted:
expect_equal(
  dt[, nlcpd(p=price, r=region, n=product, q=quantity)],
  dt[, nlcpd(p=price, r=region, n=product, w=share)]
)

# only quantity weighting:
expect_equal(
  dt[, nlcpd(p=price, r=region, n=product, w=quantity_share, base="1", simplify=TRUE,
               upper=c(rep(Inf, 6), rep(1, 3)), lower=c(rep(-Inf, 6), rep(1, 3)))],
  cpd.q,
  tolerance=1e-5
)


# Settings ----------------------------------------------------------------


expect_error(
  dt[, cpd(p=price, r=region, n=product, settings=list(chatty="abc"))]
)

expect_error(
  dt[, nlcpd(p=price, r=region, n=product, settings=list(chatty="abc"))]
)

expect_error(
  dt[, cpd(p=price, r=region, n=product, settings=list(connect="abc"))]
)

expect_error(
  dt[, nlcpd(p=price, r=region, n=product, settings=list(connect="abc"))]
)

expect_error(
  dt[, nlcpd(p=price, r=region, n=product, settings=list(self.start="abc"))]
)

expect_no_error(
  dt[, nlcpd(p=price, r=region, n=product, settings=list(self.start="s2"))]
)

expect_no_error(
  dt[, nlcpd(p=price, r=region, n=product,
             settings=list(w.delta=c("1"=0.3,"2"=0.5,"3"=0.1,"4"=0.1)))]
)

expect_error(
  dt[, nlcpd(p=price, r=region, n=product, settings=list(w.delta=c("1"=0.2)))]
)

# valid parameter start values:
par.start <- list("lnP"=setNames(rep(0, nlevels(dt$region)-1), levels(dt$region)[-1]),
                  "pi"=setNames(rep(1, nlevels(dt$product)), levels(dt$product)),
                  "delta"=setNames(rep(1, nlevels(dt$product)-1), levels(dt$product)[-1]))

expect_no_error(
  dt[, nlcpd(p=price, r=region, n=product, settings=list(par.start=par.start))]
)

# missing names:
par.start <- list("lnP"=rep(0, nlevels(dt$region)-1),
                  "pi"=rep(1, nlevels(dt$product)),
                  "delta"=rep(1, nlevels(dt$product)-1))

expect_error(
  dt[, nlcpd(p=price, r=region, n=product, settings=list(par.start=par.start))]
)

# length of 'lnP' smaller than R-1:
par.start <- list("lnP"=setNames(rep(0, nlevels(dt$region)-2), levels(dt$region)[-c(1:2)]),
                  "pi"=setNames(rep(1, nlevels(dt$product)), levels(dt$product)),
                  "delta"=setNames(rep(1, nlevels(dt$product)-1), levels(dt$product)[-1]))

expect_error(
  dt[, nlcpd(p=price, r=region, n=product, settings=list(par.start=par.start))]
)

# length of 'lnP' greater than R-1 but all names present in 'levels(r)':
par.start <- list("lnP"=setNames(rep(0, nlevels(dt$region)+1), c(levels(dt$region),"test")),
                  "pi"=setNames(rep(1, nlevels(dt$product)), levels(dt$product)),
                  "delta"=setNames(rep(1, nlevels(dt$product)-1), levels(dt$product)[-1]))

expect_no_error(
  dt[, nlcpd(p=price, r=region, n=product, settings=list(par.start=par.start))]
)


# Non-connected data ------------------------------------------------------


# example data:
set.seed(123)
dt1 <- spin::rdata(R=3, B=1, N=5)
dt2 <- spin::rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)

# CPD method:
expect_equal(
  dt[, cpd(p=price, r=region, n=product, q=quantity, base="1",
              settings=list(chatty=FALSE, connect=TRUE))][1],
  c("1"=1)
)

expect_equal(
  dt[, cpd(p=price, r=region, n=product, q=quantity, base="1",
              settings=list(chatty=FALSE, connect=TRUE))][4:7],
  setNames(rep(NA_real_, 4), 4:7)
)

expect_equal(
  dt[, cpd(p=price, r=region, n=product, q=quantity, base="4",
              settings=list(chatty=FALSE, connect=TRUE))][1:3],
  setNames(rep(NA_real_, 3), 1:3)
)

expect_equal(
  dt[, cpd(p=price, r=region, n=product, q=quantity, base="4",
              settings=list(chatty=FALSE, connect=TRUE))][4],
  c("4"=1)
)

# NLCPD method:
expect_equal(
  dt[, nlcpd(p=price, r=region, n=product, q=quantity, base="1",
           settings=list(chatty=FALSE, connect=TRUE))][1],
  c("1"=1)
)

expect_equal(
  dt[, nlcpd(p=price, r=region, n=product, q=quantity, base="1",
           settings=list(chatty=FALSE, connect=TRUE))][4:7],
  setNames(rep(NA_real_, 4), 4:7)
)

expect_equal(
  dt[, nlcpd(p=price, r=region, n=product, q=quantity, base="4",
           settings=list(chatty=FALSE, connect=TRUE))][1:3],
  setNames(rep(NA_real_, 3), 1:3)
)

expect_equal(
  dt[, nlcpd(p=price, r=region, n=product, q=quantity, base="4",
           settings=list(chatty=FALSE, connect=TRUE))][4],
  c("4"=1)
)


# Misc --------------------------------------------------------------------


# example data with no gaps:
set.seed(123)
dt <- rdata(R=3, B=1, N=4)

# CPD method:
cpd.est <- dt[, cpd(p=price, r=region, n=product, base=NULL)]
cpd.est1 <- dt[, cpd(p=price, r=region, n=product, base="1")]
cpd.est2 <- dt[, cpd(p=price, r=region, n=product, base="2")]
jev <- dt[, jevons(p=price, r=region, n=product, base="1")]

expect_equal(cpd.est1[1], c("1"=1))
expect_equal(cpd.est2[2], c("2"=1))
expect_equal(prod(cpd.est), 1)
expect_equal(cpd.est1, cpd.est2/cpd.est2[1])
expect_equal(cpd.est1, cpd.est/cpd.est[1])
expect_equal(cpd.est1, jev)

# END
