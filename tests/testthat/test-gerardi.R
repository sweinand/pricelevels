# START


# Data with one region only -----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=1, B=1, N=4)

expect_equal(
  dt[, gerardi(p=price, q=quantity, r=region, n=product)],
  c("1"=1)
)

expect_equal(
  dt[, gerardi(p=price, q=quantity, r=region, n=product, settings=list("variant"="adjusted"))],
  c("1"=1)
)


# Data with one product only ----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=4, B=1, N=1)

expect_no_error(
  dt[, gerardi(p=price, q=quantity, r=region, n=product)]
)

expect_no_error(
  dt[, gerardi(p=price, q=quantity, r=region, n=product, settings=list("variant"="adjusted"))]
)


# Data with gaps ----------------------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=3, B=1, N=4, gaps=0.2)

# variant=original:
P.est <- dt[, gerardi(p=price, q=quantity, r=region, n=product, base=NULL)]
P.est1 <- dt[, gerardi(p=price, q=quantity, r=region, n=product, base="1")]
P.est2 <- dt[, gerardi(p=price, q=quantity, r=region, n=product, base="2")]
P.est3 <- dt[, gerardi(p=price, q=quantity, r=region, n=product, simplify=FALSE)]

expect_equal(is.vector(P.est1), TRUE)
expect_equal(is.vector(P.est2), TRUE)
expect_equal(is.vector(P.est3), TRUE)
expect_true(length(P.est3)>length(P.est1))
expect_equal(P.est1[1], c("1"=1))
expect_equal(P.est2[2], c("2"=1))
expect_equal(mean(P.est), 1)
expect_equal(P.est1, P.est/P.est[1])
expect_equal(P.est1, P.est2/P.est2[1])

# variant=adjusted:
P.est <- dt[, gerardi(p=price, q=quantity, r=region, n=product, base=NULL, settings=list("variant"="adjusted"))]
P.est1 <- dt[, gerardi(p=price, q=quantity, r=region, n=product, base="1", settings=list("variant"="adjusted"))]
P.est2 <- dt[, gerardi(p=price, q=quantity, r=region, n=product, base="2", settings=list("variant"="adjusted"))]
P.est3 <- dt[, gerardi(p=price, q=quantity, r=region, n=product, simplify=FALSE, settings=list("variant"="adjusted"))]

expect_equal(is.vector(P.est1), TRUE)
expect_equal(is.vector(P.est2), TRUE)
expect_equal(is.vector(P.est3), TRUE)
expect_true(length(P.est3)>length(P.est1))
expect_equal(P.est1[1], c("1"=1))
expect_equal(P.est2[2], c("2"=1))
expect_equal(mean(P.est), 1)
expect_equal(P.est1, P.est/P.est[1])
expect_equal(P.est1, P.est2/P.est2[1])

# test quantities versus shares as weights:
dt[, "share" := (price*quantity)/sum(price*quantity), by="region"]

expect_equal(
  dt[, gerardi(p=price, q=quantity, r=region, n=product, base=NULL)],
  dt[, gerardi(p=price, w=share, r=region, n=product, base=NULL)]
)

expect_equal(
  dt[, gerardi(p=price, q=quantity, r=region, n=product, base=NULL, settings=list("variant"="adjusted"))],
  dt[, gerardi(p=price, w=share, r=region, n=product, base=NULL, settings=list("variant"="adjusted"))]
)


# Settings ----------------------------------------------------------------


# missing quantities and weights:
expect_error(
  dt[, gerardi(p=price, r=region, n=product)]
)

# connect not allowed:
expect_error(
  dt[, gerardi(p=price, q=quantity, r=region, n=product, settings=list(connect="abc"))]
)

# variant not allowed:
expect_error(
  dt[, gerardi(p=price, q=quantity, r=region, n=product, settings=list(variant="abc"))]
)


# Non-connected data ------------------------------------------------------


# example data:
set.seed(123)
dt1 <- pricelevels::rdata(R=3, B=1, N=5)
dt2 <- pricelevels::rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)

expect_equal(
  dt[, gerardi(p=price, r=region, n=product, q=quantity, base="1",
           settings=list(chatty=FALSE, connect=TRUE))][1],
  c("1"=1)
)

expect_equal(
  dt[, gerardi(p=price, r=region, n=product, q=quantity, base="1",
           settings=list(chatty=FALSE, connect=TRUE))][4:7],
  setNames(rep(NA_real_, 4), 4:7)
)

expect_equal(
  dt[, gerardi(p=price, r=region, n=product, q=quantity, base="4",
           settings=list(chatty=FALSE, connect=TRUE))][1:3],
  setNames(rep(NA_real_, 3), 1:3)
)

expect_equal(
  dt[, gerardi(p=price, r=region, n=product, q=quantity, base="4",
           settings=list(chatty=FALSE, connect=TRUE))][4],
  c("4"=1)
)

expect_equal(
  dt[, gerardi(p=price, r=region, n=product, q=quantity, base="1",
             settings=list(chatty=FALSE, connect=TRUE, solve="matrix"))][1],
  c("1"=1)
)

expect_equal(
  dt[, gerardi(p=price, r=region, n=product, q=quantity, base="1",
             settings=list(chatty=FALSE, connect=TRUE, solve="matrix"))][4:7],
  setNames(rep(NA_real_, 4), 4:7)
)

expect_equal(
  dt[, gerardi(p=price, r=region, n=product, q=quantity, base="4",
             settings=list(chatty=FALSE, connect=TRUE, solve="matrix"))][1:3],
  setNames(rep(NA_real_, 3), 1:3)
)

expect_equal(
  dt[, gerardi(p=price, r=region, n=product, q=quantity, base="4",
             settings=list(chatty=FALSE, connect=TRUE, solve="matrix"))][4],
  c("4"=1)
)

# END
