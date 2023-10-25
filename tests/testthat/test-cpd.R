# START

# example data with one region only:
set.seed(123)
data <- rdata(R=1, B=1, N=4)

expect_equal(
  data[, cpd(p=price, r=region, n=product)],
  c("1"=1)
)

expect_equal(
  data[, nlcpd(p=price, r=region, n=product)],
  c("1"=1)
)

# example data with one product only:
set.seed(123)
data <- rdata(R=4, B=1, N=1)

expect_no_error(
  data[, cpd(p=price, r=region, n=product)],
)

expect_no_error(
  data[, nlcpd(p=price, r=region, n=product)]
)

expect_equal(
  data[, cpd(p=price, r=region, n=product)],
  data[, nlcpd(p=price, r=region, n=product)]
)

# example data without weights and gaps:
set.seed(123)
data <- rdata(R=3, B=1, N=4)

# CPD method:
cpd.est <- data[, cpd(p=price, r=region, n=product, base=NULL)]
cpd.est1 <- data[, cpd(p=price, r=region, n=product, base="1")]
cpd.est2 <- data[, cpd(p=price, r=region, n=product, base="2")]
jev <- data[, jevons(p=price, r=region, n=product, base="1")]

expect_equal(cpd.est1[1], c("1"=1))
expect_equal(cpd.est2[2], c("2"=1))
expect_equal(prod(cpd.est), 1)
expect_equal(cpd.est1, cpd.est2/cpd.est2[1])
expect_equal(cpd.est1, cpd.est/cpd.est[1])
expect_equal(cpd.est1, jev)

# example data with weights and gaps:
set.seed(123)
data <- rdata(R=3, B=1, N=4, gaps=0.1)
data[, "share" := price*quantity / sum(price*quantity), by="region"]

# CPD method:
cpd.est1 <- data[, cpd(p=price, r=region, n=product, w=share, base="1")]
cpd.est2 <- data[, cpd(p=price, r=region, n=product, w=share, base=NULL)]
cpd.est3 <- data[, cpd(p=price, r=region, n=product, w=share, simplify=FALSE)]

expect_equal(is.vector(cpd.est1), TRUE)
expect_equal(is.vector(cpd.est2), TRUE)
expect_equal(is.list(cpd.est3), TRUE)
expect_equal(cpd.est1[1], c("1"=1))
expect_equal(prod(cpd.est2), 1)
expect_equal(cpd.est1, cpd.est2/cpd.est2[1])

# expenditure share weighted:
expect_equal(
  data[, cpd(p=price, r=region, n=product, q=quantity)],
  data[, cpd(p=price, r=region, n=product, w=share)]
)

# only quantity weighting:
cpd.q <- exp(c(0, lm(log(price) ~ product+region-1, data=data, weights=quantity)$coef[5:6]))
cpd.q <- setNames(cpd.q, 1:3)
expect_equal(
  data[, cpd(p=price, r=region, n=product, w=quantity, base="1")],
  cpd.q
)

# NLCPD method:
nlcpd.est <- data[, nlcpd(p=price, r=region, n=product, w=share, base=NULL)]
nlcpd.est1 <- data[, nlcpd(p=price, r=region, n=product, w=share, base="1")]
nlcpd.est2 <- data[, nlcpd(p=price, r=region, n=product, w=share, base="2")]
nlcpd.est3 <- data[, nlcpd(p=price, r=region, n=product, w=share, simplify=FALSE)]

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
  data[, nlcpd(p=price, r=region, n=product, q=quantity)],
  data[, nlcpd(p=price, r=region, n=product, w=share)]
)

# only quantity weighting:
expect_equal(
  data[, nlcpd(p=price, r=region, n=product, w=quantity, base="1", simplify=TRUE,
               upper=c(rep(Inf, 6), rep(1, 3)), lower=c(rep(-Inf, 6), rep(1, 3)))],
  cpd.q,
  tolerance=1e-5
)

# settings:
expect_error(
  data[, nlcpd(p=price, r=region, n=product, settings=list(self.start="abc"))]
)

expect_no_error(
  data[, nlcpd(p=price, r=region, n=product, settings=list(self.start="s2"))]
)

expect_no_error(
  data[, nlcpd(p=price, r=region, n=product,
               settings=list(w.delta=c("1"=0.3,"2"=0.5,"3"=0.1,"4"=0.1)))]
)

expect_error(
  data[, nlcpd(p=price, r=region, n=product,
               settings=list(w.delta=c("1"=0.2)))]
)

# END
