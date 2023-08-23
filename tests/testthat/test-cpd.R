# START

# example data with one region only:
set.seed(123)
data <- prices(R=1, N=4)

expect_equal(
  data[, cpd(p=price, r=region, n=product)],
  c("r1"=1)
)

expect_equal(
  data[, nlcpd(p=price, r=region, n=product)],
  c("r1"=1)
)

# example data with one product only:
set.seed(123)
data <- prices(R=4, N=1)

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
data <- prices(R=3, N=4)


# CPD method:
cpd.est1 <- data[, cpd(p=price, r=region, n=product, base="r1")]
cpd.est2 <- data[, cpd(p=price, r=region, n=product, base=NULL)]
jev <- data[, jevons(p=price, r=region, n=product, base="r1")]

expect_equal(cpd.est1[1], c("r1"=1))
expect_equal(prod(cpd.est2), 1)
expect_equal(cpd.est1, cpd.est2/cpd.est2[1])
expect_equal(cpd.est1, jev)


# example data with weights and gaps:
set.seed(123)
data <- prices(R=3, N=4, weights=~n, gaps=0.2)


# CPD method:
cpd.est1 <- data[, cpd(p=price, r=region, n=product, w=weight, base="r1")]
cpd.est2 <- data[, cpd(p=price, r=region, n=product, w=weight, base=NULL)]
cpd.est3 <- data[, cpd(p=price, r=region, n=product, w=weight, simplify=FALSE)]

expect_equal(is.vector(cpd.est1), TRUE)
expect_equal(is.vector(cpd.est2), TRUE)
expect_equal(is.list(cpd.est3), TRUE)
expect_equal(cpd.est1[1], c("r1"=1))
expect_equal(prod(cpd.est2), 1)
expect_equal(cpd.est1, cpd.est2/cpd.est2[1])


# NLCPD method:
nlcpd.est1 <- data[, nlcpd(p=price, r=region, n=product, w=weight, base="r1")]
nlcpd.est2 <- data[, nlcpd(p=price, r=region, n=product, w=weight, base=NULL)]
nlcpd.est3 <- data[, nlcpd(p=price, r=region, n=product, w=weight, simplify=FALSE)]

# basic functionality:
expect_equal(is.vector(nlcpd.est1), TRUE)
expect_equal(is.vector(nlcpd.est2), TRUE)
expect_equal(is.list(nlcpd.est3), TRUE)
expect_equal(nlcpd.est1[1], c("r1"=1))
expect_equal(prod(nlcpd.est2), 1)
expect_equal(nlcpd.est1, nlcpd.est2/nlcpd.est2[1])

# settings:
expect_error(
  data[, nlcpd(p=price, r=region, n=product, settings=list(self.start="abc"))]
)

expect_no_error(
  data[, nlcpd(p=price, r=region, n=product, settings=list(self.start="s2"))]
)

expect_no_error(
  data[, nlcpd(p=price, r=region, n=product,
               settings=list(w.delta=c("n1"=0.3,"n2"=0.5,"n3"=0.1,"n4"=0.1)))]
)

expect_error(
  data[, nlcpd(p=price, r=region, n=product,
               settings=list(w.delta=c("n1"=0.2)))]
)

# END
