# START

# example data with one region only:
set.seed(123)
data <- prices(R=1, N=4)

expect_equal(
  data[, geks(p=price, r=region, n=product)],
  c("r1"=1)
)

# example data with one product only:
set.seed(123)
data <- prices(R=4, N=1)

expect_no_error(
  data[, geks(p=price, r=region, n=product)],
)

# example data without weights and gaps:
set.seed(123)
data <- prices(R=3, N=4)

geks.est1 <- data[, geks(p=price, r=region, n=product, base="r1")]
geks.est2 <- data[, geks(p=price, r=region, n=product, base=NULL)]
jev.est1 <- data[, jevons(p=price, r=region, n=product, base="r1")]

expect_equal(geks.est1[1], c("r1"=1))
expect_equal(prod(geks.est2), 1)
expect_equal(geks.est1, geks.est2/geks.est2[1])
expect_equal(geks.est1, jev.est1)

# example data without gaps, but with weights:
set.seed(123)
data <- prices(R=3, N=4, weights=~n)

geks.est1 <- data[, geks(p=price, r=region, n=product, w=weight, type="toernq", base="r1")]
geks.est2 <- data[, geks(p=price, r=region, n=product, w=weight, type="toernq", base=NULL)]
toernq.est1 <- data[, toernq(p=price, r=region, n=product, w=weight, base="r1")]

expect_equal(geks.est1[1], c("r1"=1))
expect_equal(prod(geks.est2), 1)
expect_equal(geks.est1, geks.est2/geks.est2[1])
expect_equal(geks.est1, toernq.est1)

# example data with weights and gaps:
set.seed(123)
data <- prices(R=3, N=4, weights=~n, gaps=0.2)

geks.est1 <- data[, geks(p=price, r=region, n=product, w=weight, type="toernq", base="r1")]
geks.est2 <- data[, geks(p=price, r=region, n=product, w=weight, type="toernq", base=NULL)]

expect_equal(is.vector(geks.est1), TRUE)
expect_equal(is.vector(geks.est2), TRUE)
expect_equal(geks.est1[1], c("r1"=1))
expect_equal(prod(geks.est2), 1)
expect_equal(geks.est1, geks.est2/geks.est2[1])

# END
