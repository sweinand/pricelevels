# START


# index.pairs() -----------------------------------------------------------


# example data with one region only:
set.seed(123)
data <- rdata(R=1, N=4)

expect_error(
  data[, index.pairs(p=price, r=region, n=product, type="bla")]
)

expect_error(
  data[, index.pairs(p=price, r=region, n=product, type="laspey")]
)

expect_equal(
  is.matrix(data[, index.pairs(p=price, r=region, n=product, type="jevons")]),
  TRUE
)

expect_equal(
  data[, index.pairs(p=price, r=region, n=product, type="jevons")],
  matrix(data=1, nrow=1, ncol=1, dimnames=list("1","1"))
)

# example data with one product only:
set.seed(123)
data <- rdata(R=4, N=1)

expect_no_error(
  data[, index.pairs(p=price, r=region, n=product, type="jevons")]
)

# example data with gaps:
set.seed(123)
data <- rdata(R=3, N=4, gaps=0.2)

res.expec <- rbind(
  data[, jevons(p=price, r=region, n=product, base="1")],
  data[, jevons(p=price, r=region, n=product, base="2")],
  data[, jevons(p=price, r=region, n=product, base="3")]
)
rownames(res.expec) <- c("1","2","3")

expect_equal(
  data[, index.pairs(p=price, r=region, n=product, type="jevons")],
  res.expec
)

res.expec[lower.tri(res.expec)] <- NA

expect_equal(
  data[, index.pairs(p=price, r=region, n=product, type="jevons", all.pairs=FALSE)],
  res.expec
)

expect_equal(
  nrow(data[, index.pairs(p=price, r=region, n=product, type="jevons", all.pairs=TRUE, as.dt=TRUE)]),
  3^2
)

expect_equal(
  nrow(data[, index.pairs(p=price, r=region, n=product, type="jevons", all.pairs=FALSE, as.dt=TRUE)]),
  3*(3+1)/2
)

# no weights:
data <- rdata(R = 5, N = 10)
data[, "weights" := 1]

expect_equal(
  data[, index.pairs(p=price, r=region, n=product, type="jevons")],
  data[, index.pairs(price, region, product, w=weights, type="toernq")]
)

expect_equal(
  data[, index.pairs(price, region, product, type = "carli")],
  data[, index.pairs(price, region, product, w=weights, type = "laspey")]
)

expect_equal(
  data[, index.pairs(price, region, product, type = "harmonic")],
  data[, index.pairs(price, region, product, w=weights, type = "paasche")]
)


# geks() ------------------------------------------------------------------


# example data with one region only:
set.seed(123)
data <- rdata(R=1, N=4)

expect_equal(
  data[, geks(p=price, r=region, n=product)],
  c("1"=1)
)

# example data with one product only:
set.seed(123)
data <- rdata(R=4, N=1)

expect_no_error(
  data[, geks(p=price, r=region, n=product)],
)

# example data without weights and gaps:
set.seed(123)
data <- rdata(R=3, N=4)

geks.est1 <- data[, geks(p=price, r=region, n=product, base="1")]
geks.est2 <- data[, geks(p=price, r=region, n=product, base=NULL)]
jev.est1 <- data[, jevons(p=price, r=region, n=product, base="1")]

expect_equal(geks.est1[1], c("1"=1))
expect_equal(prod(geks.est2), 1)
expect_equal(geks.est1, geks.est2/geks.est2[1])
expect_equal(geks.est1, jev.est1)

# example data without gaps, but with weights:
set.seed(123)
data <- rdata(R=3, N=4)
data[, "weight" := rweights(r=region, n=product, type=~n)]

geks.est1 <- data[, geks(p=price, r=region, n=product, w=weight, type="toernq", base="1")]
geks.est2 <- data[, geks(p=price, r=region, n=product, w=weight, type="toernq", base=NULL)]
toernq.est1 <- data[, toernq(p=price, r=region, n=product, w=weight, base="1")]

expect_equal(geks.est1[1], c("1"=1))
expect_equal(prod(geks.est2), 1)
expect_equal(geks.est1, geks.est2/geks.est2[1])
expect_equal(geks.est1, toernq.est1)

# example data with weights and gaps:
set.seed(123)
data <- rdata(R=3, N=4, gaps=0.2)
data[, "weight" := rweights(r=region, n=product, type=~n)]

geks.est1 <- data[, geks(p=price, r=region, n=product, w=weight, type="toernq", base="1")]
geks.est2 <- data[, geks(p=price, r=region, n=product, w=weight, type="toernq", base=NULL)]

expect_equal(is.vector(geks.est1), TRUE)
expect_equal(is.vector(geks.est2), TRUE)
expect_equal(geks.est1[1], c("1"=1))
expect_equal(prod(geks.est2), 1)
expect_equal(geks.est1, geks.est2/geks.est2[1])

# END
