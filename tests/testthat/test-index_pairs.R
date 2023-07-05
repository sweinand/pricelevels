# START

# example data with one region only:
set.seed(123)
data <- prices(R=1, N=4)

expect_error(
  data[, index.pairs(x=price, r=region, n=product, type="bla")]
)

expect_equal(
  is.matrix(data[, index.pairs(x=price, r=region, n=product, type="jevons")]),
  TRUE
)

expect_equal(
  data[, index.pairs(x=price, r=region, n=product, type="jevons")],
  matrix(data=1, nrow=1, ncol=1, dimnames=list("r1","r1"))
)

# example data with one product only:
set.seed(123)
data <- prices(R=4, N=1)

expect_no_error(
  data[, index.pairs(x=price, r=region, n=product, type="jevons")]
)

# example data with gaps:
set.seed(123)
data <- prices(R=3, N=4, gaps=0.2)

res.expec <- rbind(
  data[, jevons(x=price, r=region, n=product, base="r1")],
  data[, jevons(x=price, r=region, n=product, base="r2")],
  data[, jevons(x=price, r=region, n=product, base="r3")]
)
rownames(res.expec) <- c("r1","r2","r3")

expect_equal(
  data[, index.pairs(x=price, r=region, n=product, type="jevons")],
  res.expec
)

res.expec[lower.tri(res.expec)] <- NA

expect_equal(
  data[, index.pairs(x=price, r=region, n=product, type="jevons", all.pairs=FALSE)],
  res.expec
)

expect_equal(
  nrow(data[, index.pairs(x=price, r=region, n=product, type="jevons", all.pairs=TRUE, as.dt=TRUE)]),
  3^2
)

expect_equal(
  nrow(data[, index.pairs(x=price, r=region, n=product, type="jevons", all.pairs=FALSE, as.dt=TRUE)]),
  3*(3+1)/2
)

# no weights:
data <- prices(R = 5, N = 10)
data[, "weights" := 1]

expect_equal(
  data[, index.pairs(x=price, r=region, n=product, type="jevons")],
  data[, index.pairs(price, region, product, w=weights, type="toernq")]
)

expect_equal(
  data[, index.pairs(price, region, product, type = "carli")],
  data[, index.pairs(price, region, product, weights, type = "laspey")]
)

expect_equal(
  data[, index.pairs(price, region, product, type = "harmonic")],
  data[, index.pairs(price, region, product, weights, type = "paasche")]
)

# END
