# START

# example data with one region only:
set.seed(123)
data <- rdata(R=1, B=1, N=4)
data[, "quantity" := 1]

expect_equal(
  data[, gk(p=price, q=quantity, r=region, n=product)],
  c("1"=1)
)

expect_equal(
  data[, gk(p=price, q=quantity, r=region, n=product, settings=list("method"="iter"))],
  c("1"=1)
)

# example data with one product only:
set.seed(123)
data <- rdata(R=4, B=1, N=1)
data[, "quantity" := 1]

expect_no_error(
  data[, gk(p=price, q=quantity, r=region, n=product)]
)

expect_no_error(
  data[, gk(p=price, q=quantity, r=region, n=product, settings=list("method"="iter"))]
)

expect_equal(
  data[, gk(p=price, q=quantity, r=region, n=product)],
  data[, gk(p=price, q=quantity, r=region, n=product, settings=list("method"="iter"))]
)


# example data with gaps:
set.seed(123)
data <- rdata(R=3, B=1, N=4, gaps=0.2)
data[, "quantity" := 1]

gk.est1 <- data[, gk(p=price, q=quantity, r=region, n=product, base="1")]
gk.est2 <- data[, gk(p=price, q=quantity, r=region, n=product, base=NULL)]
gk.est3 <- data[, gk(p=price, q=quantity, r=region, n=product, simplify=FALSE)]

expect_equal(is.vector(gk.est1), TRUE)
expect_equal(is.vector(gk.est2), TRUE)
expect_equal(is.vector(gk.est3), TRUE)
expect_equal(gk.est1[1], c("1"=1))
expect_equal(mean(gk.est2), 1)
expect_equal(gk.est1, gk.est2/gk.est2[1])

# settings:
expect_error(
  data[, gk(p=price, q=quantity, r=region, n=product, settings=list(method="abc"))]
)

expect_no_error(
  data[, gk(p=price, q=quantity, r=region, n=product, settings=list(method="iter"))]
)

expect_error(
  data[, gk(p=price, q=quantity, r=region, n=product, settings=list(tol=-2))]
)

expect_error(
  data[, gk(p=price, q=quantity, r=region, n=product, settings=list(max.iter=-2))]
)

# END
