# START

# example data with one region only:
set.seed(123)
data <- prices(R=1, N=4, w=~n)

expect_equal(
  data[, laspey(p=price, r=region, n=product, w=weight)],
  c("r1"=1)
)

expect_equal(
  data[, paasche(p=price, r=region, n=product, w=weight)],
  c("r1"=1)
)

expect_equal(
  data[, walsh(p=price, r=region, n=product, w=weight)],
  c("r1"=1)
)

expect_equal(
  data[, fisher(p=price, r=region, n=product, w=weight)],
  c("r1"=1)
)

expect_equal(
  data[, toernq(p=price, r=region, n=product, w=weight)],
  c("r1"=1)
)

# example data with one product only:
set.seed(123)
data <- prices(R=4, N=1, weights=~n)

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
data <- prices(R=3, N=4, weights=~n)

r <- data[, list(region, price/price[region=="r1"], weight), by="product"]
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
set.seed(123)
data <- prices(R=3, N=4, weights=~n, gaps=0.2)

r <- data[, list(region, price/price[region=="r1"], weight), by="product"]
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
  data[, laspey(p=price, r=region, n=product, w=weight, base="r1")],
  L
)

expect_equal(
  data[, paasche(p=price, r=region, n=product, w=weight, base="r1")],
  P
)

expect_equal(
  data[, fisher(p=price, r=region, n=product, w=weight, base="r1")],
  Fi
)

expect_equal(
  data[, toernq(p=price, r=region, n=product, w=weight, base="r1")],
  To
)

expect_equal(
  data[, walsh(p=price, r=region, n=product, w=weight, base="r1")],
  W
)

# END
