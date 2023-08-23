# START

# example data with one region only:
set.seed(123)
data <- prices(R=1, N=4)

expect_equal(
  data[, jevons(p=price, r=region, n=product)],
  c("r1"=1)
)

expect_equal(
  data[, carli(p=price, r=region, n=product)],
  c("r1"=1)
)

expect_equal(
  data[, dutot(p=price, r=region, n=product)],
  c("r1"=1)
)

expect_equal(
  data[, harmonic(p=price, r=region, n=product)],
  c("r1"=1)
)

# example data with one product only:
set.seed(123)
data <- prices(R=4, N=1)

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
data <- prices(R=3, N=4)

r <- data[, list(region, price/price[region=="r1"]), by="product"]
J <- r[, exp(mean(log(V2))), by="region"]
J <- setNames(J$V1, J$region)
C <- r[, mean(V2), by="region"]
C <- setNames(C$V1, C$region)
H <- r[, 1/mean(1/V2), by="region"]
H <- setNames(H$V1, H$region)
D <- data[, mean(price), by="region"]
D <- setNames(D$V1/D$V1[D$region=="r1"], D$region)

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
set.seed(123)
data <- prices(R=3, N=4, gaps=0.2)

r <- data[, list(region, price/price[region=="r1"]), by="product"]
J <- r[, exp(mean(log(V2))), by="region"]
J <- setNames(J$V1, J$region)
C <- r[, mean(V2), by="region"]
C <- setNames(C$V1, C$region)
H <- r[, 1/mean(1/V2), by="region"]
H <- setNames(H$V1, H$region)
D1 <- data[product%in%intersect(data[region=="r1", product], data[region=="r1", product]) & region%in%c("r1"), mean(price[region=="r1"])/mean(price[region=="r1"])]
D2 <- data[product%in%intersect(data[region=="r1", product], data[region=="r2", product]) & region%in%c("r1","r2"), mean(price[region=="r2"])/mean(price[region=="r1"])]
D3 <- data[product%in%intersect(data[region=="r1", product], data[region=="r3", product]) & region%in%c("r1","r3"), mean(price[region=="r3"])/mean(price[region=="r1"])]
D <- setNames(c(D1,D2,D3), c("r1","r2","r3"))

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

# END
