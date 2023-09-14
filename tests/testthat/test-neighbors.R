# START

### connected price data, no gaps:
set.seed(123)
data1 <- rdata(R=4, N=3)

expect_equal(
  data1[, sparsity(r = region, n = product)],
  0
)

expect_true(
  data1[, is.connected(r = region, n = product)] # true
)

expect_equal(
  data1[, neighbors(r = region, n = product, simplify = TRUE)],
  factor(x=rep(1, nrow(data1)))
)

expect_equal(
  nrow(data1[, comparisons(r = region, n = product)]),
  1
)

expect_true(
  all(data1[, connect(r=region, n=product)])
)


### connected price data, gaps:
set.seed(123)
data2 <- rdata(R=4, N=3, gaps=0.25)

expect_equal(
  data2[, sparsity(r = region, n = product)],
  0.25
)

expect_true(
  data2[, is.connected(r = region, n = product)] # true
)

expect_equal(
  data2[, neighbors(r = region, n = product, simplify = TRUE)],
  factor(x=rep(1, nrow(data2)))
)

expect_equal(
  nrow(data2[, comparisons(r = region, n = product)]),
  1
)

expect_true(
  all(data2[, connect(r=region, n=product)])
)


### non-connected price data:
data3 <- data.table::data.table(
  "region" = c("a","a","h","b","a","a","c","c","d","e","e","f",NA),
  "product" = c(1,1,"bla",1,2,3,3,4,4,5,6,6,7),
  stringsAsFactors = TRUE)

expect_equal(
  data3[, sparsity(r = region, n = product)],
  1-sum(complete.cases(unique(data3)))/(nlevels(data3$region)*nlevels(data3$product))
)

expect_false(
  data3[, is.connected(r = region, n = product)] # false
)

expect_equal(
  data3[, neighbors(r = region, n = product, simplify = TRUE)],
  factor(x=c(1,1,2,1,1,1,1,1,1,3,3,3,NA))
)

expect_equal(
  nrow(data3[, comparisons(r = region, n = product)]),
  3
)

expect_equal(
  data3[, connect(r=region, n=product)],
  data3[, region%in%c("a","b","c","d")]
)

# END
