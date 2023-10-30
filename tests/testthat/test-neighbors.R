# START


# Data with one region only -----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=1, B=1, N=4)

expect_equal(dt[, sparsity(r = region, n = product)], 0)
expect_true(dt[, is.connected(r = region, n = product)])
expect_no_error(dt[, connect(r = region, n = product)])
expect_no_error(dt[, neighbors(r = region, n = product, simplify=TRUE)])
expect_no_error(dt[, comparisons(r = region, n = product)])


# Data with one product only ----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=4, B=1, N=1)

expect_equal(dt[, sparsity(r = region, n = product)], 0)
expect_true(dt[, is.connected(r = region, n = product)])
expect_no_error(dt[, connect(r = region, n = product)])
expect_no_error(dt[, neighbors(r = region, n = product, simplify=TRUE)])
expect_no_error(dt[, comparisons(r = region, n = product)])


# Data with gaps ----------------------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=4, B=1, N=3, gaps=0.25)

expect_equal(dt[, sparsity(r = region, n = product)], 0.25)
expect_true(dt[, is.connected(r = region, n = product)])
expect_true(all(dt[, connect(r=region, n=product)]))
expect_equal(nrow(dt[, comparisons(r = region, n = product)]), 1)
expect_equal(
  dt[, neighbors(r = region, n = product, simplify = TRUE)],
  factor(x=rep(1, nrow(dt)))
)


# Non-connected data ------------------------------------------------------


# example data:
set.seed(123)
dt1 <- spin::rdata(R=3, B=1, N=5)
dt2 <- spin::rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)

# data3 <- data.table::data.table(
#   "region" = c("a","a","h","b","a","a","c","c","d","e","e","f",NA),
#   "product" = c(1,1,"bla",1,2,3,3,4,4,5,6,6,7),
#   stringsAsFactors = TRUE)

expect_equal(
  dt[, sparsity(r = region, n = product)],
  1-sum(complete.cases(unique(dt)))/(nlevels(dt$region)*nlevels(dt$product))
)

expect_equal(
  dt[, neighbors(r = region, n = product, simplify = TRUE)],
  factor(x=rep(1:2, c(nrow(dt1), nrow(dt2))))
)

expect_false(dt[, is.connected(r = region, n = product)])
expect_equal(nrow(dt[, comparisons(r=region, n=product)]), 2)
expect_equal(dt[, connect(r=region, n=product)], dt[, region%in%4:7])


# Misc --------------------------------------------------------------------


# example data without gaps:
set.seed(123)
dt <- rdata(R=4, B=1, N=3)

expect_equal(dt[, sparsity(r = region, n = product)], 0)
expect_true(dt[, is.connected(r = region, n = product)])
expect_equal(nrow(dt[, comparisons(r = region, n = product)]), 1)
expect_true(all(dt[, connect(r=region, n=product)]))
expect_equal(
  dt[, neighbors(r = region, n = product, simplify = TRUE)],
  factor(x=rep(1, nrow(dt)))
)

# END
