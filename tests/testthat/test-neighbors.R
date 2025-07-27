# START


# Data with one region only -----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=1, B=1, N=4)

expect_true(dt[, is.connected(r=region, n=product)])
expect_equal(dt[, gaps(r=region, n=product)], NA_real_)
expect_equal(dt[, pairs(r=region, n=product)], 0L)
expect_equal(dt[, connect(r=region, n=product)], rep(TRUE, nrow(dt)))
expect_equal(dt[, neighbors(r=region, n=product, simplify=TRUE)], rep(1L, nrow(dt)))
expect_equal(with(dt, neighbors(r=region, n=product, simplify=FALSE)), list("1"))
expect_equal(nrow(dt[, properties(r=region, n=product)]), 1L)


# Data with one product only ----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=4, B=1, N=1)

expect_true(dt[, is.connected(r=region, n=product)])
expect_equal(dt[, gaps(r=region, n=product)], 0)
expect_equal(dt[, pairs(r=region, n=product)], 4*3/2)
expect_equal(dt[, connect(r=region, n=product)], rep(TRUE, nrow(dt)))
expect_equal(dt[, neighbors(r=region, n=product, simplify=TRUE)], rep(1L, nrow(dt)))
expect_equal(with(dt, neighbors(r=region, n=product, simplify=FALSE)), list(as.character(1:4)))
expect_equal(nrow(dt[, properties(r=region, n=product)]), 1L)


# Data with gaps ----------------------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=4, B=1, N=3, gaps=0.25)

expect_true(dt[, is.connected(r=region, n=product)])
expect_equal(dt[, gaps(r=region, n=product)], 0.25)
expect_equal(dt[, pairs(r=region, n=product)], 4*3/2)
expect_equal(dt[, connect(r=region, n=product)], rep(TRUE, nrow(dt)))
expect_equal(dt[, neighbors(r=region, n=product, simplify=TRUE)], rep(1L, nrow(dt)))
expect_equal(with(dt, neighbors(r=region, n=product, simplify=FALSE)), list(c("2","4","1","3")))
expect_equal(nrow(dt[, properties(r=region, n=product)]), 1L)


# Non-connected data ------------------------------------------------------


# example data:
set.seed(123)
dt1 <- pricelevels::rdata(R=3, B=1, N=5, gaps=0.2)
dt2 <- pricelevels::rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)

expect_false(dt[, is.connected(r=region, n=product)])

expect_equal(
  dt[, gaps(r=region, n=product)],
  1-sum(complete.cases(unique(dt)))/(nlevels(dt$region)*nlevels(dt$product))
)

expect_equal(dt[, pairs(r=region, n=product)], (3*2+4*3)/2)

expect_equal(
  dt[, neighbors(r=region, n=product, simplify=TRUE)],
  rep(1:2, c(nrow(dt1), nrow(dt2)))
)

expect_equal(
  with(dt, neighbors(r=region, n=product, simplify=FALSE)),
  list(as.character(1:3), as.character(4:7))
)

expect_equal(dt[, connect(r=region, n=product)], dt[, region%in%4:7])

expect_equal(nrow(dt[, properties(r=region, n=product)]), 2)


# No data -----------------------------------------------------------------


# example data:
dt <- rdata(R=4, B=1, N=2)
dt$product <- NA_character_

# all incomplete cases are removed:
expect_false(dt[, is.connected(r=region, n=product)])
expect_equal(dt[, gaps(r=region, n=product)], NA_real_)
expect_equal(dt[, pairs(r=region, n=product)], 0L)
expect_equal(dt[, connect(r=region, n=product)], logical())
expect_equal(dt[, neighbors(r=region, n=product, simplify=TRUE)], integer())
expect_equal(with(dt, neighbors(r=region, n=product, simplify=FALSE)), list())
expect_equal(nrow(dt[, properties(r=region, n=product)]), 0L)


# Misc --------------------------------------------------------------------


# example data:
dt <- rdata(R=4, B=1, N=3)
dt <- rbind(dt[-c(2,3,4),], dt[c(12,12)])

# only unique observations that are available in multiple regions matter:
expect_equal(dt[, properties(r=region, n=product)]$nprods, 2L)
expect_equal(dt[, properties(r=region, n=product)]$nobs, 8L)
expect_equal(dt[, properties(r=region, n=product)]$gaps, 0)
expect_equal(dt[, properties(r=region, n=product)]$pairs, 4*3/2)

# END
