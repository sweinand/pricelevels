# START


# Unique price observations / no missings for base ------------------------


set.seed(123)
dt1 <- rdata(R=3, B=1, N=4)
levels(dt1$region) <- c("a","b","c")

# compute manually:
res <- dt1[, price/price[region=="b"], by="product"]
res <- setNames(res$V1, rep("b", nrow(res)))

# calculate price ratios by product groups:
expect_equal(
  dt1[, ratios(p = price, r = region, n = product, base = "b")],
  res
)


# Unique price observations / missings for base ---------------------------


# drop two observations:
dt2 <- dt1[-c(5,10), ]

# compute manually:
res <- dt2[, if("b"%in%region){price/price[region=="b"]}else{price/price[region=="a"]}, by="product"]
res <- setNames(res$V1, rep(c("b","a","b"), c(3,2,5)))

expect_equal(
  dt2[, ratios(price, region, product, base = "b")],
  res
)

# compute manually:
res <- dt2[, if("b"%in%region){price/price[region=="b"]}else{rep(NA_real_, .N)}, by="product"]
res <- setNames(res$V1, rep("b",10))

expect_equal(
  dt2[, ratios(price, region, product, base="b", static=TRUE)],
  res
)


# Treatment of duplicates and missings for base ---------------------------


# insert duplicates and missings:
dt3 <- rbind(dt1[2,], dt1[-c(1,10),])
dt3[1, "price" := dt1[2,price]+abs(rnorm(1))]
anyDuplicated(dt3, by=c("region","product"))

# compute manually:
res <- dt3[, if("b"%in%region){price/price[region=="b"][1]}else{price/price[region=="a"][1]}, by="product"]
res <- setNames(res$V1, rep("b", 11))

expect_equal(
  dt3[, ratios(price, region, product, base="b", drop=FALSE)],
  res
)

# compute manually:
res <- res[-c(1,5,8,10)]

expect_equal(
  dt3[, ratios(price, region, product, base="b", drop=TRUE)],
  res
)

# END
