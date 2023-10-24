# START

### (1) unique price observations; no missings for base

set.seed(123)
data1 <- rdata(R=3, B=1, N=4)
levels(data1$region) <- c("a","b","c")

# compute manually:
res <- data1[, price/price[region=="b"], by="product"]
res <- setNames(res$V1, rep("b", nrow(res)))

# calculate price ratios by product groups:
expect_equal(
  data1[, ratios(p = price, r = region, n = product, base = "b")],
  res
)


### (2) unique price observations; missings for base

# drop two observations:
data2 <- data1[-c(5,10), ]

# compute manually:
res <- data2[, if("b"%in%region){price/price[region=="b"]}else{price/price[region=="a"]}, by="product"]
res <- setNames(res$V1, rep(c("b","a","b"), c(3,2,5)))

expect_equal(
  data2[, ratios(price, region, product, base = "b")],
  res
)

# compute manually:
res <- data2[, if("b"%in%region){price/price[region=="b"]}else{rep(NA_real_, .N)}, by="product"]
res <- setNames(res$V1, rep("b",10))

expect_equal(
  data2[, ratios(price, region, product, base="b", static=TRUE)],
  res
)

### (3) treatment of duplicates and missings for base:

# insert duplicates and missings:
data3 <- rbind(data1[2,], data1[-c(1,10),])
data3[1, "price" := data1[2,price]+abs(rnorm(1))]
anyDuplicated(data3, by=c("region","product"))

# compute manually:
res <- data3[, if("b"%in%region){price/price[region=="b"][1]}else{price/price[region=="a"][1]}, by="product"]
res <- setNames(res$V1, rep("b", 11))

expect_equal(
  data3[, ratios(price, region, product, base="b", drop=FALSE)],
  res
)

# compute manually:
res <- res[-c(1,5,8,10)]

expect_equal(
  data3[, ratios(price, region, product, base="b", drop=TRUE)],
  res
)

# END
