# START


# Unique price observations / no missings for base ------------------------


set.seed(123)
dt1 <- rdata(R=3, B=1, N=4)
levels(dt1$region) <- c("a","b","c")

# compute manually:
res <- dt1[, price/price[region=="b"], by="product"]
expect_equal(
  dt1[, ratios(p=price, r=region, n=product, base="b")],
  res$V1
)

# compute manually:
res <- dt1[, price/mean(price), by="product"]
expect_equal(
  dt1[, ratios(p=price, r=region, n=product, base=NULL)],
  res$V1
)


# Unique price observations / missings for base ---------------------------


# drop two observations:
dt2 <- dt1[-c(5,10), ]

# compute manually:
res <- dt2[, if("b"%in%region){price/price[region=="b"]}else{price/price[region=="a"]}, by="product"]$V1
attr(res, "base") <- setNames(c(F,T,F,T,F,F,T,F,T,F), dt2[, if("b"%in%region){rep("b",.N)}else{rep("a",.N)}, by="product"]$V1)
expect_equal(
  dt2[, ratios(p=price, r=region, n=product, base="b", settings=list(chatty=FALSE))],
  res
)

# compute manually:
res <- dt2[, price/mean(price), by="product"]
expect_equal(
  dt2[, ratios(p=price, r=region, n=product, base=NULL, settings=list(chatty=FALSE))],
  res$V1
)

# compute manually with static base:
res <- dt2[, if("b"%in%region){price/price[region=="b"]}else{rep(NA_real_, .N)}, by="product"]
expect_equal(
  dt2[, ratios(p=price, r=region, n=product, base="b", static=TRUE)],
  res$V1
)


# Treatment of duplicates and missings for base ---------------------------


# insert duplicates and missings:
dt3 <- rbind(dt1[2,], dt1[-c(1,10),])
dt3[1, "price" := dt1[2,price]+abs(rnorm(1))]
anyDuplicated(dt3, by=c("region","product"))

# compute manually:
res31 <- dt3[, if("b"%in%region){price/price[region=="b"][1]}else{price/price[region=="a"][1]}, by="product"]
expect_equal(
  dt3[, ratios(p=price, r=region, n=product, base="b")],
  res31$V1
)

# compute manually:
res32 <- dt3[, price/mean(price), by="product"]
expect_equal(
  dt3[, ratios(p=price, r=region, n=product, base=NULL)],
  res32$V1
)

# END
