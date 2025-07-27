# START


options(pricelevels.chatty=FALSE)


# Unique price observations and no missings for base ----------------------


set.seed(123)
dt1 <- rdata(R=3, B=1, N=4)
levels(dt1$region) <- c("a","b","c")

# static base region:
res <- dt1[, price/price[region=="b"], by="product"]$V1
expect_equal(
  dt1[, ratios(p=price, r=region, n=product, base="b", settings=list(static=TRUE))],
  res
)

# static and flexible base region should be the same for these data:
expect_equal(
  dt1[, ratios(p=price, r=region, n=product, base="b", settings=list(static=TRUE))],
  dt1[, ratios(p=price, r=region, n=product, base="b", settings=list(static=FALSE))],
  ignore_attr=TRUE
)

# regional average as base:
res <- dt1[, price/mean(price), by="product"]$V1
expect_equal(
  dt1[, ratios(p=price, r=region, n=product, base=NULL)],
  res
)


# Unique price observations and missings for base -------------------------


# drop two observations:
dt2 <- dt1[-c(5,10), ]

# flexible base region:
res <- dt2[, if("b"%in%region){price/price[region=="b"]}else{price/price[region=="a"]}, by="product"]$V1
attr(x=res, "base") <- c("1"="b", "2"="a", "3"="b", "4"="b")
expect_equal(
  dt2[, ratios(p=price, r=region, n=product, base="b", settings=list(static=FALSE))],
  res
)

# static base region:
res <- dt2[, if("b"%in%region){price/price[region=="b"]}else{rep(NA_real_, .N)}, by="product"]
expect_equal(
  dt2[, ratios(p=price, r=region, n=product, base="b", settings=list(static=TRUE))],
  res$V1
)

# regional average as base:
res <- dt2[, price/mean(price), by="product"]
expect_equal(
  dt2[, ratios(p=price, r=region, n=product, base=NULL)],
  res$V1
)


# Duplicated price observations and missings for base ---------------------


# insert duplicates and missings:
dt3 <- rbind(dt1[c(2,3),], dt1[-c(11),])
dt3[1:2, c("price","quantity") := list(price*1.1, quantity*0.95)]

### using no quantities or weights for averaging duplicates

# flexible base region:
res <- dt3[, if("b"%in%region){price/mean(price[region=="b"])}else{price/mean(price[region=="a"])}, by="product"]$V1
attr(x=res, "base") <- c("1"="b", "2"="b", "3"="b", "4"="a")
expect_equal(
  dt3[, ratios(p=price, r=region, n=product, base="b", settings=list(static=FALSE))],
  res
)

# static base region:
res <- dt3[, price/mean(price[region=="b"]), by="product"]$V1
res <- ifelse(test=is.nan(res), yes=NA, no=res)
expect_equal(
  dt3[, ratios(p=price, r=region, n=product, base="b", settings=list(static=TRUE))],
  res
)

# regional average as base:
res <- merge(dt3, dt3[, mean(price), by=c("region","product")][, mean(V1), by="product"], by="product", all.x=TRUE, sort=FALSE)[, price/V1]
expect_equal(
  dt3[, ratios(p=price, r=region, n=product, base=NULL)],
  res
)

### using quantities for averaging duplicates

# flexible base region:
res <- dt3[, if("b"%in%region){price/weighted.mean(price[region=="b"], quantity[region=="b"])}else{price/weighted.mean(price[region=="a"], quantity[region=="a"])}, by="product"]$V1
attr(x=res, "base") <- c("1"="b", "2"="b", "3"="b", "4"="a")
expect_equal(
  dt3[, "pr1" := ratios(p=price, r=region, n=product, q=quantity, base="b", settings=list(static=FALSE))]$pr1,
  res
)

# static base region:
res <- dt3[, price/weighted.mean(price[region=="b"], quantity[region=="b"]), by="product"]$V1
res <- ifelse(test=is.nan(res), yes=NA, no=res)
expect_equal(
  dt3[, "pr2" := ratios(p=price, r=region, n=product, q=quantity, base="b", settings=list(static=TRUE))]$pr2,
  res
)

# regional average as base:
res <- merge(dt3, dt3[, weighted.mean(price, quantity), by=c("region","product")][, mean(V1), by="product"], by="product", all.x=TRUE, sort=FALSE)[, price/V1]
expect_equal(
  dt3[, "pr3" := ratios(p=price, r=region, n=product, q=quantity, base=NULL)]$pr3,
  res
)

### random order of data

set.seed(2) # use this seed as otherwise another choice for the base region may distort the test
dt4 <- dt3[sample(1:.N),]

# flexible base region:
expect_equal(
  dt4[, ratios(p=price, r=region, n=product, q=quantity, base="b", settings=list(static=FALSE))],
  dt4$pr1,
  ignore_attr=TRUE
)

# static base region:
expect_equal(
  dt4[, ratios(p=price, r=region, n=product, q=quantity, base="b", settings=list(static=TRUE))],
  dt4$pr2,
  ignore_attr=TRUE
)

# regional average as base:
expect_equal(
  dt4[, ratios(p=price, r=region, n=product, q=quantity, base=NULL)],
  dt4$pr3,
  ignore_attr=TRUE
)

# END
