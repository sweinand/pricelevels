# START


# rgaps() ------------------------------------------------------------------


dt <- rdata(R=6, B=1, N=15)

# number of gaps:
expect_true(
  all(abs(replicate(n=100, expr=nrow(dt[!rgaps(region, product, amount=0.2, pairs=TRUE), ]))-6*15*(1-0.2))<1e-10)
)

# number of gaps:
expect_true(
  all(abs(replicate(n=100, expr=nrow(dt[!rgaps(region, product, amount=0.2, pairs=FALSE), ]))-6*15*(1-0.2))<1e-10)
)

# still connected price data:
expect_true(
  all(replicate(n=100, expr=dt[!rgaps(region, product, amount=0.6, pairs=TRUE), is.connected(r=region, n=product)]))
)

# still connected price data:
expect_true(
  all(replicate(n=100, expr=dt[!rgaps(region, product, amount=0.6, pairs=FALSE), is.connected(r=region, n=product)]))
)

# at least two observations per product:
expect_true(
  all(replicate(n=100, expr=dt[!rgaps(region, product, amount=0.6, pairs=TRUE), .N>=2, by="product"]$V1))
)

# at least one observations per product:
expect_true(
  all(replicate(n=100, expr=dt[!rgaps(region, product, amount=0.6, pairs=FALSE), .N>=1, by="product"]$V1))
)

# probability of gaps:
test <- rowMeans(replicate(n=100, expr=dt[!rgaps(region, product, amount=0.6, prob=rev(as.integer(product))), .N, by="product"]$N))
expect_gte(test[15], test[1])

test <- rowMeans(replicate(n=100, expr=dt[!rgaps(region, product, amount=0.6, prob=as.integer(product)), .N, by="product"]$N))
expect_lte(test[15], test[1])

# no gaps for region "r2" and for product "n3" in region "r5":
dt.excl <- data.table("r"=c("2","5"),"n"=c(NA,"03"))

expect_true(
  all(
    replicate(
      n=100,
      expr={
        test <- dt[!rgaps(region, product, amount=0.6, pairs=FALSE, exclude=dt.excl)]
        all(
          c(test[region=="2", abs(.N-15)<1e-10],
            abs(nrow(test[region=="5" & product=="03",])-1)<1e-10)
        )
      }
    )
  )
)


# rweights() ---------------------------------------------------------------


# sample complete price data:
dt <- rdata(R=7, B=1, N=13)

# add weights:
dt[, "w1" := rweights(r=region, b=product, type=~1)] # constant
dt[, "w2" := rweights(r=region, b=product, type=~b)] # product-specific
dt[, "w3" := rweights(r=region, b=product, type=~b+r)] # product-region-specific

# non-negative weights:
expect_true(
  all(replicate(n=100, expr=dt[, rweights(r=region, b=product, type=~b+r)])>=0)
)

# no variation in constant weights:
expect_true(
  abs(sd(dt$w1))<1e-10
)

# variation only between products:
expect_true(
  all(abs(dt[, sd(w2), by="product"]$V1)<1e-10)
)

# weights add up to 1:
expect_true(
  all(abs(dt[, sum(w1), by = "region"]$V1-1)<1e-10)
)

expect_true(
  all(abs(dt[, sum(w2), by = "region"]$V1-1)<1e-10)
)

expect_true(
  all(abs(dt[, sum(w3), by = "region"]$V1-1)<1e-10)
)


# rsales() ----------------------------------------------------------------


# sample complete price data:
dt <- rdata(R=7, B=1, N=13)

# no sales:
expect_true(
  all(dt[, spin:::rsales(p=price, q=quantity, amount=0)]$price_is_sale==FALSE)
)

expect_true(
  any(dt[, spin:::rsales(p=price, q=quantity, amount=0.1)]$price_is_sale)
)

expect_true(
  all(dt[, spin:::rsales(p=price, q=quantity, amount=1)]$price_is_sale)
)


# rdata() ----------------------------------------------------------------


expect_true(
  abs(nrow(rdata(R=1, B=1, N=1))-1)<1e-10
)

expect_true(
  abs(nrow(rdata(R=2, B=1, N=1))-2)<1e-10
)

expect_true(
  abs(nrow(rdata(R=13, B=1, N=17))-13*17)<1e-10
)

expect_true(
  abs(nrow(rdata(R=13, B=3, N=17))-13*17*3)<1e-10
)

expect_true(
  nrow(rdata(R=13, B=1, N=17, gaps=0.1))<13*17
)

expect_true(
  any(rdata(R=10, B=1, N=15, sales=0.1)$sale)
)

expect_true(
  all(!rdata(R=10, B=1, N=15, sales=0)$sale)
)

expect_true(
  all(names(rdata(R=13, B=1, N=17))%in%c("group","weight","region","product","sale","price","quantity"))
)

expect_true(
  data.table::is.data.table(rdata(R=5, B=1, N=10))
)

expect_true(
  is.list(rdata(R=5, B=2, N=10, settings=list("par.add"=TRUE)))
)

dt.test <- rdata(R=5, B=2, N=10, settings=list("par.add"=TRUE, par.sd=c("lnP"=0, "pi"=0, "delta"=0)))

expect_true(
  all(abs(dt.test$param$lnP)<1e-10)
)

# expect_true(
#   all(abs(dt.test$param$pi-1)<1e-10)
# )

expect_true(
  all(abs(dt.test$param$delta-1)<1e-10)
)

# END
