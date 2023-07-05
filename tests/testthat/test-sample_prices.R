# START


# gaps() ------------------------------------------------------------------


data <- prices(R=6, N=15)

# number of gaps:
expect_true(
  all(abs(replicate(n=100, expr=nrow(data[!gaps(region, product, amount=0.2, pairs=TRUE), ]))-6*15*(1-0.2))<1e-10)
)

# number of gaps:
expect_true(
  all(abs(replicate(n=100, expr=nrow(data[!gaps(region, product, amount=0.2, pairs=FALSE), ]))-6*15*(1-0.2))<1e-10)
)

# still connected price data:
expect_true(
  all(replicate(n=100, expr=data[!gaps(region, product, amount=0.6, pairs=TRUE), is.connected(r=region, n=product)]))
)

# still connected price data:
expect_true(
  all(replicate(n=100, expr=data[!gaps(region, product, amount=0.6, pairs=FALSE), is.connected(r=region, n=product)]))
)

# at least two observations per product:
expect_true(
  all(replicate(n=100, expr=data[!gaps(region, product, amount=0.6, pairs=TRUE), .N>=2, by="product"]$V1))  
)

# at least one observations per product:
expect_true(
  all(replicate(n=100, expr=data[!gaps(region, product, amount=0.6, pairs=FALSE), .N>=1, by="product"]$V1))
)

# probability of gaps:
test <- rowMeans(replicate(n=100, expr=data[!gaps(region, product, amount=0.6, prob=rev(as.integer(product))), .N, by="product"]$N))
expect_gte(test[15], test[1])

test <- rowMeans(replicate(n=100, expr=data[!gaps(region, product, amount=0.6, prob=as.integer(product)), .N, by="product"]$N))
expect_lte(test[15], test[1])

# no gaps for region "r2" and for product "n3" in region "r5":
dt.excl <- data.table("r"=c("r2","r5"),"n"=c(NA,"n03"))

expect_true(
  all(
    replicate(
      n=100,
      expr={
        test <- data[!gaps(region, product, amount=0.6, pairs=FALSE, exclude=dt.excl)]
        all(
          c(test[region=="r2", abs(.N-15)<1e-10],
            abs(nrow(test[region=="r5" & product=="n03",])-1)<1e-10)
        )
      }
    )
  )
)


# weights() ---------------------------------------------------------------


# sample complete price data:
data <- prices(R=7, N=13)

# add weights:
data[, "w1" := weights(r=region, n=product, type=~1)] # constant
data[, "w2" := weights(r=region, n=product, type=~n)] # product-specific
data[, "w3" := weights(r=region, n=product, type=~n+r)] # product-region-specific

# non-negative weights:
expect_true(
  all(replicate(n=100, expr=data[, weights(r=region, n=product, type=~n+r)])>=0)
)

# no variation in constant weights:
expect_true(
  abs(sd(data$w1))<1e-10
)

# variation only between products:
expect_true(
  all(abs(data[, sd(w2), by="product"]$V1)<1e-10)
)

# weights add up to 1:
expect_true(
  all(abs(data[, sum(w1), by = "region"]$V1-1)<1e-10)
)

expect_true(
  all(abs(data[, sum(w2), by = "region"]$V1-1)<1e-10)
)

expect_true(
  all(abs(data[, sum(w3), by = "region"]$V1-1)<1e-10)
)


# prices() ----------------------------------------------------------------


expect_true(
  abs(nrow(prices(R=1, N=1))-1)<1e-10
)

expect_true(
  abs(nrow(prices(R=2, N=1))-2)<1e-10
)

expect_true(
  abs(nrow(prices(R=13, N=17))-13*17)<1e-10
)

expect_true(
  nrow(prices(R=13, N=17, gaps=0.1))<13*17
)

expect_true(
  all(names(prices(R=13, N=17))%in%c("region","product","price"))
)

expect_true(
  all(names(prices(R=13, N=17, weights=~n))%in%c("region","product","price","weight"))
)

expect_true(
  data.table::is.data.table(prices(R=5, N=10))
)

expect_true(
  is.list(prices(R=5, N=10, settings=list("par.add"=TRUE)))
)

test <- prices(R=5, N=10, settings=list("par.add"=TRUE, par.sd=c("lnP"=0, "pi"=0, "delta"=0)))

expect_true(
  all(abs(test$param$lnP)<1e-10)
)

expect_true(
  all(abs(test$param$pi-1)<1e-10)
)

expect_true(
  all(abs(test$param$delta-1)<1e-10)
)

# END
