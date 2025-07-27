# START


options(pricelevels.chatty=FALSE)


# set.base() --------------------------------------------------------------


# regions:
r <- factor(c("a","b","b","c"))
expect_equal(set.base(r=r, base=NULL, null.ok=FALSE), "b")
expect_equal(set.base(r=r, base="test", null.ok=FALSE), "b")
expect_equal(set.base(r=r, base="a", null.ok=FALSE), "a")
expect_equal(set.base(r=r, base=NULL, null.ok=TRUE), NULL)
expect_equal(set.base(r=r, base="test", null.ok=TRUE), "b")
expect_equal(set.base(r=r, base="a", null.ok=TRUE), "a")


# arrange() -------------------------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=3, B=1, N=4, gaps=0.1)
dt[, "share" := price*quantity / sum(price*quantity), by="region"]
settings <- list(missings=TRUE, prods=TRUE, connect=TRUE, duplicates=TRUE, norm.weights=TRUE)

# quantities and weights:
res1 <- dt[, pricelevels:::arrange(p=price, r=region, n=product, q=quantity, base="1", settings=settings)]
expect_equal(names(res1), c("r","n","p","q","w"))
expect_equal(res1$q, dt$quantity)
expect_equal(res1$w, dt$share)

res2 <- dt[, pricelevels:::arrange(p=price, r=region, n=product, q=quantity, w=share, base="1", settings=settings)]
expect_equal(names(res2), c("r","n","p","q","w"))
expect_equal(res2$q, dt$quantity)
expect_equal(res2$w, dt$share)
expect_equal(res1, res2)

res3 <- dt[, pricelevels:::arrange(p=price, r=region, n=product, w=share, base="1", settings=settings)]
expect_equal(names(res3), c("r","n","p","q","w"))
expect_equal(res3$q, rep(NA_real_, nrow(dt)))
expect_equal(res3$w, dt$share)

res4 <- dt[, pricelevels:::arrange(p=price, r=region, n=product, base="1", settings=settings)]
expect_equal(names(res4), c("r","n","p","q","w"))
expect_equal(res4$q, rep(NA_real_, nrow(dt)))
expect_equal(res4$w, rep(NA_real_, nrow(dt)))

settings$norm.weights <- FALSE
res5 <- dt[, pricelevels:::arrange(p=price, r=region, n=product, w=quantity, base="1", settings=settings)]
expect_equal(names(res5), c("r","n","p","q","w"))
expect_equal(res5$q, rep(NA_real_, nrow(dt)))
expect_equal(res5$w, dt$quantity)

# duplicated values:
dt2 <- rbind(dt[1,], dt)
dt2[1, c("price","quantity","share") := list(1.1*price, 0.95*quantity, 0.98*share)]

dt2agg <- dt2[, list("p"=sum(price*quantity/sum(quantity)), "q"=sum(quantity)), by=c("region","product")]
dt2agg[, "w" := p*q/sum(p*q), by="region"]
res6 <- dt2[, pricelevels:::arrange(p=price, r=region, n=product, q=quantity, base="1", settings=settings)]
expect_equal(nrow(dt2)-1, nrow(res6))
expect_equal(dt2agg[1, list(p,q,w)], res6[1, list(p,q,w)])

dt2agg <- dt2[, list("p"=sum(price*share/sum(share)), "q"=NA_real_, "w"=mean(share)), by=c("region","product")]
dt2agg[, "w" := w/sum(w), by="region"]
settings$norm.weights <- TRUE
res6 <- dt2[, pricelevels:::arrange(p=price, r=region, n=product, w=share, base="1", settings=settings)]
expect_equal(nrow(dt2)-1, nrow(res6))
expect_equal(dt2agg[1, list(p,q,w)], res6[1, list(p,q,w)])

# missings:
dt3 <- data.table::copy(dt)
dt3[1, "region":=NA]
res7 <- dt3[, pricelevels:::arrange(p=price, r=region, n=product, w=quantity, base="1", settings=settings)]
expect_equal(nrow(dt3)-1, nrow(res7))

# one region only:
expect_no_error(
  dt[region=="1", pricelevels:::arrange(p=price, r=region, n=product, w=quantity, base="1", settings=settings)]
)

settings$chatty <- TRUE
expect_warning(
  dt[-c(2,3), pricelevels:::arrange(p=price, r=region, n=product, w=quantity, base="1", settings=settings)]
)

settings$prods <- FALSE
expect_no_warning(
  dt[-c(2,3), pricelevels:::arrange(p=price, r=region, n=product, w=quantity, base="1", settings=settings)]
)

# END
