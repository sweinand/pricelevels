# START


# set.base() --------------------------------------------------------------


# regions:
r <- factor(c("a","b","b","c"))


expect_equal(
  set.base(r=r, base=NULL, null.ok=FALSE, settings=list(chatty=FALSE)),
  "b"
)

expect_equal(
  set.base(r=r, base="test", null.ok=FALSE, settings=list(chatty=FALSE)),
  "b"
)

expect_equal(
  set.base(r=r, base="a", null.ok=FALSE, settings=list(chatty=FALSE)),
  "a"
)

expect_equal(
  set.base(r=r, base=NULL, null.ok=TRUE, settings=list(chatty=FALSE)),
  NULL
)

expect_equal(
  set.base(r=r, base="test", null.ok=TRUE, settings=list(chatty=FALSE)),
  "b"
)

expect_equal(
  set.base(r=r, base="a", null.ok=TRUE, settings=list(chatty=FALSE)),
  "a"
)


# arrange() -------------------------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=3, B=1, N=4, gaps=0.1)
dt[, "share" := price*quantity / sum(price*quantity), by="region"]
settings <- list(missings=TRUE, connect=TRUE, duplicates=TRUE, chatty=FALSE, norm.weights=TRUE)

res1 <- dt[, spin:::arrange(p=price, r=region, n=product, q=quantity, base="1", settings=settings)]
expect_equal(names(res1), c("r","n","p","z","w"))
expect_equal(res1$z, dt$quantity)
expect_equal(res1$w, dt$share)

res2 <- dt[, spin:::arrange(p=price, r=region, n=product, q=quantity, w=share, base="1", settings=settings)]
expect_equal(names(res2), c("r","n","p","z","w"))
expect_equal(res2$z, dt$quantity)
expect_equal(res2$w, dt$share)
expect_equal(res1, res2)

res3 <- dt[, spin:::arrange(p=price, r=region, n=product, w=share, base="1", settings=settings)]
expect_equal(names(res3), c("r","n","p","z","w"))
expect_equal(res3$z, dt$share)
expect_equal(res3$w, dt$share)

res4 <- dt[, spin:::arrange(p=price, r=region, n=product, base="1", settings=settings)]
expect_equal(names(res4), c("r","n","p","z","w"))
expect_equal(res4$z, rep(1, nrow(dt)))
expect_equal(res4$w, rep(1, nrow(dt)))

settings$norm.weights <- FALSE
res5 <- dt[, spin:::arrange(p=price, r=region, n=product, w=quantity, base="1", settings=settings)]
expect_equal(names(res5), c("r","n","p","z","w"))
expect_equal(res5$z, dt$quantity)
expect_equal(res5$w, dt$quantity)

# END
