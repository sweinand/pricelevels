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
settings <- list(missings=TRUE, connect=TRUE, duplicates=TRUE, chatty=FALSE)

res <- dt[, spin:::arrange(p=price, r=region, n=product, q=quantity, base="1", settings=settings)]
expect_equal(names(res), c("r","n","p","z","w"))
expect_equal(res$z, dt$quantity)
expect_equal(res$w, dt$share)

res <- dt[, spin:::arrange(p=price, r=region, n=product, q=quantity, w=share, base="1", settings=settings)]
expect_equal(names(res), c("r","n","p","z","w"))
expect_equal(res$z, dt$quantity)
expect_equal(res$w, dt$share)

res <- dt[, spin:::arrange(p=price, r=region, n=product, w=share, base="1", settings=settings)]
expect_equal(names(res), c("r","n","p","z","w"))
expect_equal(res$z, dt$share)
expect_equal(res$w, dt$share)

res <- dt[, spin:::arrange(p=price, r=region, n=product, base="1", settings=settings)]
expect_equal(names(res), c("r","n","p","z","w"))
expect_equal(res$z, rep(1, nrow(dt)))
expect_equal(res$w, rep(1, nrow(dt)))

# END
