# START

# example data with one region only:
set.seed(123)
dt <- rdata(R=1, B=1, N=4)

expect_equal(
  dt[, gk(p=price, q=quantity, r=region, n=product)],
  c("1"=1)
)

expect_equal(
  dt[, gk(p=price, q=quantity, r=region, n=product, settings=list("method"="solve"))],
  c("1"=1)
)

expect_equal(
  dt[, rao(p=price, q=quantity, r=region, n=product)],
  c("1"=1)
)

expect_equal(
  dt[, idb(p=price, q=quantity, r=region, n=product)],
  c("1"=1)
)

expect_equal(
  dt[, geradi(p=price, q=quantity, r=region, n=product)],
  c("1"=1)
)

# example data with one product only:
set.seed(123)
dt <- rdata(R=4, B=1, N=1)

expect_no_error(
  dt[, gk(p=price, q=quantity, r=region, n=product)]
)

expect_no_error(
  dt[, gk(p=price, q=quantity, r=region, n=product, settings=list("method"="solve"))]
)

expect_equal(
  dt[, gk(p=price, q=quantity, r=region, n=product)],
  dt[, gk(p=price, q=quantity, r=region, n=product, settings=list("method"="solve"))]
)

expect_no_error(
  dt[, idb(p=price, q=quantity, r=region, n=product)]
)

expect_no_error(
  dt[, rao(p=price, q=quantity, r=region, n=product)]
)

expect_no_error(
  dt[, geradi(p=price, q=quantity, r=region, n=product)]
)

# example data with gaps to test output and rebasing:
set.seed(123)
dt <- rdata(R=3, B=1, N=4, gaps=0.2)

# gk():
gk.est <- dt[, gk(p=price, q=quantity, r=region, n=product, base=NULL)]
gk.est1 <- dt[, gk(p=price, q=quantity, r=region, n=product, base="1")]
gk.est2 <- dt[, gk(p=price, q=quantity, r=region, n=product, base="2")]
gk.est3 <- dt[, gk(p=price, q=quantity, r=region, n=product, simplify=FALSE)]
gk.est4 <- dt[, gk(p=price, q=quantity, r=region, n=product, simplify=FALSE, settings=list(method="solve"))]

expect_equal(is.vector(gk.est1), TRUE)
expect_equal(is.vector(gk.est2), TRUE)
expect_equal(is.list(gk.est3), TRUE)
expect_equal(is.list(gk.est4), TRUE)
expect_equal(names(gk.est3), c("par","niter","tol"))
expect_equal(names(gk.est4), c("par"))
expect_equal(gk.est3$par, gk.est4$par)
expect_equal(gk.est1[1], c("1"=1))
expect_equal(gk.est2[2], c("2"=1))
expect_equal(mean(gk.est), 1)
expect_equal(gk.est1, gk.est/gk.est[1])
expect_equal(gk.est1, gk.est2/gk.est2[1])

# ikd():
ikd.est <- dt[, gk(p=price, q=quantity, r=region, n=product, base=NULL)]
ikd.est1 <- dt[, gk(p=price, q=quantity, r=region, n=product, base="1")]
ikd.est2 <- dt[, gk(p=price, q=quantity, r=region, n=product, base="2")]
ikd.est3 <- dt[, gk(p=price, q=quantity, r=region, n=product, simplify=FALSE)]

expect_equal(is.vector(ikd.est1), TRUE)
expect_equal(is.vector(ikd.est2), TRUE)
expect_equal(is.list(ikd.est3), TRUE)
expect_equal(names(ikd.est3), c("par","niter","tol"))
expect_equal(ikd.est1[1], c("1"=1))
expect_equal(ikd.est2[2], c("2"=1))
expect_equal(mean(ikd.est), 1)
expect_equal(ikd.est1, ikd.est/ikd.est[1])
expect_equal(ikd.est1, ikd.est2/ikd.est2[1])

# rao():
rao.est <- dt[, gk(p=price, q=quantity, r=region, n=product, base=NULL)]
rao.est1 <- dt[, gk(p=price, q=quantity, r=region, n=product, base="1")]
rao.est2 <- dt[, gk(p=price, q=quantity, r=region, n=product, base="2")]
rao.est3 <- dt[, gk(p=price, q=quantity, r=region, n=product, simplify=FALSE)]

expect_equal(is.vector(rao.est1), TRUE)
expect_equal(is.vector(rao.est2), TRUE)
expect_equal(is.list(rao.est3), TRUE)
expect_equal(names(rao.est3), c("par","niter","tol"))
expect_equal(rao.est1[1], c("1"=1))
expect_equal(rao.est2[2], c("2"=1))
expect_equal(mean(rao.est), 1)
expect_equal(rao.est1, rao.est/rao.est[1])
expect_equal(rao.est1, rao.est2/rao.est2[1])

# geradi():
geradi.est <- dt[, gk(p=price, q=quantity, r=region, n=product, base=NULL)]
geradi.est1 <- dt[, gk(p=price, q=quantity, r=region, n=product, base="1")]
geradi.est2 <- dt[, gk(p=price, q=quantity, r=region, n=product, base="2")]
geradi.est3 <- dt[, gk(p=price, q=quantity, r=region, n=product, simplify=FALSE)]

expect_equal(is.vector(geradi.est1), TRUE)
expect_equal(is.vector(geradi.est2), TRUE)
expect_equal(is.list(geradi.est3), TRUE)
expect_equal(names(geradi.est3), c("par","niter","tol"))
expect_equal(geradi.est1[1], c("1"=1))
expect_equal(geradi.est2[2], c("2"=1))
expect_equal(mean(geradi.est), 1)
expect_equal(geradi.est1, geradi.est/geradi.est[1])
expect_equal(geradi.est1, geradi.est2/geradi.est2[1])

# test quantities versus shares as weights:
dt[, "share" := (price*quantity)/sum(price*quantity), by="region"]

expect_equal(
  dt[, rao(p=price, q=quantity, r=region, n=product, base=NULL)],
  dt[, rao(p=price, w=share, r=region, n=product, base=NULL)]
)

expect_equal(
  dt[, idb(p=price, q=quantity, r=region, n=product, base=NULL)],
  dt[, idb(p=price, w=share, r=region, n=product, base=NULL)]
)

expect_equal(
  dt[, geradi(p=price, q=quantity, r=region, n=product, base=NULL)],
  dt[, geradi(p=price, w=share, r=region, n=product, base=NULL)]
)

# test if ikd(), gk(), and geradi() identical if no gaps
# and quantities are the same across regions:
dt <- rdata(R=5, B=1, N=9, gaps=0)
dt[, "quantity" := 1000*rleidv(product)]

expect_equal(
  dt[, gk(p=price, q=quantity, r=region, n=product, base=NULL)],
  dt[, idb(p=price, q=quantity, r=region, n=product, base=NULL)]
)

expect_equal(
  dt[, gk(p=price, q=quantity, r=region, n=product, base=NULL)],
  dt[, geradi(p=price, q=quantity, r=region, n=product, base=NULL)]
)

expect_equal(
  dt[, geradi(p=price, q=quantity, r=region, n=product, base=NULL)],
  dt[, idb(p=price, q=quantity, r=region, n=product, base=NULL)]
)

# test inputs:

# quantities missing:
expect_error(
  dt[, gk(p=price, r=region, n=product, settings=list(method="abc"))]
)

# weights and quantities missing:
expect_error(
  dt[, rao(p=price, r=region, n=product, settings=list(method="abc"))]
)

# weights and quantities missing:
expect_error(
  dt[, idb(p=price, r=region, n=product, settings=list(method="abc"))]
)

# weights and quantities missing:
expect_error(
  dt[, geradi(p=price, r=region, n=product, settings=list(method="abc"))]
)

# wrong setting:
expect_error(
  dt[, gk(p=price, q=quantity, r=region, n=product, settings=list(method="abc"))]
)

# abbreviated setting ok:
expect_no_error(
  dt[, gk(p=price, q=quantity, r=region, n=product, settings=list(method="iter"))]
)

# negative tolerance not allowed:
expect_error(
  dt[, gk(p=price, q=quantity, r=region, n=product, settings=list(tol=-2))]
)

# negative maximum number of iterations not allowed:
expect_error(
  dt[, gk(p=price, q=quantity, r=region, n=product, settings=list(max.iter=-2))]
)

# solve-method not allowed:
expect_error(
  dt[, idb(p=price, q=quantity, r=region, n=product, settings=list(method="solve"))]
)

# solve-method not allowed:
expect_error(
  dt[, rao(p=price, q=quantity, r=region, n=product, settings=list(method="solve"))]
)

# solve-method not allowed:
expect_error(
  dt[, geradi(p=price, q=quantity, r=region, n=product, settings=list(method="solve"))]
)

# END
