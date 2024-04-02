# START


# Data with one region only -----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=1, B=1, N=4)

expect_equal(
  dt[, gkhamis(p=price, q=quantity, r=region, n=product)],
  c("1"=1)
)

expect_equal(
  dt[, gkhamis(p=price, q=quantity, r=region, n=product, settings=list(solve="matrix"))],
  c("1"=1)
)

expect_equal(
  dt[, ikle(p=price, q=quantity, r=region, n=product)],
  c("1"=1)
)

expect_equal(
  dt[, mdutot(p=price, r=region, n=product)],
  c("1"=1)
)


# Data with one product only ----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=4, B=1, N=1)

expect_no_error(
  dt[, gkhamis(p=price, q=quantity, r=region, n=product)]
)

expect_no_error(
  dt[, gkhamis(p=price, q=quantity, r=region, n=product, settings=list(solve="matrix"))]
)

expect_equal(
  dt[, gkhamis(p=price, q=quantity, r=region, n=product)],
  dt[, gkhamis(p=price, q=quantity, r=region, n=product, settings=list(solve="matrix"))]
)

expect_no_error(
  dt[, rao(p=price, q=quantity, r=region, n=product)]
)

expect_no_error(
  dt[, mcarli(p=price, r=region, n=product)]
)


# Data with gaps ----------------------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=3, B=1, N=4, gaps=0.2)

# gkhamis():
gk.est <- dt[, gkhamis(p=price, q=quantity, r=region, n=product, base=NULL)]
gk.est1 <- dt[, gkhamis(p=price, q=quantity, r=region, n=product, base="1")]
gk.est2 <- dt[, gkhamis(p=price, q=quantity, r=region, n=product, base="2")]
gk.est3 <- dt[, gkhamis(p=price, q=quantity, r=region, n=product, simplify=FALSE)]
gk.est4 <- dt[, gkhamis(p=price, q=quantity, r=region, n=product, simplify=FALSE, settings=list(solve="matrix"))]

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

# ikle():
ikd.est <- dt[, ikle(p=price, q=quantity, r=region, n=product, base=NULL)]
ikd.est1 <- dt[, ikle(p=price, q=quantity, r=region, n=product, base="1")]
ikd.est2 <- dt[, ikle(p=price, q=quantity, r=region, n=product, base="2")]
ikd.est3 <- dt[, ikle(p=price, q=quantity, r=region, n=product, simplify=FALSE)]

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
rao.est <- dt[, rao(p=price, q=quantity, r=region, n=product, base=NULL)]
rao.est1 <- dt[, rao(p=price, q=quantity, r=region, n=product, base="1")]
rao.est2 <- dt[, rao(p=price, q=quantity, r=region, n=product, base="2")]
rao.est3 <- dt[, rao(p=price, q=quantity, r=region, n=product, simplify=FALSE)]

expect_equal(is.vector(rao.est1), TRUE)
expect_equal(is.vector(rao.est2), TRUE)
expect_equal(is.list(rao.est3), TRUE)
expect_equal(names(rao.est3), c("par","niter","tol"))
expect_equal(rao.est1[1], c("1"=1))
expect_equal(rao.est2[2], c("2"=1))
expect_equal(exp(mean(log(rao.est))), 1) # different to others!
expect_equal(rao.est1, rao.est/rao.est[1])
expect_equal(rao.est1, rao.est2/rao.est2[1])

# mjevons():
mj.est <- dt[, mjevons(p=price, r=region, n=product, base=NULL)]
mj.est1 <- dt[, mjevons(p=price, r=region, n=product, base="1")]
mj.est2 <- dt[, mjevons(p=price, r=region, n=product, base="2")]
mj.est3 <- dt[, mjevons(p=price, r=region, n=product, simplify=FALSE)]

expect_equal(is.vector(mj.est1), TRUE)
expect_equal(is.vector(mj.est2), TRUE)
expect_equal(is.list(mj.est3), TRUE)
expect_equal(names(mj.est3), c("par","niter","tol"))
expect_equal(mj.est1[1], c("1"=1))
expect_equal(mj.est2[2], c("2"=1))
expect_equal(exp(mean(log(mj.est))), 1) # different to others!
expect_equal(mj.est1, mj.est/mj.est[1])
expect_equal(mj.est1, mj.est2/mj.est2[1])

# test quantities versus shares as weights:
dt[, "share" := (price*quantity)/sum(price*quantity), by="region"]

expect_equal(
  dt[, rao(p=price, q=quantity, r=region, n=product, base=NULL)],
  dt[, rao(p=price, w=share, r=region, n=product, base=NULL)]
)

expect_equal(
  dt[, ikle(p=price, q=quantity, r=region, n=product, base=NULL)],
  dt[, ikle(p=price, w=share, r=region, n=product, base=NULL)]
)

expect_equal(
  dt[, rhajargasht(p=price, q=quantity, r=region, n=product, base=NULL)],
  dt[, rhajargasht(p=price, w=share, r=region, n=product, base=NULL)]
)


# Settings ----------------------------------------------------------------


# missing quantities:
expect_error(
  dt[, gkhamis(p=price, r=region, n=product)]
)

# missing quantities and weights:
expect_error(
  dt[, rao(p=price, r=region, n=product)]
)

# wrong setting:
expect_error(
  dt[, gkhamis(p=price, q=quantity, r=region, n=product, settings=list(solve="abc"))]
)

# negative tolerance not allowed:
expect_error(
  dt[, gkhamis(p=price, q=quantity, r=region, n=product, settings=list(tol=-2))]
)

# negative maximum number of iterations not allowed:
expect_error(
  dt[, gkhamis(p=price, q=quantity, r=region, n=product, settings=list(max.iter=-2))]
)

# solve-method not allowed:
expect_error(
  dt[, ikle(p=price, q=quantity, r=region, n=product, settings=list(solve="matrix"))]
)


# Non-connected data ------------------------------------------------------


# example data:
set.seed(123)
dt1 <- pricelevels::rdata(R=3, B=1, N=5)
dt2 <- pricelevels::rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)

expect_equal(
  dt[, gkhamis(p=price, r=region, n=product, q=quantity, base="1",
           settings=list(chatty=FALSE, connect=TRUE))][1],
  c("1"=1)
)

expect_equal(
  dt[, gkhamis(p=price, r=region, n=product, q=quantity, base="1",
           settings=list(chatty=FALSE, connect=TRUE))][4:7],
  setNames(rep(NA_real_, 4), 4:7)
)

expect_equal(
  dt[, gkhamis(p=price, r=region, n=product, q=quantity, base="4",
           settings=list(chatty=FALSE, connect=TRUE))][1:3],
  setNames(rep(NA_real_, 3), 1:3)
)

expect_equal(
  dt[, gkhamis(p=price, r=region, n=product, q=quantity, base="4",
           settings=list(chatty=FALSE, connect=TRUE))][4],
  c("4"=1)
)

expect_equal(
  dt[, gkhamis(p=price, r=region, n=product, q=quantity, base="1",
             settings=list(chatty=FALSE, connect=TRUE, solve="matrix"))][1],
  c("1"=1)
)

expect_equal(
  dt[, gkhamis(p=price, r=region, n=product, q=quantity, base="1",
             settings=list(chatty=FALSE, connect=TRUE, solve="matrix"))][4:7],
  setNames(rep(NA_real_, 4), 4:7)
)

expect_equal(
  dt[, gkhamis(p=price, r=region, n=product, q=quantity, base="4",
             settings=list(chatty=FALSE, connect=TRUE, solve="matrix"))][1:3],
  setNames(rep(NA_real_, 3), 1:3)
)

expect_equal(
  dt[, gkhamis(p=price, r=region, n=product, q=quantity, base="4",
             settings=list(chatty=FALSE, connect=TRUE, solve="matrix"))][4],
  c("4"=1)
)


# Misc --------------------------------------------------------------------


# test if ikle(), gkhamis(), and gerardi() identical if no gaps
# and quantities are the same across regions:
set.seed(123)
dt <- rdata(R=5, B=1, N=9, gaps=0)
dt[, "quantity" := 1000*rleidv(product)]

expect_equal(
  dt[, gkhamis(p=price, q=quantity, r=region, n=product, base=NULL)],
  dt[, ikle(p=price, q=quantity, r=region, n=product, base=NULL)]
)

expect_equal(
  dt[, gkhamis(p=price, q=quantity, r=region, n=product, base=NULL)],
  dt[, gerardi(p=price, q=quantity, r=region, n=product, base=NULL)]
)

expect_equal(
  dt[, gerardi(p=price, q=quantity, r=region, n=product, base=NULL)],
  dt[, ikle(p=price, q=quantity, r=region, n=product, base=NULL)]
)

# test if unweighted are identical to weighted ones using weights of 1
# if there are no gaps (with gaps, renormalization of weights causes
# differences):
expect_equal(
  dt[, rao(p=price, w=rep(1,.N), r=region, n=product, base=NULL)],
  dt[, mjevons(p=price, r=region, n=product, base=NULL)]
)

expect_equal(
  dt[, rhajargasht(p=price, w=rep(1,.N), r=region, n=product, base=NULL)],
  dt[, mcarli(p=price, r=region, n=product, base=NULL)]
)

expect_equal(
  dt[, ikle(p=price, w=rep(1,.N), r=region, n=product, base=NULL)],
  dt[, mharmonic(p=price, r=region, n=product, base=NULL)]
)

expect_equal(
  dt[, gkhamis(p=price, q=rep(1,.N), r=region, n=product, base=NULL)],
  dt[, mdutot(p=price, r=region, n=product, base=NULL)]
)

# test if rao() is identical to cpd() even if there are gaps:
# see http://www.roiw.org/2005/2005-24.pdf
set.seed(123)
dt <- rdata(R=5, B=1, N=9, gaps=0.3)
dt[, "share":=price*quantity/sum(price*quantity), by="region"]

expect_equal(
  dt[, rao(p=price, q=quantity, r=region, n=product, base="1")],
  dt[, cpd(p=price, q=quantity, r=region, n=product, base="1")]
)

expect_equal(
  dt[, rao(p=price, q=quantity, r=region, n=product, base=NULL)],
  dt[, cpd(p=price, q=quantity, r=region, n=product, base=NULL)]
)

expect_equal(
  dt[, rao(p=price, w=share, r=region, n=product, base="1")],
  dt[, cpd(p=price, w=share, r=region, n=product, base="1")]
)

expect_equal(
  dt[, rao(p=price, w=share, r=region, n=product, base=NULL)],
  dt[, cpd(p=price, w=share, r=region, n=product, base=NULL)]
)

# END
