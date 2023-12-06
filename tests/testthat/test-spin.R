# START

# price indices:
Punw <- sort(spin:::pindices[uses_none==TRUE, name])
Pq <- sort(spin:::pindices[uses_none==TRUE | uses_q==TRUE, name])
Pw <- sort(spin:::pindices[uses_none==TRUE | uses_w==TRUE, name])


# Data with one region only -----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=1, B=1, N=4)

expect_equal(
  dt[, spin(p=price, r=region, n=product, base="1")],
  matrix(data=1, ncol=1, nrow=length(Punw), dimnames=list(Punw, "1"))
)

expect_equal(
  dt[, spin(p=price, r=region, n=product, w=weight, base="1")],
  matrix(data=1, ncol=1, nrow=length(Pw), dimnames=list(Pw, "1"))
)

expect_equal(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="1")],
  matrix(data=1, ncol=1, nrow=length(Pq), dimnames=list(Pq, "1"))
)


# Data with one product only ----------------------------------------------


# example data with one product only:
set.seed(123)
dt <- rdata(R=4, B=1, N=1)

expect_no_error(
  dt[, spin(p=price, r=region, n=product, base="1")]
)

expect_no_error(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="1")]
)

expect_no_error(
  dt[, spin(p=price, r=region, n=product, w=quantity, base="1")]
)


# Data with gaps ----------------------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=3, B=1, N=4, gaps=0.2, settings=list(exclude=data.frame("r"=1, "n"=NA)))
dt[, "weight" := rweights(r=region, b=product, type=~b)]

# expect no error:
expect_no_error(
  dt[, spin(p=price, r=region, n=product, base="1")]
)

expect_no_error(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="1")]
)

expect_no_error(
  dt[, spin(p=price, r=region, n=product, w=quantity, base="1")]
)

# check rebasing:
expect_equal(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="1")][,1],
  setNames(rep(1, length(Pq)), Pq)
)

expect_equal(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="2")][,2],
  setNames(rep(1, length(Pq)), Pq)
)

# test quantities versus shares as weights:
dt[, "share" := (price*quantity)/sum(price*quantity), by="region"]

A <- dt[, spin(p=price, r=region, n=product, q=quantity, base="1")]
B <- dt[, spin(p=price, r=region, n=product, w=share, base="1")]

expect_true(
  all(abs(A[!rownames(A)%in%c("medgeworth","gk","geks-medgeworth"),]-B)<1e-5, na.rm=TRUE)
)


# Settings ----------------------------------------------------------------


expect_error(
  dt[, spin(p=price, r=region, n=product, base="1", settings=list(chatty="abc"))]
)

expect_error(
  dt[, spin(p=price, r=region, n=product, base="1", settings=list(connect="abc"))]
)

expect_error(
  dt[, spin(p=price, r=region, n=product, base="1", settings=list(wmethod="abc"))]
)

expect_error(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="1", settings=list(solve="abc"))]
)

expect_error(
  dt[, spin(p=price, r=region, n=product, base="1", settings=list(type="abc"))]
)

expect_error(
  dt[, spin(p=price, r=region, n=product, base="1", settings=list(type="laspey"))]
)

expect_true(
  is.matrix(dt[, spin(p=price, r=region, n=product, base="1", settings=list(type="jevons"))])
)

expect_true(
  nrow(dt[, spin(p=price, r=region, n=product, base="1", settings=list(type="jevons"))])==1L
)

expect_true(
  is.matrix(dt[, spin(p=price, r=region, n=product, base="1", settings=list(type=c("jevons","dutot")))])
)

expect_true(
  nrow(dt[, spin(p=price, r=region, n=product, base="1", settings=list(type=c("jevons","dutot")))])==2L
)


# Non-connected data ------------------------------------------------------


# example data:
set.seed(123)
dt1 <- spin::rdata(R=3, B=1, N=5)
dt2 <- spin::rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)

expect_error(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="1",
            settings=list(chatty=FALSE, connect=FALSE))]
)

expect_equal(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="1",
              settings=list(chatty=FALSE, connect=TRUE))][,1],
  setNames(rep(1, length(Pq)), Pq)
)

expect_equal(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="1",
              settings=list(chatty=FALSE, connect=TRUE))][,4:7],
  matrix(data=NA_real_, ncol=4, nrow=length(Pq), dimnames=list(Pq, 4:7))
)

expect_equal(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="4",
              settings=list(chatty=FALSE, connect=TRUE))][,1:3],
  matrix(data=NA_real_, ncol=3, nrow=length(Pq), dimnames=list(Pq, 1:3))
)

expect_equal(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="4",
              settings=list(chatty=FALSE, connect=TRUE))][,4],
  setNames(rep(1, length(Pq)), Pq)
)

# END

