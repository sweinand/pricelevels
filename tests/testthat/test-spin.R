# START

# price indices:
Punw <- c("carli","dutot","harmonic","jevons")
Punw <- sort(c(Punw, paste("geks", Punw, sep="-"),"cpd","nlcpd"))
Pw <- c("fisher","laspey","paasche","toernq","walsh")
Pw <- sort(c(Pw, paste("geks", Pw, sep="-"),"cpd","nlcpd","geradi","idb","rao"))
Pq <- c("geary-khamis")
Pall <- sort(unique(c(Punw, Pw, Pq)))


# Data with one region only -----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=1, B=1, N=4)

expect_equal(
  dt[, spin(p=price, r=region, n=product, base="1")],
  matrix(data=1, ncol=1, nrow=length(Punw), dimnames=list(Punw, "1"))
)

expect_equal(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="1")],
  matrix(data=1, ncol=1, nrow=length(Pall), dimnames=list(Pall, "1"))
)

res <- matrix(data=1, ncol=1, nrow=length(Pall), dimnames=list(Pall, "1"))
res[rownames(res)=="geary-khamis",1] <- NA
expect_equal(
  dt[, spin(p=price, r=region, n=product, w=quantity, base="1")],
  res
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
  setNames(rep(1, length(Pall)), Pall)
)

expect_equal(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="2")][,2],
  setNames(rep(1, length(Pall)), Pall)
)

# test quantities versus shares as weights:
dt[, "share" := (price*quantity)/sum(price*quantity), by="region"]

expect_true(
  all(abs(
    dt[, spin(p=price, r=region, n=product, q=quantity, base="1")]-dt[, spin(p=price, r=region, n=product, w=share, base="1")]
    )<1e-5, na.rm=TRUE)
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

expect_true(
  is.matrix(dt[, spin(p=price, r=region, n=product, base="1", settings=list(type="jevons"))])
)

expect_true(
  nrow(dt[, spin(p=price, r=region, n=product, base="1", settings=list(type="jevons"))])==1L
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
  setNames(rep(1, length(Pall)), Pall)
)

expect_equal(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="1",
              settings=list(chatty=FALSE, connect=TRUE))][,4:7],
  matrix(data=NA_real_, ncol=4, nrow=length(Pall), dimnames=list(Pall, 4:7))
)

expect_equal(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="4",
              settings=list(chatty=FALSE, connect=TRUE))][,1:3],
  matrix(data=NA_real_, ncol=3, nrow=length(Pall), dimnames=list(Pall, 1:3))
)

expect_equal(
  dt[, spin(p=price, r=region, n=product, q=quantity, base="4",
              settings=list(chatty=FALSE, connect=TRUE))][,4],
  setNames(rep(1, length(Pall)), Pall)
)

# END

