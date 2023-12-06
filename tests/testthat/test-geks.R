# START


# Data with one region only -----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=1, B=1, N=4)

expect_error(
  dt[, index.pairs(p=price, r=region, n=product, settings=list(type="bla"))]
)

expect_error(
  dt[, index.pairs(p=price, r=region, n=product, settings=list(type="laspey"))]
)

expect_warning(
  dt[, index.pairs(p=price, r=region, n=product, q=quantity, settings=list(type="young", qbase="bla"))]
)

expect_equal(
  is.data.table(dt[, index.pairs(p=price, r=region, n=product, settings=list(type="jevons"))]),
  TRUE
)

expect_equal(
  colnames(dt[, index.pairs(p=price, r=region, n=product, settings=list(type=c("jevons","carli")))]),
  c("base","region","jevons","carli")
)

expect_equal(
  dt[, index.pairs(p=price, r=region, n=product, settings=list(type="jevons"))],
  data.table("base"=as.factor(1), "region"=as.factor(1), "jevons"=1, key=c("base","region"))
)

expect_equal(
  dt[, index.pairs(p=price, r=region, n=product, settings=list(type=c("jevons","carli")))],
  data.table("base"=as.factor(1), "region"=as.factor(1), "jevons"=1, "carli"=1, key=c("base","region"))
)

expect_equal(
  dt[, geks(p=price, r=region, n=product)],
  c("1"=1)
)


# Data with one product only ----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=4, B=1, N=1)

expect_no_error(
  dt[, index.pairs(p=price, r=region, n=product, settings=list(type="jevons"))]
)

expect_no_error(
  dt[, geks(p=price, r=region, n=product)],
)


# Data with gaps ----------------------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=3, B=1, N=4, gaps=0.2)
dt[, "share" := price*quantity/sum(price*quantity), by="region"]
dt[, "weight" := rweights(r=region, b=product, type=~b)]

# index.pairs():
res.expec <- rbind(
  dt[, jevons(p=price, r=region, n=product, base="1")],
  dt[, jevons(p=price, r=region, n=product, base="2")],
  dt[, jevons(p=price, r=region, n=product, base="3")]
)
rownames(res.expec) <- c("1","2","3")

expect_equal(
  as.matrix(dcast(data=dt[, index.pairs(p=price, r=region, n=product, settings=list(type="jevons"))], formula=base~region, value.var="jevons"), rownames="base"),
  res.expec
)

res.expec[lower.tri(res.expec)] <- NA

expect_equal(
  as.matrix(dcast(data=dt[, index.pairs(p=price, r=region, n=product, settings=list(type="jevons", all.pairs=FALSE))], formula=base~region, value.var="jevons"), rownames="base"),
  res.expec
)

expect_equal(
  nrow(dt[, index.pairs(p=price, r=region, n=product, settings=list(type="jevons", all.pairs=TRUE))]),
  3^2
)

expect_equal(
  nrow(dt[, index.pairs(p=price, r=region, n=product, settings=list(type="jevons", all.pairs=FALSE))]),
  3*(3+1)/2
)

# geks():
geks.est <- dt[, geks(p=price, r=region, n=product, w=weight, base=NULL, settings=list(type="toernq"))]
geks.est1 <- dt[, geks(p=price, r=region, n=product, w=weight, base="1", settings=list(type="toernq"))]
geks.est2 <- dt[, geks(p=price, r=region, n=product, w=weight, base="2", settings=list(type="toernq"))]

expect_equal(is.vector(geks.est1), TRUE)
expect_equal(is.vector(geks.est2), TRUE)
expect_equal(geks.est1[1], c("1"=1))
expect_equal(geks.est2[2], c("2"=1))
expect_equal(prod(geks.est), 1)
expect_equal(geks.est1, geks.est2/geks.est2[1])
expect_equal(geks.est1, geks.est/geks.est[1])

# expenditure share weights versus quantities identical:
geks.est3 <- dt[, geks(p=price, r=region, n=product, w=share, base="1", settings=list(type="toernq"))]
geks.est4 <- dt[, geks(p=price, r=region, n=product, q=quantity, base="1", settings=list(type="toernq"))]
expect_equal(geks.est3, geks.est4)

# wmethod='shares' still transitive:
geks.est5 <- dt[, geks(p=price, r=region, n=product, q=quantity, base="1", settings=list(type="toernq", wmethod="shares"))]
geks.est6 <- dt[, geks(p=price, r=region, n=product, q=quantity, base=NULL, settings=list(type="toernq", wmethod="shares"))]
expect_equal(geks.est5, geks.est6/geks.est6[1])

# multiple index types at once:
geks.est7 <- dt[, geks(p=price, r=region, n=product, q=quantity, base="1", settings=list(type=c("toernq","jevons")))]
expect_equal(is.matrix(geks.est7), TRUE)
expect_equal(dim(geks.est7), c(2,3))
expect_true(all(grepl("geks-", rownames(geks.est7))))


# Settings ----------------------------------------------------------------


expect_no_error(
  dt[, geks(p=price, r=region, n=product, q=quantity, base="1",
            settings=list(type="toernq", chatty=FALSE))]
)

expect_error(
  dt[, geks(p=price, r=region, n=product, q=quantity, base="1",
            settings=list(type="toernq", wmethod="bla", chatty=FALSE))]
)

expect_error(
  dt[, index.pairs(p=price, r=region, n=product, q=quantity,
                   settings=list(type="toernq", all.pairs="bla", chatty=FALSE))]
)

expect_error(
  dt[, index.pairs(p=price, r=region, n=product, q=quantity, settings=list(type="bla"))]
)


# Non-connected data ------------------------------------------------------


# example data:
set.seed(123)
dt1 <- spin::rdata(R=3, B=1, N=5)
dt2 <- spin::rdata(R=4, B=1, N=4)
dt2[, "region":=factor(region, labels=4:7)]
dt2[, "product":=factor(product, labels=6:9)]
dt <- rbind(dt1, dt2)

expect_equal(
  dim(as.matrix(dcast(data=dt[, index.pairs(p=price, r=region, n=product, q=quantity, settings=list(chatty=FALSE))], formula=base~region, value.var="jevons"), rownames="base")),
  c(7,7)
)

expect_equal(
  nrow(dt[, index.pairs(p=price, r=region, n=product, q=quantity, settings=list(chatty=FALSE))]),
  3*3+4*4
)

expect_equal(
  dt[, geks(p=price, r=region, n=product, q=quantity, base="1",
             settings=list(chatty=FALSE, connect=TRUE))][1],
  c("1"=1)
)

expect_equal(
  dt[, geks(p=price, r=region, n=product, q=quantity, base="1",
             settings=list(chatty=FALSE, connect=TRUE))][4:7],
  setNames(rep(NA_real_, 4), 4:7)
)

expect_equal(
  dt[, geks(p=price, r=region, n=product, q=quantity, base="4",
             settings=list(chatty=FALSE, connect=TRUE))][1:3],
  setNames(rep(NA_real_, 3), 1:3)
)

expect_equal(
  dt[, geks(p=price, r=region, n=product, q=quantity, base="4",
             settings=list(chatty=FALSE, connect=TRUE))][4],
  c("4"=1)
)


# Misc --------------------------------------------------------------------


# example data without gaps:
set.seed(123)
dt <- rdata(R=3, B=1, N=4)

geks.est <- dt[, geks(p=price, r=region, n=product, base=NULL)]
geks.est1 <- dt[, geks(p=price, r=region, n=product, base="1")]
geks.est2 <- dt[, geks(p=price, r=region, n=product, base="2")]
jev.est1 <- dt[, jevons(p=price, r=region, n=product, base="1")]

expect_equal(geks.est1[1], c("1"=1))
expect_equal(geks.est2[2], c("2"=1))
expect_equal(prod(geks.est), 1)
expect_equal(geks.est1, geks.est2/geks.est2[1])
expect_equal(geks.est1, jev.est1)

# example data with weights=1:
dt <- rdata(R=5, B=1, N=10)
dt[, "weights" := 1]

expect_equal(
  dt[, index.pairs(p=price, r=region, n=product, settings=list(type="jevons"))]$jevons,
  dt[, index.pairs(price, region, product, w=weights, settings=list(type="toernq"))]$toernq
)

expect_equal(
  dt[, index.pairs(price, region, product, settings=list(type = "carli"))]$carli,
  dt[, index.pairs(price, region, product, w=weights, settings=list(type = "laspey"))]$laspey
)

expect_equal(
  dt[, index.pairs(price, region, product, settings=list(type = "harmonic"))]$harmonic,
  dt[, index.pairs(price, region, product, w=weights, settings=list(type = "paasche"))]$paasche
)


# example data with weights=~b:
set.seed(123)
dt <- rdata(R=3, B=1, N=4)
dt[, "weight" := rweights(r=region, b=product, type=~b)]

geks.est1 <- dt[, geks(p=price, r=region, n=product, w=weight, base="1", settings=list(type="toernq"))]
geks.est2 <- dt[, geks(p=price, r=region, n=product, w=weight, base=NULL, settings=list(type="toernq"))]
toernq.est1 <- dt[, toernq(p=price, r=region, n=product, w=weight, base="1")]

expect_equal(geks.est1[1], c("1"=1))
expect_equal(prod(geks.est2), 1)
expect_equal(geks.est1, geks.est2/geks.est2[1])
expect_equal(geks.est1, toernq.est1)

# example data with gaps:
set.seed(123)
dt <- rdata(R=5, B=1, N=9)

# reciprocal of paasche identical to laspeyres:
expect_equal(
  t(1/as.matrix(dcast(data=dt[, index.pairs(p=price, r=region, n=product, q=quantity, settings=list(type="paasche"))], formula=base~region, value.var="paasche"), rownames="base")),
  as.matrix(dcast(data=dt[, index.pairs(p=price, r=region, n=product, q=quantity, settings=list(type="laspey"))], formula=base~region, value.var="laspey"), rownames="base")
)

# hence, geks-fisher, geks-laspeyres, and geks-paasche identical:
expect_equal(
  dt[, geks(p=price, r=region, n=product, q=quantity, settings=list(type="fisher"))],
  dt[, geks(p=price, r=region, n=product, q=quantity, settings=list(type="laspey"))]
)

expect_equal(
  dt[, geks(p=price, r=region, n=product, q=quantity, settings=list(type="fisher"))],
  dt[, geks(p=price, r=region, n=product, q=quantity, settings=list(type="paasche"))]
)

# END
