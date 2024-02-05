# START


# Data with one region only -----------------------------------------------


# example data:
set.seed(123)
dt <- rdata(R=1, B=1, N=4)

expect_equal(
  dt[, jevons(p=price, r=region, n=product, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, laspeyres(p=price, r=region, n=product, w=weight, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, paasche(p=price, r=region, n=product, q=quantity, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, theil(p=price, r=region, n=product, q=quantity, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, lowe(p=price, r=region, n=product, q=quantity, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, young(p=price, r=region, n=product, q=quantity, base="1")],
  c("1"=1)
)

expect_equal(
  dt[, lehr(p=price, r=region, n=product, q=quantity, base="1")],
  c("1"=1)
)


# Data with one product only ----------------------------------------------


# example data with one product only:
set.seed(123)
dt <- rdata(R=4, B=1, N=1)

expect_no_error(
  dt[, jevons(p=price, r=region, n=product, base="1")],
)

expect_no_error(
  dt[, laspeyres(p=price, r=region, n=product, w=weight, base="1")]
)

expect_no_error(
  dt[, paasche(p=price, r=region, n=product, q=quantity, base="1")]
)

expect_no_error(
  dt[, theil(p=price, r=region, n=product, q=quantity, base="1")]
)

expect_no_error(
  dt[, lowe(p=price, r=region, n=product, q=quantity, base="1")]
)

expect_no_error(
  dt[, young(p=price, r=region, n=product, q=quantity, base="1")]
)

expect_no_error(
  dt[, lehr(p=price, r=region, n=product, q=quantity, base="1")]
)


# Data with gaps ----------------------------------------------------------


# example data:
dt <- data.table(
  "region"=as.character(rep(1:3, times=4)),
  "product"=as.character(rep(1:4, each=3)),
  "price"=c(10,12,9, 5,5,4, 2,2,2, 17,20,17),
  "quantity"=c(1000,800,1500, 2000,1500,2300, 5000,4000,8000, 200,150,300)
)
dt <- dt[-c(1,2,8),]
dt[, "share" := price*quantity/sum(price*quantity), by="region"]

# base region 1:
dt1 <- merge(x=dt, y=dt[region=="1",], by="product", suffixes=c("",".base"))
dt1 <- merge(x=dt1, y=dt[region=="3",], by="product", suffixes=c("",".qbase"))

# manual computations:
PJe <- dt1[, exp(mean(log(price/price.base))), by="region"]
PJe <- setNames(PJe$V1, PJe$region)

PCa <- dt1[, mean(price/price.base), by="region"]
PCa <- setNames(PCa$V1, PCa$region)

PHa <- dt1[, 1/mean(price.base/price), by="region"]
PHa <- setNames(PHa$V1, PHa$region)

PDu <- dt1[, mean(price)/mean(price.base), by="region"]
PDu <- setNames(PDu$V1, PDu$region)

PBmw <- dt1[, sum(sqrt(price/price.base))/sum(sqrt(price.base/price)), by="region"]
PBmw <- setNames(PBmw$V1, PBmw$region)

PCSWD <- sqrt(PHa*PCa)

PLa <- dt1[, sum(price*quantity.base)/sum(price.base*quantity.base), by="region"]
PLa <- setNames(PLa$V1, PLa$region)

PPa <- dt1[, sum(price*quantity)/sum(price.base*quantity), by="region"]
PPa <- setNames(PPa$V1, PPa$region)

PFi <- sqrt(PLa*PPa)

PDr <- (PLa+PPa)/2

PPal <- dt1[, sum(share/sum(share)*(price/price.base)), by="region"]
PPal <- setNames(PPal$V1, PPal$region)

PWa <- dt1[, sum(price*sqrt(quantity*quantity.base))/sum(price.base*sqrt(quantity*quantity.base)), by="region"]
PWa <- setNames(PWa$V1, PWa$region)

PGeoLa <- dt1[, exp(sum(share.base/sum(share.base)*log(price/price.base))), by="region"]
PGeoLa <- setNames(PGeoLa$V1, PGeoLa$region)

PGeoPa <- dt1[, exp(sum(share/sum(share)*log(price/price.base))), by="region"]
PGeoPa <- setNames(PGeoPa$V1, PGeoPa$region)

PTo <- dt1[, exp(sum(0.5*(price*quantity/sum(price*quantity)+price.base*quantity.base/sum(price.base*quantity.base))*log(price/price.base))), by="region"]
PTo <- setNames(PTo$V1, PTo$region)

PMe <- dt1[, sum(price*(quantity+quantity.base))/sum(price.base*(quantity+quantity.base)), by="region"]
PMe <- setNames(PMe$V1, PMe$region)

PLo <- dt1[, sum((price.base*quantity.qbase)/sum(price.base*quantity.qbase)*(price/price.base)), by="region"]
PLo <- setNames(PLo$V1, PLo$region)

PYo <- dt1[, sum((price.qbase*quantity.qbase)/sum(price.qbase*quantity.qbase)*(price/price.base)), by="region"]
PYo <- setNames(PYo$V1, PYo$region)

PUv <- dt1[, (sum(price*quantity)/sum(quantity)) / (sum(price.base*quantity.base)/sum(quantity.base)), by="region"]
PUv <- setNames(PUv$V1, PUv$region)

PBan <- dt1[, (sum(price*quantity)/sum(quantity*(price+price.base)/2)) / (sum(price.base*quantity.base)/sum(quantity.base*(price+price.base)/2)), by="region"]
PBan <- setNames(PBan$V1, PBan$region)

PDav <- dt1[, (sum(price*quantity)/sum(quantity*sqrt(price*price.base))) / (sum(price.base*quantity.base)/sum(quantity.base*sqrt(price*price.base))), by="region"]
PDav <- setNames(PDav$V1, PDav$region)

PLehr <- dt1[, (sum(price*quantity)/sum(quantity*(price*quantity+price.base*quantity.base)/(quantity+quantity.base))) / (sum(price.base*quantity.base)/sum(quantity.base*(price*quantity+price.base*quantity.base)/(quantity+quantity.base))), by="region"]
PLehr <- setNames(PLehr$V1, PLehr$region)

dt1[, "w":= sqrt(share/sum(share)*share.base/sum(share.base)), by="region"]
PGeoWa <- dt1[, exp(sum(w/sum(w)*log(price/price.base))), by="region"]
PGeoWa <- setNames(PGeoWa$V1, PGeoWa$region)
dt1[, "w":=NULL]

dt1[, "w":= ((share/sum(share)+share.base/sum(share.base))/2*(share/sum(share))*(share.base/sum(share.base)))^(1/3), by="region"]
PTh <- dt1[, exp(sum(w/sum(w)*log(price/price.base))), by="region"]
PTh <- setNames(PTh$V1, PTh$region)
dt1[, "w":=NULL]

dt1[, "w":= ifelse(abs(share-share.base)<1e-6, share.base, (share/sum(share)-share.base/sum(share.base))/(log(share/sum(share))-log(share.base/sum(share.base)))), by="region"]
PSv <- dt1[, exp(sum(w/sum(w)*log(price/price.base))), by="region"]
PSv <- setNames(PSv$V1, PSv$region)
dt1[, "w":=NULL]

# unweighted indices:
expect_equal(dt[, jevons(p=price, r=region, n=product, base="1")], PJe)
expect_equal(dt[, carli(p=price, r=region, n=product, base="1")], PCa)
expect_equal(dt[, dutot(p=price, r=region, n=product, base="1")], PDu)
expect_equal(dt[, harmonic(p=price, r=region, n=product, base="1")], PHa)
expect_equal(dt[, cswd(p=price, r=region, n=product, base="1")], PCSWD)
expect_equal(dt[, bmw(p=price, r=region, n=product, base="1")], PBmw)

# weighted indices:
expect_equal(dt[, laspeyres(p=price, r=region, n=product, q=quantity, base="1")], PLa)
expect_equal(dt[, paasche(p=price, r=region, n=product, q=quantity, base="1")], PPa)
expect_equal(dt[, fisher(p=price, r=region, n=product, q=quantity, base="1")], PFi)
expect_equal(dt[, toernqvist(p=price, r=region, n=product, q=quantity, base="1")], PTo)
expect_equal(dt[, walsh(p=price, r=region, n=product, q=quantity, base="1")], PWa)
expect_equal(dt[, theil(p=price, r=region, n=product, q=quantity, base="1")], PTh)
expect_equal(dt[, medgeworth(p=price, r=region, n=product, q=quantity, base="1")], PMe)
expect_equal(dt[, lowe(p=price, r=region, n=product, q=quantity, base="1", settings=list(qbase="3"))], PLo)
expect_equal(dt[, young(p=price, r=region, n=product, q=quantity, base="1", settings=list(qbase="3"))], PYo)
expect_equal(dt[, uvalue(p=price, r=region, n=product, q=quantity, base="1")], PUv)
expect_equal(dt[, banerjee(p=price, r=region, n=product, q=quantity, base="1")], PBan)
expect_equal(dt[, davies(p=price, r=region, n=product, q=quantity, base="1")], PDav)
expect_equal(dt[, lehr(p=price, r=region, n=product, q=quantity, base="1")], PLehr)
expect_equal(dt[, palgrave(p=price, r=region, n=product, q=quantity, base="1")], PPal)
expect_equal(dt[, drobisch(p=price, r=region, n=product, q=quantity, base="1")], PDr)
expect_equal(dt[, svartia(p=price, r=region, n=product, q=quantity, base="1")], PSv)
expect_equal(dt[, geolaspeyres(p=price, r=region, n=product, q=quantity, base="1")], PGeoLa)
expect_equal(dt[, geopaasche(p=price, r=region, n=product, q=quantity, base="1")], PGeoPa)
expect_equal(dt[, geowalsh(p=price, r=region, n=product, q=quantity, base="1")], PGeoWa)

# base region 2:
dt2 <- merge(x=dt, y=dt[region=="2",], by="product", suffixes=c("",".base"))
dt2 <- merge(x=dt2, y=dt[region=="3",], by="product", suffixes=c("",".qbase"))

# manual computations:
PJe <- dt2[, exp(mean(log(price/price.base))), by="region"]
PJe <- setNames(PJe$V1, PJe$region)

PCa <- dt2[, mean(price/price.base), by="region"]
PCa <- setNames(PCa$V1, PCa$region)

PHa <- dt2[, 1/mean(price.base/price), by="region"]
PHa <- setNames(PHa$V1, PHa$region)

PDu <- dt2[, mean(price)/mean(price.base), by="region"]
PDu <- setNames(PDu$V1, PDu$region)

PBmw <- dt2[, sum(sqrt(price/price.base))/sum(sqrt(price.base/price)), by="region"]
PBmw <- setNames(PBmw$V1, PBmw$region)

PCSWD <- sqrt(PHa*PCa)

PLa <- dt2[, sum(price*quantity.base)/sum(price.base*quantity.base), by="region"]
PLa <- setNames(PLa$V1, PLa$region)

PPa <- dt2[, sum(price*quantity)/sum(price.base*quantity), by="region"]
PPa <- setNames(PPa$V1, PPa$region)

PFi <- sqrt(PLa*PPa)

PDr <- (PLa+PPa)/2

PPal <- dt2[, sum(share/sum(share)*(price/price.base)), by="region"]
PPal <- setNames(PPal$V1, PPal$region)

PWa <- dt2[, sum(price*sqrt(quantity*quantity.base))/sum(price.base*sqrt(quantity*quantity.base)), by="region"]
PWa <- setNames(PWa$V1, PWa$region)

PGeoLa <- dt2[, exp(sum(share.base/sum(share.base)*log(price/price.base))), by="region"]
PGeoLa <- setNames(PGeoLa$V1, PGeoLa$region)

PGeoPa <- dt2[, exp(sum(share/sum(share)*log(price/price.base))), by="region"]
PGeoPa <- setNames(PGeoPa$V1, PGeoPa$region)

PTo <- dt2[, exp(sum(0.5*(price*quantity/sum(price*quantity)+price.base*quantity.base/sum(price.base*quantity.base))*log(price/price.base))), by="region"]
PTo <- setNames(PTo$V1, PTo$region)

PMe <- dt2[, sum(price*(quantity+quantity.base))/sum(price.base*(quantity+quantity.base)), by="region"]
PMe <- setNames(PMe$V1, PMe$region)

PLo <- dt2[, sum((price.base*quantity.qbase)/sum(price.base*quantity.qbase)*(price/price.base)), by="region"]
PLo <- setNames(PLo$V1, PLo$region)

PYo <- dt2[, sum((price.qbase*quantity.qbase)/sum(price.qbase*quantity.qbase)*(price/price.base)), by="region"]
PYo <- setNames(PYo$V1, PYo$region)

PUv <- dt2[, (sum(price*quantity)/sum(quantity)) / (sum(price.base*quantity.base)/sum(quantity.base)), by="region"]
PUv <- setNames(PUv$V1, PUv$region)

PBan <- dt2[, (sum(price*quantity)/sum(quantity*(price+price.base)/2)) / (sum(price.base*quantity.base)/sum(quantity.base*(price+price.base)/2)), by="region"]
PBan <- setNames(PBan$V1, PBan$region)

PDav <- dt2[, (sum(price*quantity)/sum(quantity*sqrt(price*price.base))) / (sum(price.base*quantity.base)/sum(quantity.base*sqrt(price*price.base))), by="region"]
PDav <- setNames(PDav$V1, PDav$region)

PLehr <- dt2[, (sum(price*quantity)/sum(quantity*(price*quantity+price.base*quantity.base)/(quantity+quantity.base))) / (sum(price.base*quantity.base)/sum(quantity.base*(price*quantity+price.base*quantity.base)/(quantity+quantity.base))), by="region"]
PLehr <- setNames(PLehr$V1, PLehr$region)

dt2[, "w":= sqrt(share/sum(share)*share.base/sum(share.base)), by="region"]
PGeoWa <- dt2[, exp(sum(w/sum(w)*log(price/price.base))), by="region"]
PGeoWa <- setNames(PGeoWa$V1, PGeoWa$region)
dt2[, "w":=NULL]

dt2[, "w":= ((share/sum(share)+share.base/sum(share.base))/2*(share/sum(share))*(share.base/sum(share.base)))^(1/3), by="region"]
PTh <- dt2[, exp(sum(w/sum(w)*log(price/price.base))), by="region"]
PTh <- setNames(PTh$V1, PTh$region)
dt2[, "w":=NULL]

dt2[, "w":= ifelse(abs(share-share.base)<1e-6, share.base, (share/sum(share)-share.base/sum(share.base))/(log(share/sum(share))-log(share.base/sum(share.base)))), by="region"]
PSv <- dt2[, exp(sum(w/sum(w)*log(price/price.base))), by="region"]
PSv <- setNames(PSv$V1, PSv$region)
dt2[, "w":=NULL]

# unweighted indices:
expect_equal(dt[, jevons(p=price, r=region, n=product, base="2")], PJe)
expect_equal(dt[, carli(p=price, r=region, n=product, base="2")], PCa)
expect_equal(dt[, dutot(p=price, r=region, n=product, base="2")], PDu)
expect_equal(dt[, harmonic(p=price, r=region, n=product, base="2")], PHa)
expect_equal(dt[, cswd(p=price, r=region, n=product, base="2")], PCSWD)
expect_equal(dt[, bmw(p=price, r=region, n=product, base="2")], PBmw)

# weighted indices:
expect_equal(dt[, laspeyres(p=price, r=region, n=product, q=quantity, base="2")], PLa)
expect_equal(dt[, paasche(p=price, r=region, n=product, q=quantity, base="2")], PPa)
expect_equal(dt[, fisher(p=price, r=region, n=product, q=quantity, base="2")], PFi)
expect_equal(dt[, toernqvist(p=price, r=region, n=product, q=quantity, base="2")], PTo)
expect_equal(dt[, walsh(p=price, r=region, n=product, q=quantity, base="2")], PWa)
expect_equal(dt[, theil(p=price, r=region, n=product, q=quantity, base="2")], PTh)
expect_equal(dt[, medgeworth(p=price, r=region, n=product, q=quantity, base="2")], PMe)
expect_equal(dt[, lowe(p=price, r=region, n=product, q=quantity, base="2", settings=list(qbase="3"))], PLo)
expect_equal(dt[, young(p=price, r=region, n=product, q=quantity, base="2", settings=list(qbase="3"))], PYo)
expect_equal(dt[, uvalue(p=price, r=region, n=product, q=quantity, base="2")], PUv)
expect_equal(dt[, banerjee(p=price, r=region, n=product, q=quantity, base="2")], PBan)
expect_equal(dt[, davies(p=price, r=region, n=product, q=quantity, base="2")], PDav)
expect_equal(dt[, lehr(p=price, r=region, n=product, q=quantity, base="2")], PLehr)
expect_equal(dt[, palgrave(p=price, r=region, n=product, q=quantity, base="2")], PPal)
expect_equal(dt[, drobisch(p=price, r=region, n=product, q=quantity, base="2")], PDr)
expect_equal(dt[, svartia(p=price, r=region, n=product, q=quantity, base="2")], PSv)
expect_equal(dt[, geolaspeyres(p=price, r=region, n=product, q=quantity, base="2")], PGeoLa)
expect_equal(dt[, geopaasche(p=price, r=region, n=product, q=quantity, base="2")], PGeoPa)
expect_equal(dt[, geowalsh(p=price, r=region, n=product, q=quantity, base="2")], PGeoWa)

# check rebasing:
expect_equal(
  dt[, carli(p=price, r=region, n=product, base="1")][1],
  c("1"=1)
)

expect_equal(
  dt[, carli(p=price, r=region, n=product, base="2")][2],
  c("2"=1)
)

expect_equal(
  dt[, laspeyres(p=price, r=region, n=product, q=quantity, base="1")][1],
  c("1"=1)
)

expect_equal(
  dt[, laspeyres(p=price, r=region, n=product, q=quantity, base="2")][2],
  c("2"=1)
)

expect_equal(
  dt[, lehr(p=price, r=region, n=product, q=quantity, base="1")][1],
  c("1"=1)
)

expect_equal(
  dt[, lehr(p=price, r=region, n=product, q=quantity, base="2")][2],
  c("2"=1)
)

# test quantities versus shares as weights:
dt[, "share" := (price*quantity)/sum(price*quantity), by="region"]

expect_equal(
  dt[, laspeyres(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, laspeyres(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, paasche(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, paasche(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, fisher(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, fisher(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, toernqvist(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, toernqvist(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, walsh(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, walsh(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, geolaspeyres(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, geolaspeyres(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, geopaasche(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, geopaasche(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, geowalsh(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, geowalsh(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, theil(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, theil(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, drobisch(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, drobisch(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, palgrave(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, palgrave(p=price, r=region, n=product, w=share, base="1")]
)

expect_equal(
  dt[, svartia(p=price, r=region, n=product, q=quantity, base="1")],
  dt[, svartia(p=price, r=region, n=product, w=share, base="1")]
)


# Settings ----------------------------------------------------------------


expect_error(
  dt[, laspeyres(p=price, r=region, n=product, q=quantity, settings=list(chatty="abc"))]
)

expect_error(
  dt[, laspeyres(p=price, r=region, n=product, q=quantity, settings=list(connect="abc"))]
)

expect_error(
  dt[, lowe(p=price, r=region, n=product, q=quantity, settings=list(qbase=NA_character_))]
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
  dt[, laspeyres(p=price, r=region, n=product, q=quantity, base="1",
              settings=list(chatty=FALSE, connect=TRUE))][1],
  c("1"=1)
)

expect_equal(
  dt[, laspeyres(p=price, r=region, n=product, q=quantity, base="1",
              settings=list(chatty=FALSE, connect=TRUE))][4:7],
  setNames(rep(NA_real_, 4), 4:7)
)

expect_equal(
  dt[, laspeyres(p=price, r=region, n=product, q=quantity, base="4",
              settings=list(chatty=FALSE, connect=TRUE))][1:3],
  setNames(rep(NA_real_, 3), 1:3)
)

expect_equal(
  dt[, laspeyres(p=price, r=region, n=product, q=quantity, base="4",
              settings=list(chatty=FALSE, connect=TRUE))][4],
  c("4"=1)
)


# Misc --------------------------------------------------------------------


# check consistency of helper function, i.e., if matrix
# function produce the same output as vectorized function

# sample data:
set.seed(1)
dt <- pricelevels::rdata(R=5, B=7, N=1, gaps=0.25)
dt[, "share" := price*quantity/sum(price*quantity), by="region"]
data.table::setnames(dt, c("group","weight","r","n","is_sale","p","q","share"))

# reshape data to matrices:
P <- as.matrix(
  x=data.table::dcast(data=dt, formula=n~r, fun.aggregate=mean, value.var="p", fill=NA),
  rownames="n")

Q <- as.matrix(
  x=data.table::dcast(data=dt, formula=n~r, fun.aggregate=sum, value.var="q", fill=NA),
  rownames="n")

W <- as.matrix(
  x=data.table::dcast(data=dt, formula=n~r, fun.aggregate=mean, value.var="share", fill=NA),
  rownames="n")

# jevons:
system.time(PJ1 <- pricelevels:::Pmatrix$jevons(P=P, Q=Q))
system.time(PJ2 <- dt[, jevons(p=p, r=r, n=n, base="1")])
expect_equal(PJ1, PJ2)

# carli:
system.time(PC1 <- pricelevels:::Pmatrix$carli(P=P, Q=Q))
system.time(PC2 <- dt[, carli(p=p, r=r, n=n, base="1")])
expect_equal(PC1, PC2)

# dutot:
system.time(PD1 <- pricelevels:::Pmatrix$dutot(P=P, Q=Q))
system.time(PD2 <- dt[, dutot(p=p, r=r, n=n, base="1")])
expect_equal(PD1, PD2)

# harmonic:
system.time(PH1 <- pricelevels:::Pmatrix$harmonic(P=P, Q=Q))
system.time(PH2 <- dt[, harmonic(p=p, r=r, n=n, base="1")])
expect_equal(PH1, PH2)

# bmw:
system.time(PBmw1 <- pricelevels:::Pmatrix$bmw(P=P, Q=Q))
system.time(PBmw2 <- dt[, bmw(p=p, r=r, n=n, base="1")])
expect_equal(PBmw1, PBmw2)

# cswd:
system.time(PCSWD1 <- pricelevels:::Pmatrix$cswd(P=P, Q=Q))
system.time(PCSWD2 <- dt[, cswd(p=p, r=r, n=n, base="1")])
expect_equal(PCSWD1, PCSWD2)

# marshall-edgeworth:
system.time(PMe1 <- pricelevels:::Pmatrix$medgeworth(P=P, Q=Q))
system.time(PMe2 <- dt[, medgeworth(p=p, r=r, n=n, q=q, base="1")])
expect_equal(PMe1, PMe2)

# lowe:
system.time(PLo1 <- pricelevels:::Pmatrix$lowe(P=P, Q=Q, qbase=2))
system.time(PLo2 <- dt[, lowe(p=p, r=r, n=n, q=q, base="1", settings=list(qbase="2"))])
expect_equal(PLo1, PLo2)

system.time(PLo3 <- pricelevels:::Pmatrix$lowe(P=P, Q=Q, qbase=NULL))
system.time(PLo4 <- dt[, lowe(p=p, r=r, n=n, q=q, base="1", settings=list(qbase=NULL))])
expect_equal(PLo3, PLo4)

# young:
system.time(PYo1 <- pricelevels:::Pmatrix$young(P=P, Q=Q, qbase=2))
system.time(PYo2 <- dt[, young(p=p, r=r, n=n, q=q, base="1", settings=list(qbase="2"))])
expect_equal(PYo1, PYo2)

system.time(PYo3 <- pricelevels:::Pmatrix$young(P=P, Q=Q, qbase=NULL))
system.time(PYo4 <- dt[, young(p=p, r=r, n=n, q=q, base="1", settings=list(qbase=NULL))])
expect_equal(PYo3, PYo4)

# uvalue:
system.time(PUv1 <- pricelevels:::Pmatrix$uvalue(P=P, Q=Q))
system.time(PUv2 <- dt[, uvalue(p=p, r=r, n=n, q=q, base="1")])
expect_equal(PUv1, PUv2)

# banerjee:
system.time(PBa1 <- pricelevels:::Pmatrix$banerjee(P=P, Q=Q))
system.time(PBa2 <- dt[, banerjee(p=p, r=r, n=n, q=q, base="1")])
expect_equal(PBa1, PBa2)

# davies:
system.time(PDa1 <- pricelevels:::Pmatrix$davies(P=P, Q=Q))
system.time(PDa2 <- dt[, davies(p=p, r=r, n=n, q=q, base="1")])
expect_equal(PDa1, PDa2)

# lehr:
system.time(PLe1 <- pricelevels:::Pmatrix$lehr(P=P, Q=Q))
system.time(PLe2 <- dt[, lehr(p=p, r=r, n=n, q=q, base="1")])
expect_equal(PLe1, PLe2)

# laspeyres:
system.time(PL1 <- pricelevels:::Pmatrix$laspeyres(P=P, Q=Q))
system.time(PL2 <- dt[, laspeyres(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PL1, PL2)

system.time(PL3 <- pricelevels:::Pmatrix$laspeyres(P=P, W=W))
system.time(PL4 <- dt[, laspeyres(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PL3, PL4)

# compare weights versus quantities:
expect_equal(PL1, PL3)

# paasche:
system.time(PP1 <- pricelevels:::Pmatrix$paasche(P=P, Q=Q))
system.time(PP2 <- dt[, paasche(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PP1, PP2)

system.time(PP3 <- pricelevels:::Pmatrix$paasche(P=P, W=W))
system.time(PP4 <- dt[, paasche(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PP3, PP4)

# compare weights versus quantities:
expect_equal(PP1, PP3)

# fisher:
system.time(PF1 <- pricelevels:::Pmatrix$fisher(P=P, Q=Q))
system.time(PF2 <- dt[, fisher(p=p, q=q, r=r, n=n,base="1")])
expect_equal(PF1, PF2)

system.time(PF3 <- pricelevels:::Pmatrix$fisher(P=P, W=W))
system.time(PF4 <- dt[, fisher(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PF3, PF4)

# compare weights versus quantities:
expect_equal(PF1, PF3)

# palgrave:
system.time(PPal1 <- pricelevels:::Pmatrix$palgrave(P=P, Q=Q))
system.time(PPal2 <- dt[, palgrave(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PPal1, PPal2)

system.time(PPal3 <- pricelevels:::Pmatrix$palgrave(P=P, W=W))
system.time(PPal4 <- dt[, palgrave(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PPal3, PPal4)

# compare weights versus quantities:
expect_equal(PPal1, PPal3)

# drobisch:
system.time(PDr1 <- pricelevels:::Pmatrix$drobisch(P=P, Q=Q))
system.time(PDr2 <- dt[, drobisch(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PDr1, PDr2)

system.time(PDr3 <- pricelevels:::Pmatrix$drobisch(P=P, W=W))
system.time(PDr4 <- dt[, drobisch(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PDr3, PDr4)

# compare weights versus quantities:
expect_equal(PDr1, PDr3)

# walsh:
system.time(PW1 <- pricelevels:::Pmatrix$walsh(P=P, Q=Q))
system.time(PW2 <- dt[, walsh(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PW1, PW2)

system.time(PW3 <- pricelevels:::Pmatrix$walsh(P=P, W=W))
system.time(PW4 <- dt[, walsh(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PW3, PW4)

# compare weights versus quantities:
expect_equal(PW1, PW3)

# theil:
system.time(PTh1 <- pricelevels:::Pmatrix$theil(P=P, Q=Q))
system.time(PTh2 <- dt[, theil(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PTh1, PTh2)

system.time(PTh3 <- pricelevels:::Pmatrix$theil(P=P, W=W))
system.time(PTh4 <- dt[, theil(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PTh3, PTh4)

# compare weights versus quantities:
expect_equal(PTh1, PTh3)

# toernqvist:
system.time(PT1 <- pricelevels:::Pmatrix$toernqvist(P=P, Q=Q))
system.time(PT2 <- dt[, toernqvist(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PT1, PT2)

system.time(PT3 <- pricelevels:::Pmatrix$toernqvist(P=P, W=W))
system.time(PT4 <- dt[, toernqvist(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PT3, PT4)

# compare weights versus quantities:
expect_equal(PT1, PT3)

# sato-vartia:
system.time(PSv1 <- pricelevels:::Pmatrix$svartia(P=P, Q=Q))
system.time(PSv2 <- dt[, svartia(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PSv1, PSv2)

system.time(PSv3 <- pricelevels:::Pmatrix$svartia(P=P, W=W))
system.time(PSv4 <- dt[, svartia(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PSv3, PSv4)

# compare weights versus quantities:
expect_equal(PSv1, PSv3)

# geolaspey:
system.time(PGeoLa1 <- pricelevels:::Pmatrix$geolaspeyres(P=P, Q=Q))
system.time(PGeoLa2 <- dt[, geolaspeyres(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PGeoLa1, PGeoLa2)

system.time(PGeoLa3 <- pricelevels:::Pmatrix$geolaspeyres(P=P, W=W))
system.time(PGeoLa4 <- dt[, geolaspeyres(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PGeoLa3, PGeoLa4)

# compare weights versus quantities:
expect_equal(PGeoLa1, PGeoLa3)

# geopaasche:
system.time(PGeoPa1 <- pricelevels:::Pmatrix$geopaasche(P=P, Q=Q))
system.time(PGeoPa2 <- dt[, geopaasche(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PGeoPa1, PGeoPa2)

system.time(PGeoPa3 <- pricelevels:::Pmatrix$geopaasche(P=P, W=W))
system.time(PGeoPa4 <- dt[, geopaasche(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PGeoPa3, PGeoPa4)

# compare weights versus quantities:
expect_equal(PGeoPa1, PGeoPa3)

# geowalsh:
system.time(PGeoWa1 <- pricelevels:::Pmatrix$geowalsh(P=P, Q=Q))
system.time(PGeoWa2 <- dt[, geowalsh(p=p, q=q, r=r, n=n, base="1")])
expect_equal(PGeoWa1, PGeoWa2)

system.time(PGeoWa3 <- pricelevels:::Pmatrix$geowalsh(P=P, W=W))
system.time(PGeoWa4 <- dt[, geowalsh(p=p, w=share, r=r, n=n, base="1")])
expect_equal(PGeoWa3, PGeoWa4)

# compare weights versus quantities:
expect_equal(PGeoWa1, PGeoWa3)

# lowe, young and laspey should be identical if qbase=base:
expect_equal(
  dt[, lowe(p=p, r=r, n=n, q=q, base="1", settings=list(qbase="1"))],
  dt[, laspeyres(p=p, r=r, n=n, q=q, base="1")]
)

expect_equal(
  dt[, young(p=p, r=r, n=n, q=q, base="1", settings=list(qbase="1"))],
  dt[, laspeyres(p=p, r=r, n=n, q=q, base="1")]
)

# END

