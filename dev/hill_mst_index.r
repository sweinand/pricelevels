# START

# Minimum spanning trees
library(spin)
library(data.table)
base <- "01"

# example data:
set.seed(123)
dt <- rdata(R=10,B=1,N=7,gaps=0.2)
r.lvl <- levels(dt$region)
R <- nlevels(dt$region)

# compute laspeyres-paasche-spread:
pls <- dt[, index.pairs(p=price, r=region, n=product, q=quantity, settings=list(type=c("laspey","paasche"), all.pairs=FALSE))]
pls <- pls[base!=region,]
pls[, "spread" := abs(log(laspeyres/paasche))]
pls <- pls[order(spread),]

# Kruskal's algorithm to compute minimum spanning tree:
out <- res <- list()
for(j in 1:nrow(pls)){

  res.tmp <- c(pls[j,1], pls[j,2])
  out <- out[lengths(out)>0L]
  if(!any(sapply(X=out, FUN=function(z) all(res.tmp%in%z)))){

    res[[j]] <- res.tmp

    if(any(sapply(X=out, FUN=function(z) any(res.tmp%in%z)))){

      for(i in seq_along(out)){

        if(any(res.tmp%in%out[[i]])){
          out[[i]] <- unique(append(x=out[[i]], values=res.tmp))
        }

      }

    }else{

      out[[j]] <- res.tmp

    }

  }

  if(length(out)>1){
    cn <- combn(x=seq_along(out), m=2)
    for(i in 1:ncol(cn)){
      if(length(intersect(out[[cn[1,i]]], out[[cn[2,i]]]))>0){
        out[[cn[1,i]]] <- unique(x=c(out[[cn[1,i]]], out[[cn[2,i]]]))
        out[[cn[2,i]]] <- NULL
        break
      }
    }
  }

  res <- res[lengths(res)>0]
  if(length(res)>=(R-1) & any(lengths(out)>=(R-1))) break

}

(mst <- res[lengths(res)>0])
df.mst <- data.frame("region"=unlist(lapply(X=res, "[[", 1L)),
                     "base"=unlist(lapply(X=res, "[[", 2L)))
df.mst

# chain bilateral price indices:
base.freq <- table(df.mst$base)
base.tmp <- names(base.freq[which.max(base.freq)])
if(base.tmp%in%df.mst$region){
  idx <- df.mst$region%in%base.tmp
  df.mst[idx,] <- c(df.mst$base[idx], df.mst$region[idx])
}
df.mst.sub <- df.mst[df.mst$base!=base.tmp,]
df.mst.base <- df.mst[df.mst$base==base.tmp,]
P <- dt[, spin::jevons(p=price, r=region, n=product, base=base.tmp)]
df.mst.base$index <- P[match(table=names(P), x=df.mst.base$region)]
while(nrow(df.mst.sub)>0){

  idx1 <- which(df.mst.sub$region%in%df.mst.base$region)
  res1 <- vector(mode="numeric", length=length(idx1))
  for(j in seq_along(idx1)){
    P <- dt[, spin::jevons(p=price, r=region, n=product, base=df.mst.sub$region[idx1[j]])]
    res1[j] <- df.mst.base$index[df.mst.base$region==df.mst.sub$region[idx1[j]]]*P[names(P)==df.mst.sub$base[idx1[j]]]
  }

  idx2 <- which(df.mst.sub$base%in%df.mst.base$region)
  res2 <- vector(mode="numeric", length=length(idx2))
  for(j in seq_along(idx2)){
    P <- dt[, spin::jevons(p=price, r=region, n=product, base=df.mst.sub$base[idx2[j]])]
    res2[j] <- df.mst.base$index[df.mst.base$region==df.mst.sub$base[idx2[j]]]*P[names(P)==df.mst.sub$region[idx2[j]]]
  }

  df.mst.base <- data.frame(
    "region"=c(df.mst.base$region, df.mst.sub$base[idx1], df.mst.sub$region[idx2]),
    "base"=base.tmp,
    "index"=c(df.mst.base$index, res1, res2))
  df.mst.sub <- df.mst.sub[-c(idx1, idx2),]

}

# mst price index:
P <- setNames(c(1, df.mst.base$index), c(base.tmp, as.character(df.mst.base$region)))
if(is.null(base)) P <- P/prod(P^(1/length(P))) else P <- P/P[names(P)==base]
P <- P[match(x=r.lvl, table=names(P))]
names(P) <- r.lvl
P

# comparison to Jevons and GEKS-Jevons:
PJev <- dt[, jevons(p=price, r=region, n=product, base=base)]
PGEKS <- dt[, geks(p=price, r=region, n=product, base=base)]

sqrt(mean((PGEKS-P)^2))
sqrt(mean((PJev-P)^2))
plot(P)
points(PJev, col="red")
points(PGEKS, col="blue")

# END
