# START

# Title:    Available price indices
# Date:     2 April 2024

library(data.table)

# bilateral price indices:
bilPunw <- sapply(X=pricelevels:::Pmatrix, FUN=function(z) is.null(formals(z)$Q))
bilPq <- sapply(X=pricelevels:::Pmatrix, FUN=function(z) !is.null(formals(z)$Q))
bilPw <- sapply(X=pricelevels:::Pmatrix, FUN=function(z) !is.null(formals(z)$W))
bilP <- data.table("name"=names(pricelevels:::Pmatrix),
                   "type"="bilateral",
                   "uses_none"=bilPunw,
                   "uses_q"=bilPq,
                   "uses_w"=bilPw)

# multilateral price indices:
multP <- copy(bilP)
multP[, "name" := paste("geks", name, sep="-")]
multP[, "type" := "multilateral"]
multP <- rbind(multP,
               data.table("name"=c("cpd","nlcpd","gkhamis","rao","ikle",
                                   "rhajargasht","gerardi"),
                          "type"="multilateral",
                          "uses_none"=c(T,T,T,T,T,T,F),
                          "uses_q"=c(T,T,T,T,T,T,T),
                          "uses_w"=c(T,T,F,T,T,T,T)))

# list of all available price indices:
pindices <- rbind(bilP, multP)

# export to internal data:
usethis::use_data(pindices, internal=TRUE, overwrite=TRUE)

# END
