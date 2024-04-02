# START

# Title:    Available price indices
# Date:     8 March 2024

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
                                   "gerardi","rhajargasht","mcarli","mjevons",
                                   "mdutot","mharmonic"),
                          "type"="multilateral",
                          "uses_none"=c(T,T,F,F,F,F,F,T,T,T,T),
                          "uses_q"=c(T,T,T,T,T,T,T,F,F,F,F),
                          "uses_w"=c(T,T,F,T,T,T,T,F,F,F,F)))

# list of all available price indices:
pindices <- rbind(bilP, multP)

# export to internal data:
usethis::use_data(pindices, internal=TRUE, overwrite=TRUE)

# END
