# START

# Title:    Available price indices
# Date:     11 January 2024

library(data.table)

# bilateral price indices:
bilPunw <- sapply(X=spin:::Pmatrix, FUN=function(z) is.null(formals(z)$Q))
bilPq <- sapply(X=spin:::Pmatrix, FUN=function(z) !is.null(formals(z)$Q))
bilPw <- sapply(X=spin:::Pmatrix, FUN=function(z) !is.null(formals(z)$W))
bilP <- data.table("name"=names(spin:::Pmatrix),
                   "type"="bilateral",
                   "uses_none"=bilPunw,
                   "uses_q"=bilPq,
                   "uses_w"=bilPw)

# multilateral price indices:
multP <- copy(bilP)
multP[, "name" := paste("geks", name, sep="-")]
multP[, "type" := "multilateral"]
multP <- rbind(multP,
               data.table("name"=c("cpd","nlcpd","gkhamis","rao","idb","gerardi"),
                          "type"="multilateral",
                          "uses_none"=c(T,T,F,F,F,F),
                          "uses_q"=c(T,T,T,T,T,T),
                          "uses_w"=c(T,T,F,T,T,T)))

# list of all available price indices:
pindices <- rbind(bilP, multP)

# export to internal data:
usethis::use_data(pindices, internal=TRUE, overwrite=TRUE)

# END
