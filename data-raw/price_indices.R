# START

# Title:    Available price indices
# Date:     20 November 2023

library(data.table)

# bilateral price indices:
bilPunw <- c("jevons","carli","dutot","harmonic","cswd")
bilPq <- c("medgeworth")
bilPw <- c("toernq","laspey","paasche","walsh","fisher","palgrave",
           "svartia","drobisch","theil","geolaspey","geopaasche","geowalsh")
bilP <- data.table("name"=c(bilPunw, bilPq, bilPw),
                   "type"="bilateral",
                   "uses_q"=c(rep(FALSE, length(bilPunw)), rep(TRUE, length(bilPq)), rep(TRUE, length(bilPw))),
                   "uses_w"=c(rep(FALSE, length(bilPunw)), rep(FALSE, length(bilPq)), rep(TRUE, length(bilPw))))

# multilateral price indices:
multP <- copy(bilP)
multP[, "name" := paste("geks", name, sep="-")]
multP[, "type" := "multilateral"]
multP <- rbind(multP,
               data.table("name"=c("cpd","nlcpd","gk","rao","idb","gerardi"),
                          "type"="multilateral",
                          "uses_q"=c(T,T,T,T,T,T),
                          "uses_w"=c(T,T,F,T,T,T)))

# list of all available price indices:
pindices <- rbind(bilP, multP)

# export to internal data:
usethis::use_data(pindices, internal=TRUE, overwrite=TRUE)

# END
