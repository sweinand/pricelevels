#----------------------------------------#
# Funktionen der bilateralen Preisindizes
#----------------------------------------#

# rm(list=ls(all=TRUE))
# setwd("D:/Empirie/R/Funktionen")

# Es werden zwei Listen bilateraler Preisindizes erzeugt.
# Die Indizes in der Liste indices.bil benötigen
# die zwei Preisvektoren und die zwei
# Mengenvektoren als Input.
# Die Liste indices.bil.rvv benötigt
# die relativen Preise r
# und die Ausgaben der beiden Periode 
# (dafür steht v und v).
# Die Formeln löschen Beobachtungen mit NAs automatisch.
# Der Code besteht aus drei Teilen.

# Teil 1: Es wird die Liste indices.bil erzeugt.
# Sie enthält alle bilateralen Preisindizes als 
# Funktionen zweier Preisvektoren p0 und p1
# und Mengenvektoren x0 und x1.

# Teil 2: Bis auf den UV-Index erfüllten alle 
# aufgelisteten Indizes die Kommensurabilität.
# Preisindizes können dann als Funktionen von r, v0 und v1
# formuliert werden. Diese Liste lautet indices.bil.rvv.
# In den Funktionen stehen a, b, c für r, v0 und v1.

# Teil 3. Es werden noch die elementaren bilateralen 
# Preisindizes erzeugt als Funktionen
# der Preisvektoren p0 and p1.





#--------------------------------------#
#
#          Beispieldatensatz        ####
#
#--------------------------------------#

# Beispiele müssen auskommentiert sein,
# damit sie bei Aufruf dieses Codes in 
# anderene Codes nicht ausgeführt werden.

# Testbeispiel mit konstanten Gesamtausgaben
# p0 <- c(3,1,5)
# p1 <- c(4,1,6)
# x0 <- c(4,8,2)
# x1 <- c(2,10,2)
# pa <- p1/p0
# pb <- p0*x0
# pc <- p1*x1
# sum(p0*x0)
# sum(p1*x1)

# Beispiel aus CPI Manual (2004) p. 346
# p0bsp <- c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
# p1bsp <- c(1.2, 3.0, 1.3, 0.7, 1.4, 0.8)
# p2bsp <- c(1.0, 1.0, 1.5, 0.5, 1.7, 0.6)
# p3bsp <- c(0.8, 0.5, 1.6, 0.3, 1.9, 0.4)
# p4bsp <- c(1.0, 1.0, 1.6, 0.1, 2.0, 0.2)
# Pbsp <- rbind(p0bsp, p1bsp, p2bsp, p3bsp, p4bsp)
# q0bsp <- c(1.0, 1.0, 2.0, 1.0, 4.5, 0.5)
# q1bsp <- c(0.8, 0.9, 1.9, 1.3, 4.7, 0.6)
# q2bsp <- c(1.0, 1.1, 1.8, 3.0, 5.0, 0.8)
# q3bsp <- c(1.2, 1.2, 1.9, 6.0, 5.6, 1.3)
# q4bsp <- c(0.9, 1.2, 2.0, 12.0, 6.5, 2.5)
# Qbsp <- rbind(q0bsp, q1bsp, q2bsp, q3bsp, q4bsp)


#--------------------------------------#
#
#          indices.bill             ####
#
#--------------------------------------#

# Laspeyres
P.La <- function(p0, p1, x0, x1){
  # Beobachtungen mit mindestens einem NA löschen
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  number <- sum(b*a) / sum(b)
  return(number)
}

# Paasche
P.Pa <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  number <- sum(c) / sum(c/a)
  return(number)
}

# Fisher
P.Fi <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  number <- ((sum(b*a)*sum(c)) / (sum(b)*sum(c/a)))^0.5
  return(number)
}

# Törnqvist
P.To <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  prenumber <- 0.5*sum(log(a)*((b/sum(b)+(c/sum(c))))) 
  number <- exp(prenumber)
  return(number)
}

# Walsh
P.Wa <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  number <- (sum(((b/a)*c)^0.5))^(-1) * sum(a*((b/a)*c)^0.5) # b*c/a führt zu integer overflow 
  return(number)
}

# Drobisch
P.Dr <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  number <- 0.5*(sum(b*a)/sum(b) + sum(c)/sum(c/a))
  return(number)
}

# Marshall-Edgeworth
P.ME <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  number <- sum(a*(b+c/a)) / sum(b+c/a)
  return(number)
}

# Walsh-2
P.Wa2 <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  prenumber <- sum(log(a)*((b*c)^0.5/sum((b*c)^0.5))) 
  number <- exp(prenumber)
  return(number)
}

# Walsh-Vartia
P.WV <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  prenumber <- sum(log(a)*b^0.5*c^0.5)/(sum(b)*sum(c))^0.5 
  number <- exp(prenumber)
  return(number)
}

# Theil
P.Th <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  prenumber <- sum(log(a)*(0.5*(b+c)*b*c)^(1/3)) / sum((0.5*(b+c)*b*c)^(1/3)) 
  number <- exp(prenumber)
  return(number)
}

# Vartia (or Vartia I or Montogomary-Vartia; see Balk 2008, p. 27)
P.Va <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  L <- ifelse(round(b,5) != round(c,5), (c-b)/(log(c)-log(b)), b)
  Lsum <- ifelse(round(sum(b),5) != round(sum(c),5), 
                 (sum(c)-sum(b))/(log(sum(c))-log(sum(b))), sum(b))
  prenumber <- (1/Lsum)*sum(log(a)*L) 
  number <- exp(prenumber)
  return(number)
}

# Sato-Vartia (or Vartia II)
P.SV <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- (dat$p0*dat$x0)/sum(dat$p0*dat$x0)
  c <- (dat$p1*dat$x1)/sum(dat$p1*dat$x1)
  L <- ifelse(round(b,5) != round(c,5), (c-b)/(log(c)-log(b)), b)
  Lsum <- sum(L)
  prenumber <- (1/Lsum)*sum(log(a)*L) 
  number <- exp(prenumber)
  return(number)
}

# GUV-Indizes

# Banerjee (GUV-3)
P.Ba <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  d1 <- 1+a
  d2 <- 1+1/a
  number <- (sum(c)/sum(b)) * (sum(b*d1)/sum(c*d2)) 
  return(number)
}

# Davies (GUV-4)
P.Da <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  d1 <- a^0.5
  d2 <- (1/a)^0.5
  number <- (sum(c)/sum(b)) * (sum(b*d1)/sum(c*d2)) 
  return(number)
}

# GUV-5
P.GUV5 <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  d1 <- (1+1/a)^(-1)
  d2 <- (1+a)^(-1)
  number <- (sum(c)/sum(b)) * (sum(b*d1)/sum(c*d2)) 
  return(number)
}

# GUV-6
P.GUV6 <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  d1 <- a^(c/(b+c))
  d2 <- a^(-(b/(b+c)))
  number <- (sum(c)/sum(b)) * (sum(b*d1)/sum(c*d2)) 
  return(number)
}

# Lehr (GUV-7)
P.Le <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  b <- dat$p0*dat$x0
  c <- dat$p1*dat$x1
  d1 <- (b+c)/(b+c/a)
  d2 <- (b+c)/(b*a+c)
  number <- (sum(c)/sum(b)) * (sum(b*d1)/sum(c*d2)) 
  return(number)
}

# Unit Value
P.UV <- function(p0, p1, x0, x1){
  dat.raw <- data.frame(p0,p1,x0,x1)
  dat <- na.omit(dat.raw)
  f1 <- sum(dat$p1*dat$x1)/sum(dat$p0*dat$x0)
  f2 <- sum(dat$x0)/sum(dat$x1)
  number <- f1*f2 
  return(number)
}


indices.bil <- list(P.La, P.Pa, P.Fi, P.Dr, P.To, P.Wa, P.ME, 
                P.Wa2, P.WV, P.Th, P.Va, P.SV,
                P.Ba, P.Da, P.GUV5, P.GUV6, P.Le, P.UV)
index.names.bil <- c("P.La", "P.Pa", "P.Fi", "P.Dr", "P.To", "P.Wa", "P.ME", 
                 "P.Wa2", "P.WV", "P.Th", "P.Va", "P.SV",
                 "P.Ba", "P.Da", "P.GUV5", "P.GUV6", "P.Le", "P.UV")





#---------------------------------------------#
#
#       indices.bill.rvv                   ####
#
#---------------------------------------------#

# Nun wird die Liste indices.bil.rvv erzeugt.
# Zu ihr gehöhren alle Indizes, welche die 
# Kommensurabilität erfüllen.
# Folglich ist der UV-Index nicht dabei.

# Laspeyres
P.La.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  number <- sum(dat$b*dat$a) / sum(dat$b)
  return(number)
}

# Paasche
P.Pa.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  number <- sum(dat$c) / sum(dat$c/dat$a)
  return(number)
}

# Fisher
P.Fi.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  number <- ((sum(dat$b*dat$a)*sum(dat$c)) /
               (sum(dat$b)*sum(dat$c/dat$a)))^0.5
  return(number)
}

# Törnqvist
P.To.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  prenumber <- 0.5*sum(log(dat$a)*((dat$b/sum(dat$b)+(dat$c/sum(dat$c))))) 
  number <- exp(prenumber)
  return(number)
}

# Walsh
P.Wa.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  number <- (sum(((dat$b/dat$a)*dat$c)^0.5))^(-1) *
    sum(dat$a*((dat$b/dat$a)*dat$c)^0.5) 
  # b*c/a führt zu integer overflow 
  return(number)
}

# Drobisch
P.Dr.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  number <- 0.5*(sum(dat$b*dat$a)/sum(dat$b) +
                   sum(dat$c)/sum(dat$c/dat$a))
  return(number)
}

# Marshall-Edgeworth
P.ME.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  number <- sum(dat$a*(dat$b+dat$c/dat$a)) / 
    sum(dat$b+dat$c/dat$a)
  return(number)
}

# Walsh-2
P.Wa2.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  prenumber <- sum(log(dat$a)*((dat$b*dat$c)^0.5 / 
                                 sum((dat$b*dat$c)^0.5))) 
  number <- exp(prenumber)
  return(number)
}

# Walsh-Vartia
P.WV.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  prenumber <- sum(log(dat$a)*dat$b^0.5*dat$c^0.5) / 
    (sum(dat$b)*sum(dat$c))^0.5 
  number <- exp(prenumber)
  return(number)
}

# Theil
P.Th.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  prenumber <- sum(log(dat$a)*(0.5*(dat$b+dat$c)*dat$b*dat$c)^(1/3)) / 
    sum((0.5*(dat$b+dat$c)*dat$b*dat$c)^(1/3)) 
  number <- exp(prenumber)
  return(number)
}

#a<-P4[2,]/P4[1,]
#b<-P4[1,]*Q4[1,]
#c<-P4[2,]*Q4[2,]

# Vartia
P.Va.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  L <- ifelse(round(dat$b,5) != round(dat$c,5), 
              (dat$c-dat$b)/(log(dat$c)-log(dat$b)), dat$b)
  Lsum <- ifelse(round(sum(dat$b),5) != round(sum(dat$c),5),
                 (sum(dat$c)-sum(dat$b)) / 
                   (log(sum(dat$c))-log(sum(dat$b))), sum(dat$b))
  prenumber <- (1/Lsum)*sum(log(dat$a)*L) 
  number <- exp(prenumber)
  return(number)
}

# Sato-Vartia
P.SV.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  L <- ifelse(round(dat$b/sum(dat$b),5) != round(dat$c/sum(dat$c),5), 
              (dat$c/sum(dat$c)-dat$b/sum(dat$b)) / 
                (log(dat$c/sum(dat$c))-log(dat$b/sum(dat$b))), dat$b/sum(dat$b))
  Lsum <- sum(L)
  prenumber <- (1/Lsum)*sum(log(dat$a)*L) 
  number <- exp(prenumber)
  return(number)
}


# GUV-Indizes

# Banerjee (GUV-3)
P.Ba.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  d1 <- 1+dat$a
  d2 <- 1+1/dat$a
  number <- (sum(dat$c)/sum(dat$b)) * (sum(dat$b*d1)/sum(dat$c*d2)) 
  return(number)
}

# Davies (GUV-4)
P.Da.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  d1 <- dat$a^0.5
  d2 <- (1/dat$a)^0.5
  number <- (sum(dat$c)/sum(dat$b)) * (sum(dat$b*d1)/sum(dat$c*d2)) 
  return(number)
}

# GUV-5
P.GUV5.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  d1 <- (1+1/dat$a)^(-1)
  d2 <- (1+dat$a)^(-1)
  number <- (sum(dat$c)/sum(dat$b)) * (sum(dat$b*d1)/sum(dat$c*d2)) 
  return(number)
}

# GUV-6
P.GUV6.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  d1 <- dat$a^(dat$c/(dat$b+dat$c))
  d2 <- dat$a^(-(dat$b/(dat$b+dat$c)))
  number <- (sum(dat$c)/sum(dat$b)) * (sum(dat$b*d1)/sum(dat$c*d2)) 
  return(number)
}

# Lehr (GUV-7)
P.Le.rvv <- function(a,b,c){
  dat.raw <- data.frame(a,b,c)
  dat <- na.omit(dat.raw)
  d1 <- (dat$b+dat$c)/(dat$b+dat$c/dat$a)
  d2 <- (dat$b+dat$c)/(dat$b*dat$a+dat$c)
  number <- (sum(dat$c)/sum(dat$b)) * (sum(dat$b*d1)/sum(dat$c*d2)) 
  return(number)
}

# Es wird kein Unit Value Index 
# in der rvv-Form definiert, da er
# die Kommensurabilität verletzt.

indices.bil.rvv <- list(P.La.rvv, P.Pa.rvv, P.Fi.rvv,
                        P.Dr.rvv, P.To.rvv, P.Wa.rvv, 
                        P.ME.rvv, P.Wa2.rvv, P.WV.rvv, 
                        P.Th.rvv, P.Va.rvv, P.SV.rvv, P.Ba.rvv,
                        P.Da.rvv, P.GUV5.rvv, P.GUV6.rvv,
                        P.Le.rvv)
index.names.bil.rvv <- c("P.La.rvv", "P.Pa.rvv", "P.Fi.rvv", 
                         "P.Dr.rvv", "P.To.rvv", "P.Wa.rvv",  
                         "P.ME.rvv", "P.Wa2.rvv", "P.WV.rvv", 
                         "P.Th.rvv", "P.Va.rvv", "P.SV.rvv,", "P.Ba.rvv",
                         "P.Da.rvv", "P.GUV5.rvv", "P.GUV6.rvv",
                         "P.Le.rvv")



# Prüfung
# N <- length(indices.bil)
# index <- rep(NA,N)
# index.rvv <- rep(NA,N)
# diff <- rep(NA,N)
# for(bil in 1:N){
#  index[bil] <- indices.bil[[bil]](p0bsp, p1bsp, q0bsp, q1bsp) 
#  index.rvv[bil] <- indices.bil.rvv[[bil]](pa,pb,pc)
#  diff[bil] <- index[bil] - index.rvv[bil]
# }

# names(index) <- index.names.bil
# View(index)
# stimmt überein mit Seiten 347 und 349




#----------------------------------------#
#
#               indices.bil.el        ####
#
#----------------------------------------#


# Jevons
P.Je <- function(p0, p1){
  dat.raw <- data.frame(p0,p1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
  number <- prod(a)^(1/length(a)) 
  return(number)
}

# Dutot
P.Du <- function(p0, p1){
  dat.raw <- data.frame(p0,p1)
  dat <- na.omit(dat.raw)
  a <- sum(dat$p1)
  b <- sum(dat$p0)
  number <- a/b 
  return(number)
}  
  
# Dutot
P.Ca <- function(p0, p1){
  dat.raw <- data.frame(p0,p1)
  dat <- na.omit(dat.raw)
  a <- dat$p1/dat$p0
    number <- sum(a)/length(a) 
    return(number)
}
  
indices.bil.el <- list(P.Je, P.Du, P.Ca)
index.names.bil.el <- c("P.Je", "P.Du", "P.Ca")
  
