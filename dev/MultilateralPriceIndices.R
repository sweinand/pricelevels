#-----------------------------------------------------------------#
# Multilaterale Preisindizes für Daten aus mehr als zwei Perioden 
#-----------------------------------------------------------------#

# Es geht hier nur um echte multilaterale Preisindizes
# und nicht um pseudo-multilaterale Preisindizes 
# wie dem augmented Lehr.
# Die Indizes sind für interregionale oder auch
# intertemporale Vergleiche einsetzbar.
# Sie berücksichtigen auch die verschiedenen 
# Splicing Varianten (nur bei intertemporal sinnvoll).

# Inhaltsübersicht:
# Teil A: Beispieldaten
# Teil B: RGEKS/GEKS
# Teil C: Geary-Khamis
# Teil D: TPD

# Datengrundlage sind immer eine Preismatrix P
# und eine Mengenmatrix Q, wobei
# die Regionen/Perioden zeilenweise angeordnet sind
# und die Güter Spaltenweise

# rm(list=ls(all=TRUE))
source("D:/Empirie/R/Funktionen/BilateralPriceIndices.R")
# setwd("D:/Empirie/R/Funktionen")


#------------------------------------------------------------------#
#
#            Teil A: Beispieldatensatz #####
#
#------------------------------------------------------------------#



# Zur Veranschaulichung wird ein Datensatz aus 
# Maddison and Rao (1996, S. 16) verwendet.

# region <- c("USA", "India", "Brazil")
# product <- c("Wheat", "Potatoes", "Milk", "Lamb")
# # Es wird ein dataframe aus allen Kombinationen
# # der beiden Vektoren geschaffen.
# # Jede "Zeile" des Dataframes ist eine eigene Kombination.
# data <- expand.grid(Region = region, Commodity = product)
# price <- c(1.42, 22, 4, 1.1, 17, 3.2, 2.85, 40, 8, 24, 380, 80)
# quantity <- c(30, 20, 26, 50, 26, 42, 120, 43, 80, 140, 30, 45)
# # Der Dataframe wird um die Preise und Mengen erweitert.
# data <- data.frame(data, Price = price, Quantity = quantity)
# # Die Daten werden in eine Preis- und eine Mengenmatrix
# # (jeweils Dataframes) umgewandelt.
# # Dabei Regionen in die Zeilen und Güter in Spalten.
# # Dazu wird die Spalte "Commodity" als erstes Element
# # einer Liste definiert und die Spalte "Region" als zweites.
# # Die Funktion mean mit tapply berechnet für jede
# # Commodity-Region-Kombination (Faktorkombination)
# # den Mittelwert. Da jeweils nur ein Wert existiert,
# # ist dies der Mittelwert.
# # Das Ergebnis wird als Matrix ausgegeben, da Faktorkombinationen
# # betrachtet wurden.
# P.Beispiel <- tapply(data$Price, list(data$Commodity, data$Region), mean)
# Q.Beispiel <- tapply(data$Quantity, list(data$Commodity, data$Region), mean)
# # Dies war ein einfacher Weg, um eine Preismatrix zu erzeugen.
# # Transponiere Ergebnisse, um Regionen in den Zeilen zu haben.
# P.Beispiel1 <- t(P.Beispiel)
# Q.Beispiel1 <- t(Q.Beispiel)
# 
# # Erweiterte Datensätze
# P.Beispiel2 <- rbind(P.Beispiel1, c(8.22, 6.62, 16.01, 147),
#                      c(3.44, 2.01, 5.33, 50),
#                      c(10.22, 5.62, 13.01, 155))
# Q.Beispiel2 <- rbind(Q.Beispiel1, c(20,40,110,120),
#                      c(13,24,36,21),
#                      c(17,33,113,100))
# # mean movement splicing erfordert noch größeren Datensatz als Beispiel2
# 
# P.Beispiel3 <- rbind(P.Beispiel2, c(11.22, 7.62, 15.01, 136),
#                      c(3.74, 1.81, 6.13, 46))
# Q.Beispiel3 <- rbind(Q.Beispiel2, c(15,45,100,130),
#                      c(15,21,32,24))
# 
# rownames(P.Beispiel2) <- c("USA", "India", "Brazil", 
#                            "Estonia", "Latvia", "Poland")
# rownames(P.Beispiel3) <- c("USA", "India", "Brazil", 
#                            "Estonia", "Latvia", "Poland",
#                            "Romania", "Bulgaria")
# 
# # Datensatz mit Lücken
# 
# # Eine Matrix der NAs wird erzeugt.
# # Sie hat die gleiche Dimension wie P.Beispiel3.
# NA.Beispiel4 <- matrix(1,nrow=dim(P.Beispiel3)[1],
#                        ncol=dim(P.Beispiel3)[2])
# NA.Beispiel4[1,3] <- NA
# NA.Beispiel4[2,1] <- NA
# NA.Beispiel4[2,2] <- NA
# NA.Beispiel4[3,4] <- NA
# NA.Beispiel4[5,2] <- NA
# NA.Beispiel4[6,4] <- NA
# NA.Beispiel4[7,2] <- NA
# NA.Beispiel4[7,4] <- NA
# NA.Beispiel4[8,1] <- NA
# # Die NAs werden in die P- und Q-Matrix eingespeist.
# P.Beispiel4 <- P.Beispiel3*NA.Beispiel4
# Q.Beispiel4 <- Q.Beispiel3*NA.Beispiel4
# 
# # Beispiel aus CPI Manual (2004) S. 346
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




#--------------------------------------------------------------------#
#
#                 Teil B: RGEKS/GEKS ####
#
#--------------------------------------------------------------------#

# Vorgegeben werden der Funktion die Preis- und Mengenmatrix,
# entweder die zugrundeliegende bilaterale Indexformel
# oder die bilateralen Preisindizes und schließlich
# die Größe des rollierenden Fensters.

# Die Funktion P.GEKS arbeitet in drei Stufen.
# 1) Falls die bilateralen Indexwerte
# aller Periodenpaare nicht vorab 
# eingespeist wurden, berechnet
# sie aus der vorgegebenen bilateralen Indexformel zunächst
# sämtliche bilateralen Indexwerte und speichert
# sie in P.bil.matrix ab.
# 2) Dann führt sie für das erste Fenster
# die GEKS-Korrektur durch
# und speichert die sich ergebenden
# Preisniveaus in GEKS.box ab.
# Dabei hat die erste Periode des Fensters
# immer den Wert 1.
# Wenn die Größe des Fensters kleiner ist als 
# die Gesamtzahl der Perioden der Preis- und Mengenmatrix,
# wird das Fenster danach um eine Periode verschoben
# und Schritt 2) wird erneut durchgeführt.
# In GEKS.box erhält auch das zweite Fenster
# eine eigene Zeile. Dieser Prozess wird bis zum
# letzten Fenster durchgeführt.
# 3) Dann werden auf die fertige GEKS.box alle möglichen 
# Splicing Varianten angewendet.

# Im Standardfall ist der Output der Funktion
# eine Liste mit nur einem Element:
# Element 1: Für jede Splicing Variante der Vektor aller Preisniveaus.
# Wenn gewünscht werden zusätzlich ausgegeben:
# Element 2: GEKS.box
# Element 3: alle bilateralen Ergebnisse


# In den Argumenten der Funktion P.GEKS
# werden zunächst die Preis- und die Mengenmatrix angegeben.
# (Zeilen sind Regionen/Perioden, Spalten sind Güter)
# Dann wird die bilaterale Preisindexformel (z.B. Törnqvist)
# aus der Liste indices.bil durch 
# Angabe der Position bil (Törnqvist ist 4) ausgewählt.
# Dies ist redundant, wenn die bilateralen Indexwerte
# vorab angegeben werden.
# Es wird auch eine Fenstergröße w festgelegt.
# Bei interregionalen Vergleichen sollte w
# normalerweise der Anzahl der Zeilen von P und Q entsprechen,
# denn bei solchen Vergleichen ist Splicing nicht notwendig/sinnvoll.
# Schließlich gibt man an, ob nur die GEKS-Indexwerte
# als Output weitergereicht werden sollen (Standardfall), 
# oder auch die bilateralen Indizes und die GEKS.box.

# Im GEKS-Index werden Funktionen der 
# bilaterale Preisindizes benötigt.
# Sie wurden anfangs bereits geladen und
# stehen in der Liste indices.bil.
# Diese können auch mit Lücken umgehen.
# Wenn aber ein bilateraler Index
# nicht berechenbar ist, wird der hier
# definierte GEKS auch nicht berechenbar sein.
# Bei Scannerdaten taucht dieses Problem nicht auf,
# sofern die Fenstergröße nicht zu groß ist.

# Die nachfolgende Funktion P.Bilateral.Matrix
# erzeugt in effizienter Weise
# eine Matrix bilateraler Preisindizes
# mit dem Namen P.bil.matrix.

# P.bil.matrix hat auf der Diagonalen immer den Wert 1.
# Zeile ist die Vergleichsperiode
# Spalte ist die Basisperiode.
# Wenn die Perioden 0,1,..,3 betrachtet wurden,
# steht auf Position [4,1] der Index für
# Vergleichsperiode 3 und Basisperiode 0.
# Oberes Dreieck hat die Kehrwerte des unteren Dreiecks.

# Die Funktion wird in der GEKS-Funktion verwendet, 
# kann aber auch eingesetzt werden, 
# um vorab die bilateralen Indexwerte zu berechnen.
# Die Vorabberechnung ist bei Verwendung der GEKS-Funktion
# im Rolling Windows GEKS deutlich schneller,
# da die bilateralen Indexwerte 
# nicht bei jedem verschobenen Fenster neu berechnet werden.

# Das Argument bil greift aus der Liste indices.bil die
# gewünschte bilaterale Indexformel heraus (Törnqvist: bil = 5).

P.Bilateral.Matrix <- function(P,Q,bil){
  T <- dim(P)[1]
  P.bil.matrix <- diag(T)  # Diagonalmatrix
  # Zeile ist die Vergleichsperiode
  # Spalte ist die Basisperiode
  
  # Berechnung aller bilateralen Indexwerte
  for(j in 1:(T-1)){   # Basisperiode
    for(i in (j+1):T){ # Vergleichsperiode
      tmp <-  indices.bil[[bil]](P[j,], P[i,], Q[j,], Q[i,]) 
      P.bil.matrix[i,j] <- tmp
      P.bil.matrix[j,i] <- 1/tmp
    }
  }
  P.bil.matrix
}




#_________________ Beginn der Funktion P.GEKS_____________________________

P.GEKS <- function(P, Q, bil, w=dim(P)[1], bilmat=NULL, fulloutput=FALSE){
  T <- dim(P)[1]

# Stufe 1: Matrix der bilateralen Preisindizes 
#.............................................

# Wenn die Matrix nicht vorab eingespeist wurde (else-Fall),
# dann muss sie erzeugt werden (Standardfall).
# Nur dann ist das Argument bil relevant.

if(is.null(bilmat)){
    P.bil.matrix <- P.Bilateral.Matrix(P,Q,bil)
  } else {
    P.bil.matrix <- bilmat
  }


# Stufe 2: Berechnung der Preisniveaus für jedes Fenster 
#.......................................................

# Es wird aus der Matrix der bilateralen 
# Preisindizes P.bil.matrix oben links 
# ein Quadrat mit w Zeilen und Spalten ausgewählt.
# Dieses enthält alle bilateralen Preisindizes
# für w Perioden, also bei w = 4 die Indizes
# für die Perioden 0 bis 3.
# Für dieses Fenster wird die GEKS-Korrektur der 
# ursprünglichen bilateralen Indexwerte durchgeführt.
# Dabei wird das Preisniveau der ersten Periode
# auf 1 normiert und die anderen
# w-1 Preisniveaus relativ zu diesem berechnet.
# Dies gelingt mit dem geometrischen Mittel
# aller indirekten Preisindizes zwischen Basis-
# und Vergleichsperiode.
# Dies erzeugt für das Fenster einen Vektor mit w Preisniveaus.
# Dieser Vektor wird in die oberste Zeile der Matrix
# GEKS.box eingetragen. Im Gegensatz zu P.bil.matrix
# ist GEKS.box eine Matrix der PreisNIVEAUS.

# Wenn w=T (Fenstergröße umfasst alle Perioden/Regionen;
# dies ist der Normalfall in interregionalen Vergleichen)
# ergibt sich der normale GEKS.Index
# und nur ein Vektor wird berechnet.

# Wenn aber w<T, wird das Fenster in P.bil.matrix eine Position 
# nach rechts unten verschoben und die Operation wiederholt.
# Insgesamt ergeben sich so T-w+1 
# ausgeschnittene Quadrate in P.bil.matrix
# und damit Vektoren in GEKS.box.
# Sie werden zeilenweise in GEKS.box abgespeichert,
# wobei jeder Vektor um 1 nach rechts verschoben wird.
# GEKS.box hat somit T-w+1 Zeilen und T Spalten.
# Nicht alle NAs werden in dieser Matrix ersetzt.

# Jeder Zeilenvektor in GEKS.box beginnt mit dem Wert 1.
# Rechts in GEKS.box ergibt sich eine Null-Matrix.
# Hülse für GEKS.box wird geschaffen.
GEKS.box <- cbind(diag(T-w+1), matrix(0,ncol=w-1,nrow=T-w+1))

# Es werden die Preisniveaus für GEKS.box
# berechnet und eingetragen.
# z ist die Zeile in GEKS.box
for(z in 1:(T-w+1)){
  # Greife für jede Zeile z die zweite bis letzte
  # relevante Position j (die Vergleichsperioden) heraus 
  # (erste Position ist schon mit 1 belegt)
  for(j in (z+1):(z+w-1)){
    # Erstelle Hülse für Vektor, welcher die
    # insgesamt w über eine Brückenperiode i indirekt
    # berechneten bilateralen Indizes enthält. 
    fa <- rep(NA,w)
    for(i in 1:w){
      # z+i-1 ist dann die Brückenperiode, über die die 
      # Vergleichsperiode j und die Basisperiode z
      # indirekt verglichen werden. Für i=1 ist
      # die Brückenperiode gleich z.
      # Trage die w Produktergebnisse in fa ein.
      fa[i] <- P.bil.matrix[j,(z+i-1)]*P.bil.matrix[(z+i-1),z]
    }
    # Bilde das geometrische Mittel der Werte in fa 
    # und trage das Ergebnis in GEKS.box in Zeile z und Spalte j ein.
    GEKS.box[z,j] <- prod(fa)^(1/w)
  }
}


# Stufe 3: Splicing 
#.............................

# Diese Stufe ist nur für w<T durchzuführen.
# Für w=T wird direkt das Resultat aus GEKS.box
# als Endresultat verwendet.

# Falls w<T müssen die Zeilen von GEKS.box miteinander
# verknüpft werden (Splicing), um den jeweils aktuellen
# Indexwert zu erhalten.
# Ausgangspunkt ist immer der letzte Wert des
# in der Vorperiode betrachteten Fensters.
# Von diesem Wert wird innerhalb des Vorperiodenfensters
# auf eine frühere Periode als Brückenperiode 
# zurückgerechnet und dann
# von diesem Wert mit Hilfe des aktuellsten
# Wertes des neuen Fensters der
# aktuelle Indexwert berechnet,
# der dann wiederum Ausgangswert für das nächste
# Splicing ist.
# Bei der Rückrechnung innerhalb des Vorperiodenfensters
# auf eine Brückenperiode
# kann man maximal bis zur ersten Periode des aktuellen Fensters
# zurückrechnen (genannt: window splicing).
# Aber auch jede aktuellere Periode ist als Brücke möglich.
# Wenn man gar nicht zurückrechnet, erhält man das
# movement splicing.
# Mittelt man alle möglichen Varianten, 
# erhält man das mean splicing.

# r sei der Index für Rückrechnung zur Brückenperiode.
# r = 0 ist keine Rückrechnung (movemenet splicing).
# r = w-1 ist maximale Rückrechung (window splicing)
# Die Ergebnisse werden in der Matrix GEKS.All
# gespeichert, wobei jede Spalte eine eigene
# Rückrechnungsvariante darstellt.
# Das ergibt w-1 Spalten.
# Eine zusätzliche Spalte wird für
# mean splicing hinzugefügt,
# also das geometrische Mittel
# aller anderer Spalten.
# Es gibt somit w Splicing Methoden.

if(w==T){
  GEKS.All <- GEKS.box
  }
else{ 
  # Spalten in GEKS.All sind die verschiedenen
  # Splicing Methoden (r=0 ist movement splice).
  # Jede Spalte erhält einen vollständigen Satz
  # an Preisniveaus (Spaltenlänge ist T).
  GEKS.All <- matrix(NA,T,w) # leere Matrixhülle    
  for(r in 0:(w-2)){
    # Zeile 1 in GEKS.box (sie besteht aus w Preisniveaus)
    # sind die Ergebnisse des ältesten Fensters.
    # Diese werden in allen Spalten
    # in GEKS.All als oberste Einträge eingesetzt.
    # Die erste Spalte in GEKS.All ergibt sich für r=0,
    # wird also mit r+1 korrekt (movement splice) angesprochen.
    # Auch die weiteren Spalten werden mit r+1 
    # korrekt angesprochen.
    for(j in 1:w){
      GEKS.All[j,r+1] <- GEKS.box[1,j]
    }
    # Für die weiteren Positionen in jeder Spalte müssen
    # die Werte nun einzeln aus Multiplikation des Basiswertes
    # mit Rückrechnung und Vorrechnung ermittelt werden.
    # j ist die Zeile in GEKS.All.
    for(j in (w+1):T){
      GEKS.All[j,(r+1)] <- 
        # j ist die Spalte in GEKS.box und
        # die Zeile in GEKS.All.
        # Wenn also in GEKS.All das Preisniveau
        # der Zeile j berechnet werden soll,
        # dann steht der Basiswert in GEKS.all
        # in der Zeile j-1.
        GEKS.All[(j-1),(r+1)] * # Basiswert
        # Der Basiswert wird um r zurückgerechnet.
        # Dafür wird das Vorperiodenfenster verwendet.
        # Für j=w+1 ist es das Fenster in der ersten Zeile,
        # Für j=w+2 das Fenster in der zweiten Zeile.
        # Das Fenster wird also durch j-w korrekt angesprochen.
        # Innerhalb des Vorperiodenfensters wird der
        # letzte Wert (dies ist zugleich der Basiswert, Position j-1)
        # mit dem Wert der Brückenperiode verglichen. Letzterer liegt
        # in der Zeile um r Positionen vor dem 
        # Basiswert, also an Position j-1-r.
        (GEKS.box[(j-w),(j-1-r)] / GEKS.box[(j-w),(j-1)]) * # Rückrechnung
        # Die Vorrechnung erfolgt mit dem Fenster
        # der aktuellen Periode. Dieses ist somit in Zeile j-w+1.  
        # Dort wird der aktuellste Wert
        # (er ist in Zeile j-w+1 der GEKS.box an Position j)
        # mit dem Brückenwert
        # (er ist in Zeile j-w+1 der GEKS.box an Position j-1-r)
        # verglichen.
        GEKS.box[(j-w+1),j] / GEKS.box[(j-w+1),(j-1-r)] # Vorrechnung
    }
  }
  # Es muss in GEKS.All noch die Spalte für 
  # mean splicing ermittelt werden.
  # Es ist die letzte Spalte.
  for(i in 1:T){
    GEKS.All[i,w] <- prod(GEKS.All[i,c(1:(w-1))])^(1/(w-1))
  }

# Melser (2016) schlägt vor, in der GEKS-Korrektur
# die Preisniveaus aller vorangegangenen Perioden
# als aus dem Vorperiodenfenster fest vorgegeben
# zu betrachteten und nur den aktuellen Indexwert
# der Korrektur zu unterziehen, also nur ein
# geometrisches Mittel über alle Brückenperioden
# zu berechnen. Damit werden die Stufen 2 und 3
# zu einer sehr einfachen Stufe verknüpft.
# Allerdings ist der Beginn komplizierter,
# weil anfangs noch nicht die "Brückenköpfe" existieren.
# Es werden für nachfolgende Programmierung
# 2*w Beobachtungen benötigt.
# Wenn weniger vorliegen, bleibt der entsprechende
# Vektor mit NA gefüllt.
  
GEKS.Incr <- rep(NA,T)
if(T>=2*w){
# Die Preisniveaus der ersten w Perioden
# können direkt aus der ersten Zeile von GEKS.box
# übernommen werden.
GEKS.Incr[1:w] <- GEKS.box[1,(1:w)]

# Das Preisniveau der Periode w+1
# kann nur auf ein einziges früheres Fenster zurückgreifen.
# Es handelt sich eigentlich um den Movement Splice.
# Das Preisniveau der Periode w+2
# greift auf zwei Fenster zurück
# und mittelt dann die beiden Resultate.
# Dieser Aufbau setzt sich fort bis zu Periode w+(w-1).
# Dort ist erstmals die vollständige Verlinkung möglich.
# Bei w=4 sind das Periode 7 und drei Verlinkungen.
# Im einzelnen wären das:
# GEKS.Incr[w+1] <- GEKS.Incr[w]*(GEKS.box[2,w+1]/GEKS.box[2,w])
# GEKS.Incr[w+2] <- ((GEKS.Incr[w]*(GEKS.box[3,w+2]/GEKS.box[3,w]))*
#                   (GEKS.Incr[w+1]*(GEKS.box[3,w+2]/GEKS.box[3,w+1])))^(1/2)
# GEKS.Incr[w+3] <- ((GEKS.Incr[w]*(GEKS.box[4,w+3]/GEKS.box[4,w]))*
#                   (GEKS.Incr[w+1]*(GEKS.box[4,w+3]/GEKS.box[4,w+1]))*
#                   (GEKS.Incr[w+2]*(GEKS.box[4,w+3]/GEKS.box[4,w+2])))^(1/3)
# Dies muss in allgemeine Form gegossen werden.
# Gelingt mit Schleife in Schleife.
# Äußere Schleife (Index i) legt fest, welcher Wert in GEKS.Incr 
# berechnet werden soll. Außerdem wird
# Hülse für die Faktoren angelegt,
# die in der inneren Schleife berechnet werden.
# Die innere Schleife (Index j) erzeugt dann die 
# Faktoren zur Berechnung des GEKS.Incr-Wertes.
# Diese Faktoren werden außerhalb der inneren
# Schleife zum geometrischen Mittel verknüpft.
for(i in 1:(w-1)){
  # Schaffe Hülse für die i Faktoren des geometrischen Mittels
  faktoren <- rep(NA,i)
  for(j in 1:i){
    faktoren[j] <- GEKS.Incr[w+j-1]*
      (GEKS.box[i+1,w+i]/GEKS.box[i+1,w+j-1])  
  }
  GEKS.Incr[w+i] <- prod(faktoren)^(1/i) # berechnet geometrisches Mittel
}
# Die Preisniveaus der Perioden 2w bis T ergeben sich wie
# in der letzten Schleife. Es werden in der inneren Schleife
# also w-1 Faktoren berechnet. 
# Index i spricht nun aber direkt die Periode an.
for(i in (2*w):T){
  # Schaffe Hülse für die (w-1) Faktoren des geometrischen Mittels
  faktoren <- rep(NA,(w-1))
  for(j in 1:(w-1)){
    faktoren[j] <- GEKS.Incr[i-j]*
      (GEKS.box[i-w+1,i]/GEKS.box[i-w+1,i-j])  
  }
  GEKS.Incr[i] <- prod(faktoren)^(1/(w-1)) # berechnet geometrisches Mittel
}
}

# Füge die Ergebnisse zu GEKS.All hinzu.
# Incr ganz an den Anfang stellen, dann Mean und dann den Rest
GEKS.All <- cbind(GEKS.Incr, GEKS.All[,w], GEKS.All[,1:(w-1)])

colnames(GEKS.All) <- c("GEKS.MeanMove", "GEKS.Mean", 
                        paste("GEKS.", 0:(w-2), sep=""))
# Wenn GEKS.MeanMove mit NAs, dann lösche aus GEKS.All
if(identical(GEKS.Incr,rep(NA,T))){
  GEKS.All <- GEKS.All[,-1]
}

}

if(fulloutput==TRUE){
  GEKS.results <- list(GEKS.All, GEKS.box, P.bil.matrix)
  return(GEKS.results)
  } else {
  GEKS.results <- list(GEKS.All)
  return(GEKS.results)
  }
}

#_________________ Ende der Funktion P.GEKS_____________________________



# Anwendung auf Beispieldatensätze
# Resultat.GEKS1 <- P.GEKS(P.Beispiel1, Q.Beispiel1, bil=5, w=dim(P.Beispiel1)[1])
# Resultat.GEKS2 <- P.GEKS(P.Beispiel2, Q.Beispiel2, bil=5, w=4)
# Resultat.GEKS3 <- P.GEKS(P.Beispiel3, Q.Beispiel3, bil=5, w=4)[[1]]
# Resultat.GEKS4 <- P.GEKS(P.Beispiel4, Q.Beispiel4, bil=5, w=4)[[1]]

# Prüfung durch Ergebnisvergleich mit R-Package IndexNumR
# library(IndexNumR)
# Im Paket enthalten ist der Datensatz
# View(CES_sigma_2)
# Indexberechnung (Törnqvist, mean splicing,
# Fenstergröße 11 Perioden)
# ErgebnisA <- GEKSIndex(CES_sigma_2, pvar = "prices", 
#                       qvar = "quantities", pervar = "time",
#          prodID = "prodID", indexMethod = "tornqvist", 
#          window=11, splice = "movement")

# Nun mit meiner Funktion.
# Dafür Vektor in Matrix verwandeln.
# P.B <- matrix(CES_sigma_2[,2],12,4)
# Q.B <- matrix(CES_sigma_2[,3],12,4)
# ErgebnisB <- P.GEKS(P.B, Q.B, bil=5, w=11, fulloutput=TRUE)[[1]][,2]
# vergleich <- cbind(ErgebnisA, ErgebnisB)
# View(vergleich)






#---------------------------------------------------------------#
#
#       Teil C: Geary-Khamis (inkl. Rolling Window)          ####
#
#---------------------------------------------------------------#

# Der Index lässt sich auf Perioden (intertemporal)
# oder Regionen (interregional) anwenden.
# Falls intertemporal angewendet, können
# verschiedene Fenstergrößen gewählt werden.
# Alle möglichen Splicing-Varianten sind
# dann ebenfalls automatisch eingebaut.
# Datenlücken sind nicht vorgesehen!

# In den Argumenten der Funktion sind:
# P = Preismatrix (Güter in Zeilen)
# Q = Mengenmatrix
# tol = Grad der Genauigkeit
# w = Größe des Fensters

# Der Berechnungsprozess besitzt 2 Hauptstufen.

# Stufe 1
# 1a) Die Funktion Pi_func wird definiert. 
# Sie liefert für gegebene 
# Preisniveaus PPP die Güterwerte pi.i.
# 1b) Die Funktion PPP_func wird defniert.
# Sie liefert für gegebene pi.i die PPPs.
# 1c) Die Funktion P.GK wird definiert.
# Sie berechnet in einer Iteration
# der Funktionen Pi_func und PPP_func den GK-Index.

# Stufe 2 
# Die Funktion P.GK wird definiert.
# Sie berechnet mit Hilfe von P.GK
# für jedes Fenster einen eigenen GK-Index und
# fasst die Ergebnisse in der Matrix GK.box zusammen.
# Falls w<T gewählt wurde (intertemporale Anwendung),
# wendet die Funktion anschließend
# alle Splicing-Varianten auf GK.box an
# und berechnet für jede Variante einen GK-Index.
# Falls w=T wird nur ein Vektor berechnet.

# Achtung: Die nachfolgende Formel P_func verwendet
# nicht exakt die Formeln (22) und (23) in Auer (2012, S. 46)
# sondern diese Formeln in etwas umgeschriebener Form (aber äquivalent).


# Stufe 1:
#---------------#

Pi_func <- function(P.w, Q.w, PPP){
  # PPP ist Vektor der Preisniveaus der Perioden/Regionen.
  # Erzeuge Matrix der Ausgaben
  V.w <- P.w*Q.w
  # Summiere für ein Item (Spalte) über
  # alle Perioden/Regionen (Zeilen) die mit PPP deflationierten Ausgaben.
  # Verwende dafür das innere Produkt aus PPP und V-Spalte.
  v.sum.defl <- (1/PPP)%*%V.w 
  # Berechne für jedes Item (Spalte) die Gesamtmenge
  # über alle Perioden/Regionen (Zeilen) hinweg.
  x.sum <- apply(Q.w,2,sum)  # Die 2 steht für spaltenweise Ausführung.
  pi.i <- v.sum.defl/x.sum
  pi.i
}


PPP_func <- function(P.w, Q.w, pi.i){
  V.w <- P.w*Q.w
  # Gesamtausgaben in jeder Periode/Region (Zeile)
  V.sum.period <- apply(V.w,1,sum) # 1 steht für zeilenweise Ausführung.
  # Erzeuge aus Vektor pi.i eine Matrix
  # die in jeder Zeile den Vektor pi.i zeigt.
  # Die Zeilenanzahl der Matrix soll den Perioden/Regionen entsprechen
  Pr <- matrix(rep(pi.i, dim(P.w)[1]), nrow=dim(P.w)[1],
               ncol=dim(P.w)[2], byrow=T)
  # Gesamtausgaben jeder Periode/Region, wenn die
  # künstlichen Preise Pr verwendet werden
  V.Pr <- Pr*Q.w
  V.Pr.sum.period <- apply(V.Pr,1,sum)
  # Dividiere beide Summen und normiere so,
  # dass Element PPP[1] den Wert 1 besitzt
  PPP <- (V.sum.period / V.Pr.sum.period) / 
    (V.sum.period[1] / V.Pr.sum.period[1])
  PPP
}

# Nun wird die eigentliche iterative 
# Geary Khamis-Funktion GK() definiert.
# Sie bezieht sich auf eine
# vorgegebene Fenstergröße w=T oder w<T.
# Argumente sind wie zuvor. Hinzu kommt:
# tol = Abbruchkriterium für Iteration (geforderte Genauigkeit)
GK <- function(P.w, Q.w, tol){
  # Die ersten PPP-Werte für die Anwendung 
  # von P_func sind alle 1.
  PPP <- rep(1, dim(P.w)[1])
  # Führe die nachfolgenden Befehle immer wieder aus,
  # bis das Abbruchkriterium erfüllt ist.
  repeat{
    # Speicher alten PPP-Vektor gesondert ab,
    # um Genauigkeitsvergleich zu machen
    PPPold <- PPP
    # Erzeuge pi.i-Vektor aus P_func()
    pi.i <- Pi_func(P.w, Q.w, PPP)
    # Verwende diese pi.i-Werte um neues PPP zu erzeugen
    PPP <- PPP_func(P.w, Q.w, pi.i)
    # Falls die Summe der absoluten Abweichungen
    # zwischen PPPold und PPP unter tol gehen,
    # beende den Prozess.
    if(sum(abs(PPPold-PPP)) < tol) break
  }
  # Gib die Werte der letzten Iteration aus
  PPP
} 


# Stufe 2:
#-------------#

# Es wird die Funktion P.GK() definiert.
# Dafür wird die Matrix GK.box geschaffen.
# Sie hat die gleiche Struktur wie GEKS.box.
# GK.box hat also T-w+1 Zeilen und T Spalten.

# Setzt man w=T, ergibt sich der normale GK.Index
# und nur ein Vektor wird für GK.box berechnet.

# Die Funktion P.GK() muss zusätzlich
# zu den Argumenten von GK()
# auch die Fenstergröße angegeben bekommen.
# Die Funktion erzeugt GK.box
# und zusätzlich die Ergebnisse
# zu allen Splicing Varianten.

#_________________ Beginn der Funktion P.GK ________________________

P.GK <- function(P, Q, tol, w, fulloutput=FALSE){
  # Schaffe Objekt T (Gesamtzahl der Perioden)
  T <- dim(P)[1]
  # Schaffe Hülse für GK.box
  GK.box <- matrix(NA, T-w+1, T)
  # Greife die relevante Zeile in GK.box heraus.
  for(z in 1:(T-w+1)){
    # Definiere die Zeilen der P und Q-Matrix, 
    # welche für die Berechung zu verwenden sind.
    P.w <- P[z:(z+w-1),] # bei z=1 also Zeilen 1 bis w
    Q.w <- Q[z:(z+w-1),]
    # Berechne GK.Index und schreibe ihn
    # in GK.box in die korrekte Zeile.
    GK.box[z,z:(z+w-1)] <- GK(P.w,Q.w,tol)
  }

# GK.box hat die gleiche Struktur wie GEKS.box.
# Deshalb stehen auch die gleichen 
# Splicing Optionen wie bei GEKS zur Verfügung.

if(w==T){
  GK.All <- GK.box
}
else{ 
  GK.All <- matrix(NA,T,w) # leere Matrixhülle    
  for(r in 0:(w-2)){
    for(j in 1:w){
      GK.All[j,r+1] <- GK.box[1,j]
    }
    for(j in (w+1):T){
      GK.All[j,(r+1)] <- 
        GK.All[(j-1),(r+1)] * # Basiswert
        (GK.box[(j-w),(j-1-r)] / GK.box[(j-w),(j-1)]) * # Rückrechnung
        GK.box[(j-w+1),j] / GK.box[(j-w+1),(j-1-r)] # Vorrechnung
    }
  }
  for(i in 1:T){
    GK.All[i,w] <- prod(GK.All[i,c(1:(w-1))])^(1/(w-1))
  }

  GK.Incr <- rep(NA,T)
  if(T>=2*w){
    GK.Incr[1:w] <- GK.box[1,(1:w)]
    for(i in 1:(w-1)){
      faktoren <- rep(NA,i)
      for(j in 1:i){
        faktoren[j] <- GK.Incr[w+j-1]*
          (GK.box[i+1,w+i]/GK.box[i+1,w+j-1])  
      }
      GK.Incr[w+i] <- prod(faktoren)^(1/i) # berechnet geometrisches Mittel
    }
    for(i in (2*w):T){
      faktoren <- rep(NA,(w-1))
      for(j in 1:(w-1)){
        faktoren[j] <- GK.Incr[i-j]*
          (GK.box[i-w+1,i]/GK.box[i-w+1,i-j])  
      }
      GK.Incr[i] <- prod(faktoren)^(1/(w-1)) # berechnet geometrisches Mittel
    }
  }
GK.All <- cbind(GK.Incr, GK.All[,w], GK.All[,1:(w-1)])
colnames(GK.All) <- c("GK.MeanMove", "GK.Mean", 
                          paste("GK.", 0:(w-2), sep=""))
if(identical(GK.Incr,rep(NA,T))){
    GK.All <- GK.All[,-1]
  }
}

if(fulloutput){
  GK.results <- list(GK.All, GK.box)
  return(GK.results)
} else {
  GK.results <- list(GK.All)
  return(GK.results)
}

}

#_________________ Ende der Funktion P.GK_____________________________


# Anwendung auf Beispieldatensätze
# Resultat.GK1 <- P.GK(P.Beispiel, Q.Beispiel, tol=0.001,  w=dim(P.Beispiel)[1])
# Resultat.GK2 <- P.GK(P.Beispiel2, Q.Beispiel2, tol=0.001, w=4)
# Resultat.GK3 <- P.GK(P.Beispiel3, Q.Beispiel3, tol=0.001, w=4)[[1]]








#----------------------------------------------------------------#
#
#        Teil D: Time Product Dummy Variable Approach         ####
#
#----------------------------------------------------------------#

# Der TPD Ansatz ist eine CPD-Regression mit 
# Time (Perioden) statt mit Country (Regionen).
# Entsprechend lässt sich die Modellierung 
# der CPD-Methode heranziehen.
# Der Ansatz kann auch bei unvollständigen
# Matrizen eingesetzt werden.

# Für Beispiele 2 und 4 werden
# zusätzlich jeweils eine Gewichtungsmatrix
# für die TPD-Methode erzeugt.
# Hier soll sie für jedes Produkt den durchschnittlichen
# Ausgabenanteil innerhalb der Region/Periode
# anzeigen (bei vollständigem Datensatz).
# Dies ist auch bei unterschiedlichen Währungen
# eine sinnvolle Gewichtung.
# Bei der Einbindung der TPD()-Schätzung
# in eine Rolling-Window-Umgebung
# müssen diese Gewichte für jedes Fenster
# neu berechnet werden.

# # Zunächst die Ausgaben berechnen,
# # die sich bei vollständigen Daten
# # eingestellt haben/hätten.
# V.Beispiel3 <- P.Beispiel3*Q.Beispiel3
# # Gesamtausgaben in jeder Periode/Region (Zeile)
# V.sum.Beispiel3 <- apply(V.Beispiel3,1,sum) # 1 steht für zeilenweise Ausführung.
# # Dividiere jede Zeile aus V.Beispiel3 durch
# # das entsprechende Element dieses Vektors
# # und trage den berechneten Ausgabenanteil
# # in folgende Matrix-Hülse ein.
# V.anteil.Beispiel3 <- matrix(NA,nrow=dim(V.Beispiel3)[1],
#                              ncol=dim(V.Beispiel3)[2])
# for(i in 1:dim(V.Beispiel3)[1]){
#   V.anteil.Beispiel3[i,] <- V.Beispiel3[i,] / V.sum.Beispiel3[i]
# } 
# # Diese Gewichtungsmatrix wäre verwendbar.
# 
# # Man kann aber auch Gewichte erzeugen,
# # die für jede Region/Periode identisch sind.
# # Bilde das arithmetische Mittel jeder Spalte.
# W.vektor.Beispiel3 <- rep(NA,dim(P.Beispiel3)[2]) # Hülse
# for(i in 1:dim(P.Beispiel3)[2]){
#   W.vektor.Beispiel3[i] <- mean(V.anteil.Beispiel3[,i], na.rm=TRUE)
# }
# # Resultat ist ein Vektor. Er wird zur Matrix erweitert
# W.Beispiel3 <- matrix(W.vektor.Beispiel3,
#                       nrow=dim(P.Beispiel3)[1],
#                       ncol=dim(P.Beispiel3)[2], byrow=TRUE)
# 
# 
# # Nun noch die Gewichte für Beispiel 4.
# # Eliminiere die Elemente, die in P-Matrix NAs sind.
# V.anteil.Beispiel4 <- V.anteil.Beispiel3*NA.Beispiel4
# # Bilde aus verbliebenen Werten 
# # das arithmetische Mittel jeder Spalte.
# W.vektor.Beispiel4 <- rep(NA,dim(P.Beispiel3)[2]) # Hülse
# for(i in 1:dim(P.Beispiel3)[2]){
#   W.vektor.Beispiel4[i] <- mean(V.anteil.Beispiel4[,i], na.rm=TRUE)
# }
# # Resultat ist ein Vektor. Er wird zur Matrix erweitert
# W.Beispiel4 <- matrix(W.vektor.Beispiel4,
#                       nrow=dim(P.Beispiel3)[1],
#                       ncol=dim(P.Beispiel3)[2], byrow=TRUE)*
#   NA.Beispiel4
# # Die Gewichtung durch arithmetische Mittelwerte
# # ist aber problematisch wenn nur P.Beispiel4
# # bekannt ist, nicht aber P.Beispiel3.
# # Dann wäre eine TPD-Schätzung der 
# # fehlenden Ausgaben denkbar.
# # Das wäre aber nur bei "Missing at Random" sinnvoll.



# Vorab wird die Standard TPD-Funktion definiert.
# Die Argumente umfassen die Preismatrix
# (Regionen/Perioden in Zeilen, Produkte in Spalten)
# und die Matrix der Gewichte 
# (Standard ist eine Gleichgewichtung).
# Die Gewichtsmatrix sollte die gleiche 
# Dimension und die gleichen NAs haben
# wie die Preismatrix.
# Oftmals dürften die Gewichte in einer Spalte
# identisch sein.

TPD <- function(P, W=matrix(data=1,nrow=dim(P)[1],ncol=dim(P)[2]),
                fulloutput=FALSE){
  # Die Preismatrix darf unvollständig sein.
  # Wichtig ist, dass die entsprechenden
  # Elemente mit NA gekennzeichnet sind.
  # Angabe der W-Matrix ist nur erforderlich,
  # falls keine Gleichgewichtung gewünscht ist.
  # Regression mit lm-Befehl ist am einfachsten.
  # Dafür müssen die Preise, die Perioden/Regionen
  # und die Produkte jeweils als Vektor vorliegen.
  # Bei den Perioden/Regionen und bei den Produkten
  # muss es sich um Faktorvariablen (Levels) handeln,
  # denn solche Vektoren werden im lm()-Befehl
  # automatisch in Dummyvariablen umgewandelt.

  # Schreibe P-Matrix als Vektor.
  price.komplett <- as.vector(P,)
  # as.vector() liest standardmäßig spaltenweise ein,
  # hier also zunächst Gut 1 mit allen Perioden/Regionen,
  # dann Gut 2 usw.
  # Möchte man es umgekehrt, 
  # einfach Matrix im Befehl transponieren.
  # Analoges gilt für die Güter.
  # Der Preisvektor enthält gegebenenfalls auch die NAs.
  
  # Um einen Vektor für die Perioden/Regionen-Bezeichnungen
  # und einen Vektor für Produkte-Bezeichnungen zu erzeugen,
  # werden zunächst zwei zusätzliche Matrizen erzeugt.
  # Die erste enthält auf allen Positionen einer
  # Zeile die jeweilige Zeilennummer.
  # Die zweite enthält auf allen Positionen einer 
  # Spalte die jeweilige Spaltennummer.
  # Die beiden Matrizen werden in Vektoren verwandelt.
  # Anschließend werden noch die Positionen,
  # die im Preisvektor NAs haben, aus
  # allen drei Vektoren eliminiert.
  # Erzeuge Zeilennummern-Matrix
  Zeilen.matrix <- matrix(rep(1:dim(P)[1], dim(P)[2]), 
                          nrow=dim(P)[1], ncol= dim(P)[2]) 
  # Erzeuge Vektor aus Matrix
  period.komplett <- as.vector(Zeilen.matrix)
  # Erzeuge Spaltennummern-Matrix
  Spalten.matrix <- matrix(rep(1:dim(P)[2], dim(P)[1]),
                             nrow=dim(P)[1], ncol=dim(P)[2], byrow=TRUE)
  # Erzeuge Vektor aus Matrix
  product.komplett <- as.vector(Spalten.matrix)
  # Lösche in den drei Vektoren die Zeilen mit NAs. 
  price <- subset(price.komplett, !is.na(price.komplett))
  period <- subset(period.komplett, !is.na(price.komplett))
  product <- subset(product.komplett, !is.na(price.komplett))
  # Mache aus period und product Faktorvariablen
  period <- as.factor(period)
  product <- as.factor(product)
  # Definiere den Vektor der Gewichte (gegebenenfalls nur 1er)
  gewichte.komplett <- as.vector(W,)
  gewichte <- subset(gewichte.komplett, !is.na(price.komplett))
  # Führe Regression durch
  # und entlogarithmiere die Koeffizienten
  TPD.results.log <- lm( log(price) ~ - 1 + product + period, weights = gewichte)
  # Entlogarithmieren der Koeffizienten
  TPD.results <- exp(as.numeric(TPD.results.log$coefficients))
  # Die Anzahl der geschätzten Koeffizienten beträgt
  ncoef <- length(TPD.results)
  # Der erste Koeffizient der Regionen/Perioden ist
  firstcoef <- dim(P)[2]+1
  # Wir brauchen normalerweise nur die Resultate
  # für die Preisniveaus der Regionen/Perioden
  # und nicht der Produkte.
  # Referenz ist 0.
  if(fulloutput==FALSE){
    return(c(1, TPD.results[firstcoef : ncoef]))
  }
  else{
  return(TPD.results)
  }
}

# TPD(P.Beispiel2, W=W.Beispiel2)
# TPD(P.Beispiel2)

# Wären die Gewichte über den gesamten
# Zeitraum identisch, müsste die Funktion TPD() nicht in
# eine Rolling-Window-Umgebung eingebaut werden,
# denn die Ergebnisse bezüglich der
# Preisniveaus (nicht der Produktpreise)
# wären identisch zur umfassenden TPD-schätzung.
# Unterschiede können nur dann entstehen,
# wenn die Gewichtung für jedes Fenster neu berechnet wird.
# Dies passiert automatisch, wenn die Zeilen
# der Gewichtungsmatrix unterschiedlich sind,
# also V.anteil.Beispiel2 statt W.Beispiel2
# verwendet wird.
# Es passiert auch dann, wenn die Gewichte-Matrizen
# für jedes Fenster separat erzeugt werden
# (jede Matrix wie W.Beispiel2), also ein
# Array aus dim(P)[1]-w+1 Matrizen.
# In P.TPD() wird nur der erste Fall zugelassen.
# Die Gewichte-Matrix wird nur dann als Argument übergeben,
# wenn eine Gewichtung erwünscht ist.



#_________________ Beginn der Funktion P.TPD ________________________

P.TPD <- function(P, 
                  W = matrix(1, nrow=dim(P)[1], ncol=dim(P)[2]),
                  w, fulloutput=FALSE){

  # Schaffe Objekt T (Gesamtzahl der Perioden)
  T <- dim(P)[1]
  # Schaffe Hülse für TPD.box
  TPD.box <- matrix(NA, T-w+1, T)
  # Greife die relevante Zeile in TPD.box heraus.
  for(z in 1:(T-w+1)){
    # Definiere die Zeilen der P- und W-Matrix, 
    # welche für die Berechung zu verwenden sind.
    P.w <- P[z:(z+w-1),] # bei z=1 also Zeilen 1 bis w
    W.w <- W[z:(z+w-1),]
    # Berechne TPD.Index und schreibe ihn
    # in TPD.box in die korrekte Zeile.
    TPD.box[z,z:(z+w-1)] <- TPD(P.w, W.w)
  }
  # TPD.box hat die gleiche Struktur wie GEKS.box.
  # Deshalb stehen auch die gleichen 
  # Splicing Optionen wie bei GEKS zur Verfügung.
  
  if(w==T){
    TPD.All <- TPD.box
  }
  else{ 
    TPD.All <- matrix(NA,T,w) # leere Matrixhülle    
    for(r in 0:(w-2)){
      for(j in 1:w){
        TPD.All[j,r+1] <- TPD.box[1,j]
      }
      for(j in (w+1):T){
        TPD.All[j,(r+1)] <- 
          TPD.All[(j-1),(r+1)] * # Basiswert
          (TPD.box[(j-w),(j-1-r)] / TPD.box[(j-w),(j-1)]) * # Rückrechnung
          TPD.box[(j-w+1),j] / TPD.box[(j-w+1),(j-1-r)] # Vorrechnung
      }
    }
    for(i in 1:T){
      TPD.All[i,w] <- prod(TPD.All[i,c(1:(w-1))])^(1/(w-1))
    }
    
    TPD.Incr <- rep(NA,T)
    if(T>=2*w){
      TPD.Incr[1:w] <- TPD.box[1,(1:w)]
      for(i in 1:(w-1)){
        faktoren <- rep(NA,i)
        for(j in 1:i){
          faktoren[j] <- TPD.Incr[w+j-1]*
            (TPD.box[i+1,w+i]/TPD.box[i+1,w+j-1])  
        }
        TPD.Incr[w+i] <- prod(faktoren)^(1/i) # berechnet geometrisches Mittel
      }
      for(i in (2*w):T){
        faktoren <- rep(NA,(w-1))
        for(j in 1:(w-1)){
          faktoren[j] <- TPD.Incr[i-j]*
            (TPD.box[i-w+1,i]/TPD.box[i-w+1,i-j])  
        }
        TPD.Incr[i] <- prod(faktoren)^(1/(w-1)) # berechnet geometrisches Mittel
      }
    }
    TPD.All <- cbind(TPD.Incr, TPD.All[,w], TPD.All[,1:(w-1)])
    colnames(TPD.All) <- c("TPD.MeanMove", "TPD.Mean", 
                          paste("TPD.", 0:(w-2), sep=""))
    if(identical(TPD.Incr,rep(NA,T))){
      TPD.All <- TPD.All[,-1]
    }
  }
  
  if(fulloutput){
    TPD.results <- list(TPD.All, TPD.box)
    return(TPD.results)
  } else {
    TPD.results <- list(TPD.All)
    return(TPD.results)
  }
  
}

#_________________ Ende der Funktion P.TPD_____________________________




# Test.TPD <- as.data.frame(P.TPD(P=P.Beispiel2, V.anteil.Beispiel2, w=4))
# Test.GEKS <- as.data.frame(P.GEKS(P=P.Beispiel2, Q=Q.Beispiel2, bil=4, w=4))
# Test.GK <- as.data.frame(P.GK(P=P.Beispiel2, Q=Q.Beispiel2, tol=0.001, w=4))