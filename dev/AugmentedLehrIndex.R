##############################################################
# Bilaterale Preisindizes für Daten aus mehr als zwei Perioden
##############################################################

# Es geht hier um pseudo-multilaterale Preisindizes 
# wie den augmented Lehr.
# Die Indizes sind für interregionale oder auch
# intertemporale Vergleiche einsetzbar.
# Sie berücksichtigen auch die verschiedenen 
# Splicing Varianten (nur bei intertemporal sinnvoll).

# Datengrundlage sind immer eine Preismatrix P
# und eine Mengenmatrix Q, wobei
# die Regionen/Perioden zeilenweise angeordnet sind
# und die Güter Spaltenweise

# Zur Veranschaulichung wird ein Datenstz aus 
# Maddison and Rao (1996, S. 16) verwendet.

region <- c("USA", "India", "Brazil")
product <- c("Wheat", "Potatoes", "Milk", "Lamb")
# Es wird ein dataframe aus allen Kombinationen
# der Faktoren geschaffen.
# Jede "Zeile" des Dataframes ist eine eigene Kombination.
data <- expand.grid(Region = region, Commodity = product)
price <- c(1.42, 22, 4, 1.1, 17, 3.2, 2.85, 40, 8, 24, 380, 80)
quantity <- c(30, 20, 26, 50, 26, 42, 120, 43, 80, 140, 30, 45)
# Der Dataframe wird um die Preise und Mengen erweitert.
data <- data.frame(data, Price = price, Quantity = quantity)
# Die Daten werden in eine Preis- und eine Mengenmatrix
# (jeweils Dataframes) umgewandelt.
# Dabei Regionen in die Zeilen und Güter in Spalten.
# Dazu wird die Spalte "Commodity" als erstes Element
# einer Liste definiert und die Spalte "Region" als zweites.
# Die Funktion tapply berechnet für jede
# Commodity-Region-Kombination (Faktorkombination)
# den Mittelwert.
# Das Ergebnis wird als Matrix ausgegeben, da Faktorkombinationen
# betrachtet wurden.
P.Beispiel <- tapply(data$Price, list(data$Commodity, data$Region), mean)
Q.Beispiel <- tapply(data$Quantity, list(data$Commodity, data$Region), mean)
# Dies war ein einfacher Weg, um eine Preismatrix zu erzeugen.
# Transponiere Ergebnisse, um Regionen in den Zeilen zu haben.
P.Beispiel <- t(P.Beispiel)
Q.Beispiel <- t(Q.Beispiel)

# Erweiterte Datensätze
P.Beispiel2 <- rbind(P.Beispiel, c(8.22, 6.62, 16.01, 147),
                     c(3.44, 2.01, 5.33, 50),
                     c(10.22, 5.62, 13.01, 155))
P.Beispiel3 <- rbind(P.Beispiel2, c(11.22, 7.62, 15.01, 136),
                     c(3.74, 1.81, 6.13, 46))
Q.Beispiel2 <- rbind(Q.Beispiel, c(20,40,110,120),
                     c(13,24,36,21),
                     c(17,33,113,100))
Q.Beispiel3 <- rbind(Q.Beispiel2, c(15,45,100,130),
                     c(15,21,32,24))

rownames(P.Beispiel2) <- c("USA", "India", "Brazil", 
                           "Estonia", "Latvia", "Poland")
rownames(P.Beispiel3) <- c("USA", "India", "Brazil", 
                           "Estonia", "Latvia", "Poland",
                           "Romania", "Bulgaria")
# P<-P.Beispiel2
# Q<-Q.Beispiel2


###########################################
# Augmented Lehr 
###########################################

# Es sind mehr als zwei Perioden involviert.
# P ist die Matrix der Preise (T Zeilen).
# Q ist die Matrix der Mengen

# Bei diesem Index besteht das Problem,
# dass bei einem Fenster der Länge w
# der Index erst ab Periode w
# in vollem Umfang berechnet werden könnte.
# Wenn das Preisniveau der ersten Periode
# auf 1 normiert wird, müsste für die
# Berechnung des Index der Periode 2 
# w-2 Vorlaufperioden existieren,
# damit auf einen vollständigen 
# Datensatz zugegriffen werden kann.
# Die Daten der Vorlaufperioden müssen als P.vor und Q.vor 
# der Funktion als Argument eingegeben werden.
# Die Fensterlänge ist implizit über die
# Dimension von P.vor gegeben.

# Beispieldaten 6 Perioden (Zeilen), 2 Güter (Spalten)
# P <- data.frame(c(3,2,4,2,3,3),c(5,5,8,6,7,4))
# Q <- data.frame(c(2,2,2,3,4,2),c(4,3,2,3,4,2))
# P.vor <- data.frame(c(3,2),c(5,8))
# Q.vor <- data.frame(c(2,2),c(4,2))

P.aLe <- function(P, Q, P.vor, Q.vor){
  # Zeitdimension und Güterdimension
  P <- as.matrix(P)
  Q <- as.matrix(Q)
  T <- dim(P)[1]
  N <- dim(Q)[2]
  # Hülse für Indexzahlen
  P.aLehr.index <- c(1,rep(NA,(T-1)))
  # Länge des Fensters beträgt
  w <- dim(P.vor)[1]+2
  # Bilde ein Dataframe für alle Preise.
  # Dafür müssen zunächst die Spaltennamen
  # vereinheitlich werden.
#  names(P.vor) <- names(P)
#  names(Q.vor) <- names(Q)
  PX <- rbind(P.vor, P)
  # Ebenso die Mengen
  QX <- rbind(Q.vor, Q)
  # Matrix der Ausgaben
  VX <- PX*QX
  # Die erste relevante Periode ist in Zeile w+1.
  # Der Index wird für jedes Fenster ab Periode w+1 berechnet.
  for(j in w:(T+w-2)){
  # Transformationsfaktoren z berechnen
  # Hülse für Transformationsfaktoren
  z <- rep(NA,N)
  # Hülse für Gesamtmengen der jeweiligen
  # Güter (innerhalb des Fensters)
  q <- rep(NA,N)
  # Hülse für Gesamtausgaben
  v <- rep(NA,N)
  for(i in 1:N){        # greift einzelnes Gut heraus
    v[i] <- sum(VX[(j-w+1):j,i])  # addiert alle Ausgaben des Gutes
    q[i] <- sum(QX[(j-w+1):j,i])  # addiert alle Mengen des Gutes
    z[i] <- v[i]/q[i]    # berechnet Transformationsfaktor (unit value)
    }
  # Berechne Indexwert
  P.aLehr.index[j-w+2] <- P.aLehr.index[j-w+1] * 
    (sum(VX[j,])/sum(VX[j-1,])) *
    (sum(z*QX[j-1,])/sum(z*QX[j,])) 
  }
  return(P.aLehr.index)
} 
# Funktionsende
  

