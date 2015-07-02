# analyse_data.R
# Frédéric Schütz (schutz@mathgen.ch)
#
# Prend les résultats bruts du Parlomètre (fichier CSV avec les réponses)
# et en sort les tables de résultats, ainsi que les cartes pour chaque
# réponse

# Données sur les communes suisses
# Downloadé de
# http://data.geo.admin.ch/ch.swisstopo-vd.ortschaftenverzeichnis_plz/PLZO_CSV_LV03.zip
# et modifié pour lecture dans R
# Plus d'infos sur les données:
#http://www.cadastre.ch/internet/cadastre/fr/home/products/plz/data.html

datacommunes <- read.table("contrib/PLZO_CSV_LV03.csv", sep="\t", header=TRUE,
                           quote="", as.is=TRUE)
# On ne prend que les cantons qui nous intéressent (romandie)
romandie <- c("GE", "VD", "FR", "VS", "NE", "JU", "BE")
# ... et uniquement les colonnes "nom de commune" (4), "canton" (5)
# et les coordonnées E (6) et N (7)
datacommunes <- datacommunes[ datacommunes[,5] %in% romandie,
                              c("Gemeindename", "Kantonskürzel", "E", "N") ]

# Les coordonnées sont données au mètre près, ce qui est bien trop précis
# pour notre utilisation
datacommunes[,3] <- round(datacommunes[,3]/1000, 0)
datacommunes[,4] <- round(datacommunes[,4]/1000, 0)

# Les limites de carte spécifiées explicitement (en fonction des données
# cartographiques et du masque de carte)
minxrange <- 484
maxxrange <- 680
minyrange <- 72
maxyrange <- 266

minx <- minxrange
miny <- minyrange
maxx <- maxxrange
maxy <- maxyrange

widthx <- maxx-minx
widthy <- maxy-miny

x <- seq(from=minx, by=1, length.out=widthx+1)
y <- seq(from=miny, by=1, length.out=widthy+1)

library(fields)

# Couleurs
colmin <- "#0080ff"
colmid <- "#fff2cc"
colmax <- "#ff4d4d"
colours <- colorRampPalette(c(colmin, colmid, colmax))

parlodataread <- read.table("data/sondage-processed.csv", sep="\t",
                            fill=TRUE, quote="", header=TRUE,
                            comment.char="", as.is=TRUE)

legendeorig  <- scan("data/legende.csv", what="character")

# Supprime les colonnes dont on a pas besoin
parlodataorig <- parlodataread[, -c(3:8, 184:186)]
legende <- legendeorig[-c(3:8)]

answersbycanton <- t(as.matrix(table(parlodataorig[,2])))

# On ne prend que les cantons en Suisse romande
parlodata <- parlodataorig[ parlodataorig[,2] %in% romandie,]

# Supprime quelques communes Suisses allemandes (mais dans des cantons
# au moins partiellement romands) qui ont été indiquées par les
# participants
parlodata <- parlodata[parlodata[,1] != "Täsch", ]
parlodata <- parlodata[parlodata[,1] != "Visp", ]
parlodata <- parlodata[parlodata[,1] != "Frutigen", ]
parlodata <- parlodata[parlodata[,1] != "Bern", ]
parlodata <- parlodata[parlodata[,1] != "Köniz", ]
parlodata <- parlodata[parlodata[,1] != "Eriz", ]
parlodata <- parlodata[parlodata[,1] != "Lax", ]
parlodata <- parlodata[parlodata[,1] != "Oberhofen am Thunersee",]
parlodata <- parlodata[parlodata[,1] != "Ostermundigen",]
parlodata <- parlodata[parlodata[,1] != "Steffisburg",]
parlodata <- parlodata[parlodata[,1] != "Zollikofen",]

parlodata[,2] <- factor(parlodata[,2])

pvalues <- NULL

cantons <- parlodata[,2]
communes <- parlodata[,1]

# On regarde chaque question l'une après l'autre (les 5 premières colonnes
# contiennent l'origine, âge, etc du répondant)
for (i in 6:42) {

  question <- paste("q", i, sep="")
  
  # Les données qui concernent cette question
  dataquestion <- parlodata[, legende==question]
  
  # S'il n'y a qu'une colonne, tous les résultats sont là
  if (is.null( dim(dataquestion))) {
    # Nombre de réponses par option pour cette question
    # questiontable <- t(as.matrix(table( dataquestion)))))
    # Réponses par canton  
    table_canton <- t(as.matrix(table( cantons, dataquestion)))
  } else {
    # Sinon, les réponses sont réparties sur plusieurs colonnes
    table_canton <- matrix( NA, nrow=ncol(dataquestion), ncol=length(table(cantons)))
    colnames(table_canton) <- names(table(cantons))
    rownames(table_canton) <- colnames(dataquestion)
          
    for (j in 1:nrow(table_canton)) {
      table_canton[j,] <- t(table( cantons, dataquestion[,j]))[2,]
    }
  }
  
  # Total de réponses par canton
  total_canton <- apply( table_canton, MAR=2, FUN=sum)
  
  # Efface les lignes vides (pas de réponse), qui nous sont inutiles
  table_canton <- table_canton[ rownames(table_canton) != "",]
  
  # Un test de chi^2 est utilisé pour avoir une estimation du pouvoir
  # discriminant de chaque question
  # On a besoin que d'une estimation grossière; on réduit donc les
  # données d'abord pour éviter les p-values trop faibles.
  pvalues <- c(pvalues, chisq.test(table_canton/100)$p.value )
  
  #
  #  Pour chaque réponse, créé les cartes par commune
  #

  # S'il n'y a qu'une colonne, tous les résultats sont là
  if (is.null( dim(dataquestion))) {
    # Réponses par commune (note that we do not transpose, because we
    # are interested in the questions, not the communes)
    table_communes <- as.matrix(table(communes, dataquestion))
  } else {
    table_communes <- matrix( NA, ncol=ncol(dataquestion), nrow=length(table(communes)))
    rownames(table_communes) <- names(table(communes))
    colnames(table_communes) <- colnames(dataquestion)
    
    for (j in 1:ncol(table_communes)) {
      table_communes[,j] <- table( communes, dataquestion[,j])[,2]
    }
  }
  
  # Total des réponses par commune
  somme_par_commune <- apply( table_communes, FUN=sum, MAR=1)
    
  # Enlève les communes sans réponses (pas nécessaire au vu de la commande
  # suivante)
  #table_communes <- table_communes[ somme_par_commune > 0, ]

  # Enlève les communes atypiques qui n'ont reçu qu'un nombre limité de
  # réponses (1)
  table_communes <- table_communes[ somme_par_commune > 1, ]
  
  # Recalcule les sommes sans les communes atypiques
  somme_par_commune <- apply( table_communes, FUN=sum, MAR=1)
  
  table_communes_pourcent <- table_communes / somme_par_commune
    
  # Identifie les communes et leurs coordonnées
  coordinates <- datacommunes[ match( rownames(table_communes_pourcent), datacommunes[,1]) , c(1,3:4) ]
    
  # Traite chaque réponse séparément
  for (j in 1:ncol(table_communes_pourcent)) {
    thisquestion <- table_communes_pourcent[,j]
    mapdata <- matrix(NA, ncol=widthx, nrow=widthy)        
  
    for (k in 1:nrow( table_communes_pourcent )) {
       mapdata[ coordinates[k,3]-miny, coordinates[k,2]-minx] <- thisquestion[k]
    }
      
    mapdata <- image.smooth(mapdata)$z
    
    # Renormalise les pourcentages de réponse à 1
    mapdata <- mapdata / max(mapdata, na.rm=TRUE)

    # Créé un PNG avec les couleurs en fonction des réponses; il reste ensuite à
    # appliquer un masque de la Suisse
    png(paste("images/Question", i, "-reponse-", j, "-", colnames(table_communes)[j],".png", sep=""), width=500, height=500)
    par(mar=c(0,0,0,0))
  
    image(x,y, t(mapdata), zlim=c(0,1), col=colours(16), asp=1, axes=FALSE,
          xlim=c(minxrange, maxxrange), ylim=c(minyrange, maxyrange))

    dev.off()
  }
}

# Estimation de poids pour chaque question, en utilisant les
# résultats (p-values) déjà obtenues auparavant
questionpvalues <- paste("q", 6:42, sep="")[ order(pvalues) ]
cbind( questionpvalues, pvalues[order(pvalues)])

# Petite p-value ==> grand poids, on inverse donc les résultats
poids <- 1/pvalues[order(pvalues)]
names(poids) <- questionpvalues

poids <- log(poids)
poids[ poids > 5] <- 5
poids[ poids < 1] <- 1
poids <- round(poids)

questions <- c(6,31,39,18,25,29,30,38,35,16,7,13,21,27,24,20,22,10,28,33,34,37,23,32,12)

poids <- poids[ paste("q", questions, sep="") ]
