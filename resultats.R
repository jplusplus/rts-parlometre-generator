#Frédéric Schütz <schutz@mathgen.ch>
# Dépendance: package "fields"
# Installable avec la commande
#   install.packages("fields")
#   install.packages("RJSONIO")

json_dir <- 'hashes'
# Pour spécifier les questions d'intérêt: p.ex. les 4 premières
questions <- c(6,31,39,18)

library(fields)
library(RJSONIO)

# Créait le dossier contenant les JSON
dir.create(json_dir, showWarning=F)

# Charge les données résumées des communes, des cantons, ainsi que
# la légende des tableaux
resultats_cantons  <- read.table("resultats-cantons.csv", as.is=TRUE, sep="\t", quote="")
resultats_communes <- read.table("resultats-communes.csv", as.is=TRUE, sep="\t", quote="")
legende            <- read.table("resultats-legende.csv", as.is=TRUE, sep="\t", quote="")

# Extrait les informations sur les communes (pour la carte)
info_communes <- cbind(resultats_communes[,1:3], id=1:nrow(resultats_communes))
resultats_communes <- resultats_communes[,-(2:3)]

# Calcule les proportions de réponses
resultats_cantons  <- resultats_cantons  / resultats_cantons[,1]
resultats_communes <- resultats_communes / resultats_communes[,1]

# Extrait les colonnes qui contiennent nos réponses d'intérêt
qcolumns <- NULL
qquestions <- paste("q", questions, sep="")
for (i in qquestions) {
  qcolumns <- c(qcolumns, which( legende[1,]==i))
}

resultats_cantons  <- resultats_cantons[,qcolumns]
resultats_communes <- resultats_communes[,qcolumns]
legende            <- legende[,qcolumns]

# Créé un index des réponses par question
index_reponses <- 1
for (i in 2:length( legende[1,])) {
  if (legende[1,i]==legende[1,i-1])
    index_reponses <- c( index_reponses, tail(index_reponses, n=1)+1)
  else
    index_reponses <- c( index_reponses, 1 )
}
legende[3,] <- paste( legende[1,], index_reponses, sep="-")

# Calcule les scores minimums et maximums par canton
canton_min <- NULL
canton_max <- NULL

for (q in qquestions) {
  this_question <- resultats_cantons[, which( legende[1,]==q)]

  the_min <- this_question
  the_max <- this_question

  for (canton in 1:nrow(this_question)) {
    q_min <- which.min( this_question[canton,] )[1]
    q_max <- which.max( this_question[canton,] )[1]
    the_min[ canton, ] <- 0; the_min[ canton, q_min] <- 1
    the_max[ canton, ] <- 0; the_max[ canton, q_max] <- 1
  }

  if (is.null(canton_min)) {
    canton_min <- the_min
    canton_max <- the_max
  } else {
    canton_min <- cbind(canton_min, the_min)
    canton_max <- cbind(canton_max, the_max)
  }
}

#distf <- function(x) { x ^ 2}
distf <- function(x) { abs(x) }

cantons_min_scores <- sqrt( apply( distf(resultats_cantons - canton_min), FUN=sum, MAR=1 ) )
cantons_max_scores <- sqrt( apply( distf(resultats_cantons - canton_max), FUN=sum, MAR=1 ) )

# Calcule les scores minimums et maximums par commune
commune_min <- NULL
commune_max <- NULL

for (q in qquestions) {
  this_question <- resultats_communes[, which( legende[1,]==q)]

  the_min <- this_question
  the_max <- this_question

  for (commune in 1:nrow(this_question)) {
    q_min <- which.min( this_question[commune,] )[1]
    q_max <- which.max( this_question[commune,] )[1]
    the_min[ commune, ] <- 0; the_min[ commune, q_min] <- 1
    the_max[ commune, ] <- 0; the_max[ commune, q_max] <- 1
  }

  if (is.null(commune_min)) {
    commune_min <- the_min
    commune_max <- the_max
  } else {
    commune_min <- cbind(commune_min, the_min)
    commune_max <- cbind(commune_max, the_max)
  }
}

communes_min_scores <- sqrt( apply( distf(resultats_communes - commune_min), FUN=sum, MAR=1 ) )
communes_max_scores <- sqrt( apply( distf(resultats_communes - commune_max), FUN=sum, MAR=1 ) )

# Coordonnées pour générer les cartes
minxrange <- 484
maxxrange <- 680
minyrange <- 74
maxyrange <- 266

minx <- minxrange
miny <- minyrange
maxx <- maxxrange
maxy <- maxyrange

widthx <- maxx-minx
widthy <- maxy-miny

x <- seq(from=minx, by=1, length.out=widthx+1)
y <- seq(from=miny, by=1, length.out=widthy+1)

# Setting colours
#colmin <- "#0080ff"
#colmid <- "#fff2cc"
#colmax <- "#ff4d4d"
#colours <- colorRampPalette(c(colmin, colmid, colmax))

distance <- function( reponses ) {
  if (length(reponses) != length(questions))
    stop("Le nombre de réponses n'est pas le bon.")

  # Index des réponses dans le vecteur
  q_thisrep <- paste( qquestions, reponses, sep="-")

  vecteur <- rep(0, ncol(legende))
  # Marque les réponses de cette personne
  vecteur[ match( q_thisrep, legende[3,]) ] <- 1

  # Calcule le score pour chaque canton, et utilise les scores maximum et minimum
  # possibles pour chaque canton pour obtenir un pourcentage
  canton_score <- apply( resultats_cantons, MAR=1, FUN=function(x) { sqrt( sum( distf(vecteur-x) ) ) } )
  canton_score <- (canton_score-cantons_max_scores)/(cantons_min_scores-cantons_max_scores)
  canton_score <- 1-canton_score

  commune_score <- apply( resultats_communes, MAR=1, FUN=function(x) { sqrt( sum( distf(vecteur-x) ) ) } )
  commune_score <- (commune_score-communes_max_scores)/(communes_min_scores-communes_max_scores)
  commune_score <- 1-commune_score

  mapdata <- matrix(NA, ncol=widthx, nrow=widthy)

  for (commune in 1:length(commune_score)) {
    mapdata[ info_communes[commune,3]-miny, info_communes[commune,2]-minx ] <- commune_score[commune]
  }
  mapdata <- image.smooth(mapdata)$z
  mapdata <- mapdata / max(mapdata, na.rm=TRUE)

  #image(x, y, t(mapdata), zlim=c(0,1), col=colours(16), asp=1, axes=FALSE,
  #      xlim=c(minxrange, maxxrange), ylim=c(minyrange, maxyrange))

  hash <- paste(reponses, sep="", collapse="")
  cantons <- round(canton_score,2)

  # Les communes les plus proches, mais uniquement avec 10 personnes
  best_communes <- info_communes[order( commune_score),]
  best_communes <- best_communes[best_communes[,1]>10,4]
  communes <- rev(best_communes)[1:3]

  mapdata2 <- c(0:9, LETTERS[1:6])[1+round(15*as.vector(t(mapdata)), 2)]
  mapdata2[is.na(mapdata2)] <- "."
  image <- paste(mapdata2, collapse="")

  result <- list(hash=hash, cantons=cantons, communes=communes, image=image)
  return(result)
}

#reponses <- c(3, 5, 3, 1)  # Réponse typiquement genevoise
#distance(reponses)


# n <- 0
# for (i in 1:sum(legende[1,]==qquestions[1])) {
#   for (j in 1:sum(legende[1,]==qquestions[2])) {
#     for (k in 1:sum(legende[1,]==qquestions[3])) {
#       for (l in 1:sum(legende[1,]==qquestions[4])) {
#         n <- n+1
#         # Récupère les valuers sous forme de list nommé
#         result <- distance( c(i,j,k,l) )
#         # Fichier JSON dans lequel écrire
#         path <- paste(json_dir, '/', result[['hash']], '.json', sep='')
#         # JSON a mettre dans le fichier
#         json  <- toJSON(result)
#         write(json, file=path)
#         stop()
#       }
#     }
#   }
# }