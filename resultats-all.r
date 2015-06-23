#Frédéric Schütz <schutz@mathgen.ch>
# Dépendance: packages "fields" et "RJSONIO"
# Installables avec la commande
#   install.packages("fields")
#   install.packages("RJSONIO")

source('resultats.r')

json_dir <- 'hashes'
# Créé le dossier contenant les JSON
dir.create(json_dir, showWarning=FALSE)

n <- 0

cree_parcours <- function( questions, precedent=NULL ) {
  question <- paste("q", questions[1], sep="")
  nreponses <- sum(legende[1,]==question)

  for (i in 1:nreponses) {
    # Si on a traité la dernière question
    if (length(questions)==1) {
      n <- n+1
      # Récupère les valuers sous forme de list nommé
      result <- distance( c(precedent, i ) )
      # Fichier JSON dans lequel écrire
      path <- paste(json_dir, '/', result[['hash']], '.json', sep='')
      # JSON a mettre dans le fichier
      json  <- toJSON(result)
      write(json, file=path)
      #cat(paste(precedent, sep="", collapse=""), i, "\n", sep="")
    } else {
      newquestions <- questions[-1]
      newprecedent <- c(precedent, i)
      cree_parcours( newquestions, newprecedent )
    }
  }
}

cree_parcours(questions)


