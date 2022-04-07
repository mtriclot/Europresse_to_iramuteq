# Fonctions utilisées dans le traitement des HTML issus d'Europresse

# la fonction LIRE récupère le contenu des balises comme journal, auteur, date...

LIRE <- function(html) {
  
  doc <- htmlParse(html) # On parse le document
  
  articles <- getNodeSet(doc, "//article") # On récupère chaque article séparément
  
  journal <- sapply(articles, function(art) {
    journ <- xpathSApply(art, "./header/div[2]/span/text()", xmlValue)
    journ[[1]]
  })
  
  auteur <- sapply(articles, function(art) { # On récupère l'auteur de chaque article
    aut <- xpathSApply(art, "./header/div[@class='docAuthors']/text()", xmlValue)
    ifelse (length(aut)==1, aut <- aut[[1]], aut <- NA)
    if (is.null(aut)) aut <- NA
    aut
  })
  
  titre <- sapply(articles, function(art) { # On récupère le titre de chaque article
    tmp <- xpathSApply(art, "./header/div[@class='titreArticle']//text()", xmlValue)
    if (is.null(tmp)) tmp <- NA
    tmp <- paste(tmp, collapse = "")
    
    str_trim(tmp)
  })
  
  date <- sapply(articles, function(art) { # On récupère la date de chaque article
    tmp <- xpathSApply(art, ".//div[@class='publiC-lblNodoc']//text()", xmlValue)
    if (is.null(tmp)) tmp <- NA
    tmp <- substr(tmp, 6, 13)
    tmp
  })
  date <- as.Date(date, "%Y%m%d") # On met la date au bon format
  
  texte <- sapply(articles, function(art) { # Enfin, on récupère le texte de ces articles
    tmp <- xpathSApply(art, ".//div[@class='DocText clearfix']//text()", xmlValue)
    if (is.null(tmp)) tmp <- NA
    tmp <- paste(tmp, collapse = "")
    str_trim(tmp)
  })
  
  # Il ne reste plus qu'à compiler ces informations dans une base de données lisible.
  txt <- data.frame(Journal = journal,
                    Titre = titre,
                    Date = date,
                    Auteur = auteur,
                    Texte = texte)
  
  # Maintenant qu'on a toutes les infos, on enlève les lignes NA (celles qui n'ont pas de nom de journaux ou de titre) (étape qu'on peut enlever mais qui semble plus prudente)
  
  txt <- subset(txt, !is.na(Journal) & !is.na(Titre))
  
  txt
  
}

# la fonction lire_dossier applique LIRE à tout le contenu du répertoire

lire_dossier <- function(chemin) {
  
  list<-list.files(chemin, pattern= ".HTML", full.names=TRUE, recursive=FALSE)
  
  l <- lapply(list, function(file) {
    print(file)
    LIRE(html=file)
  })
  bind_rows(l)
  
}

# la fonction qui élimine les doublons

doublons <- function (corpus, seuil = 40) {
  articles <- corpus %>%  
    filter(nchar(Texte) > 500) %>% # On ne garde que les textes assez longs (cela pourrait être fait à un autre moment)
    mutate(extrait_debut = str_sub(Texte, 50, 150), # On crée une colonne avec les caractères compris entre la place 50 et 150 du texte de chaque article. On peut ici modifier le paramètre pour regarder un autre endroit du texte.
           extrait_fin = str_sub(Texte, -250, -150)) # Idem avec la fin
  
  ## EXTRAIT DE DEBUT - Calcul des paires de distance
  dist <- stringdistmatrix(articles$extrait_debut) # C'est ici qu'a lieu le calcul de distance entre tous les textes.
  ## Conversion en matrice 
  m <- as.matrix(dist)
  m[lower.tri(m)] <- 1000 # Dans la matrice, on met 1000 comme valeur pour toutes les valeurs en dessous de la diagonale, pour éviter d'avoir deux fois la même mesure
  diag(m) <- 1000 # Dans la matrice, on met 1000 comme valeur pour la diagonale pour ne pas enlever un texte parce qu'il ressemble à lui-même...
  ## Sélection des paires proches
  indices <- which(m < seuil, arr.ind = TRUE) # On regarde les positions pour lesquelles l'indice de dissimilarité est inférieure à 50. C'est ici donc qu'on fixe le seuil et qu'on peut le changer !  
  articles <- articles %>% slice(-indices[,2])
  
  ## EXTRAIT DE FIN - non pertinent, engendre des erreurs si résultat nul
  ## Calcul des paires de distance
  # dist <- stringdistmatrix(articles$extrait_fin)
  ## Conversion en matrice 
  # m <- as.matrix(dist)
  # m[lower.tri(m)] <- 1000 
  # diag(m) <- 1000
  ## Sélection des paires proches
  # indices <- which(m < seuil, arr.ind = TRUE)
  ## Vérifications
  # test2 <- cbind(articles$extrait_fin[indices[,1]], articles$extrait_fin[indices[,2]])
  # View(test2)
  ## Suppression des articles proches
  # articles <- articles %>% slice(-indices[,2]) 
  
}

