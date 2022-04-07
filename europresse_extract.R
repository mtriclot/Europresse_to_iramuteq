# traiter html téléchargés d'europresse
# source : https://rpubs.com/CorentinRoquebert/europresse

# mode d'emploi
# placer les fichiers HTML dans le même dossier que les deux scripts R
# on récupère à la fin des .csv à chaque étape de traitement des données 
# et un fichier .txt prêt pour Iramuteq


# Initialisation
rm(list=ls()) # on vide la mémoire de RStudio

load.lib <- c("xml2", "XML", "stringdist", "stringi", #chargement des packages
              "lubridate", "tidyverse","rstudioapi")
install.lib <- load.lib[!load.lib %in% installed.packages()] # On regarde les paquets qui ne sont pas installés
for (lib in install.lib) install.packages(lib,dependencies=TRUE) # On les installe
sapply(load.lib,require,character=TRUE) # Et on charge tous les paquets nécessaires

wd <- dirname (rstudioapi::getActiveDocumentContext()$path)
setwd(wd) # on spécifie le répertoire de travail

source("europresse_fonctions.R")


# ETAPE 1 
# CREATION DE LA BASE DE DONNEES

nom_projet <- "Bug_Simoncini" # On choisit un nom de projet qui servira pour les sauvegardes intermédiaires

d <- lire_dossier(wd) # on applique la fonction de parsing du HTML 
write.csv2(d, file= paste (nom_projet,"_base",".csv", sep=""), row.names = FALSE) # sauvegarde intermédiaire


# ETAPE 2
# ELIMINATION DES DOUBLONS
# En vérifiant la distance des textes entre eux (sur le début et fin d'articles)
# On définit un seuil arbitraire de proximité pour éliminer ces doublons

d <- doublons (corpus = d, seuil = 40)

write.csv2(d, file=paste (nom_projet,"_sans_doublons",".csv", sep=""), row.names = FALSE)

# ETAPE 3
# RECODER VARIABLES
# A commencer par les noms des journaux
# 2 stratégies : repérer (site web) / ", no. "
# Enlever les espaces superflus


d$CJournal <- d$Journal

index.par <- str_which(d$CJournal, "\\(si")
d$CJournal[index.par] <- stri_sub (d$CJournal[index.par], from = 1, to = str_locate(d$CJournal[index.par], "\\(si")[,1]-2)
index.par <- str_which(d$CJournal, ", no")
d$CJournal[index.par] <- stri_sub (d$CJournal[index.par], from = 1, to = str_locate(d$CJournal[index.par], ", no")[,1]-1)
index.par <- str_which(d$CJournal, " - Édition de")
d$CJournal[index.par] <- stri_sub (d$CJournal[index.par], from = 1, to = str_locate(d$CJournal[index.par], " - Édition de")[,1]-1)

d$CJournal <- str_squish(d$CJournal)

# ETAPE 4
# NETTOYAGE DES TEXTES

# On enlève tout ce qui est entre balise (des balises restent en effet souvent) : 
d$Texte<- gsub ("<.*>", "", d$Texte)
d$Titre<- gsub ("<.*>", "", d$Titre)

# On enlève les adresses mails (souvent situés en début ou en fin d'article)
d$Texte<- gsub (" [^ ]*@.[^ ]* ", " ", d$Texte)
d$Titre<- gsub (" [^ ]*@.[^ ]* ", " ", d$Titre)

# Souvent, on retrouve en début de texte une mention "mis ? jour le ..."
d$Texte<- gsub(".is à jour le .{20}[^ ]* ", "", d$Texte) # On enlève dès qu'il est question de "mis à jour le" et les 20 caractères qui suivent jusqu'au prochain espace.
d$Texte<- gsub("propos recueillis par .{20}[^ ]* ", "", d$Texte) # On enlève dès qu'il est question de "propos recueillis par" et les 20 caractères qui suivent jusqu'au prochain espace.

# D'autres idées d'expressions r?currentes dans le "méta" des articles ?
d$Texte<-gsub("\\*", "", d$Texte) # On enlève les étoiles, qui peuvent poser problème à Iramuteq (plus que les autres caractères spéciaux)

d <- select(d, Journal, CJournal, Titre, Date, Auteur, Texte) # Ne pas oublier les variables qu'on a cr?? pendant le nettoyage (dans ce tuto, CJournal et Internet par ex)
write.csv2(d ,file= paste (nom_projet,"_recod",".csv", sep=""), row.names = FALSE)

# ETAPE 5
# LE FICHIER IRAMUTEQ

d$Annee <- year(d$Date)
iram_text<- select(d, c(Annee, CJournal, Texte)) # On étudie ici le texte, et on garde comme variable l'année de publication de l'article, le journal recodé en plusieurs catégories (cf au dessus) et une variable indicatrice qui indique si l'article a été publié sur internet ou non.

# Dans Iramuteq, ce qui indique qu'on passe à un autre texte est le fait qu'une ligne commence par quatre étoiles, il faut donc que la première variable commence par quatre étoiles. 
#L'idée général va alors d'être de reformuler les variables et modalités dans les termes d'Iramuteq, comme ceci :

iram_text$Annee <- paste("**** *Annee_", iram_text$Annee, sep = "") # On colle ce qui est nécessaire à Iramuteq (les étoiles) avec l'année qu'on avait de base dans notre variable
# Pour la deuxième variable et les suivantes, il n'y a plus besoin de rajouter les quatre étoiles au début, mais juste d'une pour introduire la variable :
iram_text$CJournal <- paste("*journal_", iram_text$CJournal, sep = "")


# Facultatif
# iram_text$Texte <- gsub( "[^[:alnum:],]", " ", iram_text$Texte)
# iram_text$Texte <- tolower(iram_text$Texte)

iram_text$Texte<- paste("\n",iram_text$Texte, "\n", sep=" ")

write.table(iram_text, file= paste (nom_projet,"_toIramuteq",".txt", sep=""), sep=" ", quote=FALSE, row.names=FALSE, col.names=FALSE)

