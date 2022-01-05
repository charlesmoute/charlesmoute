
# Verification des données saisies
# 2018-12-14

library(tidyverse)

elig.valid <- read.csv("../Data/Handicap/apure/data_handicap.csv")
id.operator <- read.csv("../Data/GridDataChecking_20190105.csv")
id.treated <- unique(eligibilite$idlab01)

path <- "../../../[01] Documents/Scans des grilles"
# "../../../[01] Documents/Scans des grilles/PT/B163"
# id.supervisor <- c(head(list.files(sprintf("%s/PH",path)),-1),
#                    head(list.files(sprintf("%s/PT",path)),-1))

id.supervisor <- c(list.files(sprintf("%s/PH",path)),list.files(sprintf("%s/PT",path)))
id.supervisor <- str_replace_all(id.supervisor,".pdf","")
rm(path)


# Identification de potentiels doublons dans la saisie d'identifiant
idlist <- unique(id.operator$codeSujet[which(duplicated(id.operator$codeSujet))])
if(length(idlist)){
  dbtmp <- id.operator %>% filter(codeSujet %in% idlist) %>%
    group_by(codeSujet) %>%
    summarise(Operator= paste(Operateur,collapse = ", "),
              err.doublon = "Oui",
              observation00 =sprintf("Grille %s saisie plusieurs fois par: %s",unique(codeSujet),
                                     Operator)) %>%
    ungroup() %>% select(codeSujet,Operateur=Operator,err.doublon,observation00)
  
  db <- id.operator %>% filter(!codeSujet %in% idlist) %>%
    mutate(err.doublon="Non",observation00="") %>% 
    select(codeSujet,Operateur,err.doublon,observation00)
  
  id.operator <- rbind(db,dbtmp) %>% arrange(codeSujet)
}else{
  id.operator <- id.operator %>%
    mutate(err.doublon="Non",observation00=NA_character_) %>%
    select(codeSujet,Operateur,err.doublon,observation00)
}
db <- id.operator

# Liste des identifiants dont le fichier est disponible mais dont l'infos
# n'as pas encore été saisie
#aSaisir
idlist <-  unique(id.supervisor[which(!id.supervisor %in% id.operator$codeSujet)])
if(length(idlist)){
  dbtmp <- data.frame(codeSujet=idlist,
                      aSaisir = "Oui",
                      observation01="Fichier physique disponible mais grille non saisie")
  db <- merge(id.operator,dbtmp,by="codeSujet",all = TRUE)
  db %>% filter(codeSujet %in% idlist)
  
  db$aSaisir <- as.character(db$aSaisir)
  rowid <- which(is.na(db$aSaisir) | db$aSaisir=="")
  if(length(rowid)) db$aSaisir[rowid] <- "Non"
  
  db$Operateur <- as.character(db$Operateur)
  rowid <- which(is.na(db$Operateur))
  if(length(rowid)) db$Operateur[rowid] <- "---"
  db %>% filter(codeSujet %in% idlist)
}else{
  db$aSaisir <- "Non"
  db$observation01 <- NA_character_
}


# Liste des identifiants saisis figurant dans la base de données mais donc 
# les fichiers scanés ne sont pas disponibles sur le serveur
idlist <- id.treated[which(!id.treated %in% id.supervisor)]
#err.superviseur
if(length(idlist)){
  db %>% filter(codeSujet %in% idlist)
  dbtmp <- data.frame(codeSujet=idlist,
                      err.superviseur="Oui",
                      observation02="Donnees collectees mais fichier physique indisponible sur le serveur")
  db <- merge(db,dbtmp,by="codeSujet",all = TRUE)
  
  db$err.superviseur <- as.character(db$err.superviseur)
  rowid <- which(is.na(db$err.superviseur) | db$err.superviseur=="")
  if(length(rowid)) db$err.superviseur[rowid] <- "Non"
  
  db$Operateur <- as.character(db$Operateur)
  rowid <- which(is.na(db$Operateur))
  if(length(rowid)) db$Operateur[rowid] <- "---"
}else{
  db$err.superviseur <- "Non"
  db$observation02 <- NA_character_
}


# Lequel des identifiants saisis n'est pas dans la base de données
#err.operateur
idlist <- id.operator$codeSujet[which(!id.operator$codeSujet %in% id.treated)]
if(length(idlist)){
  db %>% filter(codeSujet %in% idlist)
  dbtmp <- data.frame(codeSujet=idlist,
                      err.operateur="Oui",
                      observation03="Grille saisie mais code sujet non present dans la base de donnees")
  db <- merge(db,dbtmp,by="codeSujet",all = TRUE)
  
  db$err.operateur <- as.character(db$err.operateur)
  rowid <- which(is.na(db$err.operateur) | db$err.operateur=="")
  if(length(rowid)) db$err.operateur[rowid] <- "Non"
  
  db$Operateur <- as.character(db$Operateur)
  rowid <- which(is.na(db$Operateur))
  if(length(rowid)) db$Operateur[rowid] <- "---"
}else{
  db$err.operateur <- "Non"
  db$observation03 <- NA_character_
}


# Lequel des identifiants saisis n'a pas de fichier physique disponible
idlist <- id.operator$codeSujet[which(!id.operator$codeSujet %in% id.supervisor)]
#err.operateur
if(length(idlist)){
  db %>% filter(codeSujet %in% idlist)
  dbtmp <- data.frame(codeSujet=idlist,
                      observation04="Grille saisie mais fichier physique indisponible sur le serveur")
  db <- merge(db,dbtmp,by="codeSujet",all = TRUE)
  
  db$err.operateur <- as.character(db$err.operateur)
  db$err.operateur[which(db$codeSujet %in% idlist)] <- "Oui"
  
  db$Operateur <- as.character(db$Operateur)
  rowid <- which(is.na(db$Operateur))
  if(length(rowid)) db$Operateur[rowid] <- "---"
}else{
  db$observation04 <- NA_character_
}

# Si la grille est saisie car le fichier physique est disponible mais que
# la donnée ne figure pas dans la base de données alors une incoherence
# que le superviseur devrait vérifier
idlist <- db$codeSujet[which(db$err.operateur=="Oui" & is.na(db$observation04))]
if(length(idlist)){
  rowid <- which(db$codeSujet %in% idlist)
  db$err.superviseur[rowid] <- "Oui"
}

# Les identifiants qui sont corrects : saisies, fichier disponible sur le serveur et 
# données enregistré sur la tablette [Correcte]
# idlist <- id.operator$codeSujet[which(id.operator$codeSujet %in% id.supervisor &
#                                            id.operator$codeSujet %in% id.treated)]
# idlist <- !idlist %in% db$codeSujet[with(db,which(
#   (!is.na(observation00) & observation00!="") | # Doublon
#   (!is.na(observation01) & observation01!="") | # Grille non saisie
#   (!is.na(observation02) & observation02!="") | # Données collectées mais fichier physique indisponible
#   (!is.na(observation03) & observation03!="") | # Données non collectées
#   (!is.na(observation04) & observation04!="") # Grille saisie mais fichier physique indisponible 
# ))]

# if(length(idlist)){
#   db %>% filter(codeSujet %in% idlist)
#   dbtmp <- data.frame(codeSujet=idlist,
#                       observation05="Saisie correcte : fichier physique disponible & donnees collectees")
#   db <- merge(db,dbtmp,by="codeSujet",all = TRUE)
#   db$Operateur <- as.character(db$Operateur)
#   rowid <- which(is.na(db$Operateur))
#   if(length(rowid)) db$Operateur[rowid] <- "---"
# }else{
#   db$observation05 <- NA_character_
# }

# Identifiant present dans la base des données d'éligibilité considérées comme valides
rowid <- which(!(id.operator$codeSujet %in% elig.valid$idlab01))
if(length(rowid)){
  idlist <- id.operator$codeSujet[rowid]
  dbtmp <- data.frame(codeSujet=idlist,
                      observation06="Grille saisie mais refus de participation, individu ineligible ou indisponible")
  db <- merge(db,dbtmp,by="codeSujet",all = TRUE)
  
  db$err.superviseur <- as.character(db$err.superviseur)
  db$err.superviseur[which(db$codeSujet %in% idlist)] <- "Oui"
  
  db$Operateur <- as.character(db$Operateur)
  rowid <- which(is.na(db$Operateur))
  if(length(rowid)) db$Operateur[rowid] <- "---"
}else{
  db$observation06 <- NA_character_
}

db$incoherence <- "Oui"
idlist <- db$codeSujet[with(db,which(
  (err.doublon=="Non") & (err.superviseur=="Non") & (err.operateur=="Non") #& (aSaisir=="Non")
))]
db$incoherence[which(db$codeSujet %in% idlist)] <- "Non"

# Fusionne pour chacun des observatio01-06 en une observation
# # Suppression des NA et conservation
# for(elt in sprintf("observation0%d",0:6)){
#   rowid <- which(is.na(db[,elt]))
#   if(length(rowid)) db[rowid,elt] <- ""
# }
# rm(elt,rowid)
db <- db %>% group_by(codeSujet,Operateur,err.doublon,aSaisir,
                       err.superviseur,err.operateur,incoherence) %>%
  summarise(observation = paste(observation00,observation01,
                                observation02,observation03,
                                observation04,observation06,
                                collapse = ","),
            observation=str_trim(str_replace_all(observation,"NA,?",""))) %>%
  ungroup()


# Le reste état indéterminée pour l'instant
# str_trim(db)
# rowid <- with(db,which(err.doublon=="Non" & observation==""))
# db[rowid,]
# if(length(rowid)) db$observation[rowid] <- "Etat saisie indéterminée"

#Export checkData
write.csv(db,file = sprintf("checkGrids_%s.csv",format(Sys.time(),"%Y%m%d")))

rm(db,dbtmp,rowid,idlist,id.operator,id.supervisor,id.treated,elig.valid)
