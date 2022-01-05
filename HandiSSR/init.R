
# Script d'initialisation des variables et autres fonctions utilitaires

# HandiSSR
# Novembre 2017
# @version : 20171129_0955

# Correction beug Time-Zime
# options(tz="Europe/Paris")
# options(max.print = 5000) #1000 par defaut

library(foreign) # Import de données d'autres logiciels d'analyses sastistiques
library(tidyr) # Traitement des données
suppressMessages(library(ggplot2)) # Visualisation des données
library(openxlsx) # Manipulation de fichiers excel
library(officer) # Reporting sur document word
library(flextable) # Manipulation des tables
suppressMessages(library(magrittr))
suppressMessages(library(dplyr)) # Traitement des données
suppressMessages(library(stringr))
suppressMessages(library(lubridate)) # Manipulation des dates
library(grid)
suppressMessages(library(gridExtra))
suppressMessages(library(cowplot))
# suppressMessages(library(forcats)) # Pour la manipulation aisée des variables catégorielles

#  En lieu et place de officer et flextable
# dyn.load("/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home/lib/server/libjvm.dylib")
# require(rJava)
# require(ReporteRs)

# Variables de manipulation des variables de l'environnement en utilisation
# leur nom comme paramétre 
## exists : Test l'existence d'une variable dans l'espace de travail
## get : retourne le contenu d'une variable 
## assign : affecte à une variable une valeur

#----------------------------------------------------------------------------------------------->
# Chargement des variables immuables dans le temps
#----------------------------------------------------------------------------------------------->

# Base des triplettes sélectionnées depuis le début de l'enquête
if(!exists("dbTriplette",envir = .GlobalEnv)){
  assign("dbTriplette",value = NULL,envir = .GlobalEnv)
}

if(!exists("checkerTriplette",envir = .GlobalEnv)){
  assign("checkerTriplette",value = NULL,envir = .GlobalEnv)
}

if(!exists("trashTriplette",envir = .GlobalEnv)){
  assign("trashTriplette",value = NULL,envir = .GlobalEnv)
}

rm(list=setdiff(ls(),c("dbTriplette","dbTriplette.save",
                       "checkerTriplette","checkerTriplette.save",
                       "trashTriplette","trashTriplette.save")))
save.image()

repartition.zd <- read.xlsx("../Data/Repartition ZD.xlsx")
names(repartition.zd) <- tolower(gsub("_",".",names(repartition.zd)))

smartphone <- read.xlsx("../Data/Smartphones.xlsx")
names(smartphone) <- tolower(gsub("_",".",names(smartphone)))

staff <- read.xlsx("../Data/Staff.xlsx")
names(staff) <- tolower(gsub("_",".",names(staff)))

enum.errorList <- c("Nombre de 15 ans et plus")
household.errorList <- c("Identification ménage",
                         "Statut du chef de ménage",
                         "Statut matrimonial du conjoint",
                         "Sexe des conjoints",
                         "Cohérence âge et date de naissance",
                         "Ecart âge enfant du CM et CM",
                         "Ecart âge parent du CM et CM",
                         "Statut de résidence du chef de ménage",
                         "Age du chef de ménage");

zd.finalized <- as.character(sort(unique((read.xlsx("../Data/Finalized zd.xlsx"))$zd)))

#----------------------------------------------------------------------------------------------->
# Chargement des bases de données à utiliser
#----------------------------------------------------------------------------------------------->

# Sauvegarde du chemin d'accès
path <- getwd()

## Bases de données dénombrement
setwd("../Data/Enumeration"); # Chemin d'accès aux fichiers de données
source("./CONTROLE.R")
enum.controle <- controle; rm(controle)
rowid <- str_which(enum.controle$e_hhid,"^9")
if(length(rowid)) enum.controle <- enum.controle[-rowid,]

source("./HOUSEHOLD.R")
enum.household <- household; rm(household)
rowid <- str_which(enum.household$e_hhid,"^9")
if(length(rowid)) enum.household <- enum.household[-rowid,]

source("./SUPERVISION.R")
enum.supervision <- supervision; rm(supervision)
rowid <- str_which(enum.supervision$e_hhid,"^9")
if(length(rowid)) enum.supervision <- enum.supervision[-rowid,]

setwd(path) # Reinitialisation du chemin d'accès

## Bases de données ménages
setwd("../Data/Household") # Chemin d'accès aux fichiers de données
source("./Habitat.R")
household.habitat <- habitat; rm(habitat)
rowid <- str_which(household.habitat$hhid,"^9")
if(length(rowid)) household.habitat <- household.habitat[-rowid,]

source("./Membres.R")
household.membres <- membres; rm(membres)
rowid <- str_which(household.membres$hhmid,"^9")
if(length(rowid)) household.membres <- household.membres[-rowid,]
## Reinitialisation au chemin du dossier de travail
setwd(path)

## Bases de donnéesHandicap
setwd("../Data/Handicap/")
source("./Eligibilte.R") # Module synthèse Handicap
eligibilite <- eligibilte; rm(eligibilte)
# Suppression des incohérences de la base de données pour nous faciliter la vie
rowid <- str_which(eligibilite$elig_hhmid,"^9")
if(length(rowid)) eligibilite <- eligibilite[-rowid,]
# Suppression des grilles dont l'identifiant est 999 ou 9999 ou n'a pas été affecté
rowid <- which(str_trim(eligibilite$idlab01) %in% c("B0999","B9999",""))
if(length(rowid)) eligibilite <- eligibilite[-rowid,]
rm(rowid)

# source("./Eligibilte_PH.R") # Module Eligibilite PH
# eligibilite.ph <- eligibilte_ph; rm(eligibilte_ph)
# # Suppression des incohérences de la base de données pour nous faciliter la vie
# rowid <- str_which(eligibilite.ph$elig_hhmid,"^9")
# if(length(rowid)) eligibilite.ph <- eligibilite.ph[-rowid,]
# 
# source("./Eligibilte_PT.R") # Module Eligibilite PT
# eligibilite.pt <- eligibilte_pt; rm(eligibilte_pt)
# rowid <- str_which(eligibilite.pt$elig_hhmid,"^9")
# if(length(rowid)) eligibilite.pt <- eligibilite.pt[-rowid,]

source("./handicap.R")
# On ne conserve que les éléments de eligibilite se trouvant dans handicap
handicap <- handicap %>% filter(h_hhmid %in% as.character(eligibilite$elig_hhmid))


## Reinitialisation au chemin du dossier de travail
setwd(path)

## suppression de la variable tampon
rm(path)

## Templates
template.screening <- "./template/Screening.xlsx"
template.handicap <- "./template/Handicap.xlsx"
template.fichePH <- "./template/PH.docx"
template.fichePT <- "./template/PT.docx"
logo <- "./logo-handiSSR.png"

save.image("HandiSSR.RData"); save.image()
# Chargement des fonctions de traitements des différents phases
source("Screening.R")
source("Handicap.R")

#----------------------------------------------------------------------------------------------->
# Execution des scripts de gestion des données
#----------------------------------------------------------------------------------------------->

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
##Script de gestion des donnees du volet screening
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# str_sort(unique(household.habitat$zd),numeric = TRUE)
# write.screeningList(zdList=as.character(c(496:501,512:515,519,522)),
#                     agentList=NULL)
# capture.output(grid.arrange(write.screeningWord()))
write.screeningInfos()

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## Script de gestion des donnees du volet handicap
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Suppression d'identifiants problématiques
# idlist <- c("3084753002500202")
# eligibilite <- eligibilite %>% filter(!elig_hhmid %in% idlist)
# eligibilite.ph <- eligibilite.ph %>% filter(!elig_hhmid %in% idlist)
# eligibilite.pt <- eligibilite.pt %>% filter(!elig_hhmid %in% idlist)
# handicap <- handicap %>% filter(!h_hhmid %in% idlist)

# Identification des doublons
# sum(duplicated(dbTriplette[,setdiff(names(dbTriplette),c("today","time"))]))
# sum(duplicated(dbTriplette$hhmid))
rowid <- which(duplicated(dbTriplette$hhmid))
if(length(rowid)>0) dbTriplette <- dbTriplette[-rowid,]
rm(rowid)

# Mise à jour de la base de tirage avec les informations disponibles dans la 
# base éligibilité et la base handicap
update.triplette()

# Vérification de toute la base de données éligibilité et autres le cas échéant, afin de 
# s'assurer de la cohérence des données :
checkerTriplette <- check.Triplette()

# Ajout des observations manquantes dans la base de tirage mais présentes dans les bases
# éligibilité et screening
## Selection des observations manquantes dans la base de tirage mais présentes dans la
## base éligibilité
dbase <- bind_rows(checkerTriplette[unlist(lapply(checkerTriplette,function(elt){
  elt$unknown.person & !elt$is.checked & !str_detect(elt$inconsistency.var,"#")}))])

# dbase <- bind_rows(checkerTriplette[unlist(lapply(checkerTriplette,function(elt){
#   elt$unknown.person}))])

## Selection parmis les observations manquantes dans la base de tirage et présentes dans la base
## eligibilité; celles des observations dont l'identifiant est également dans la base screening
dbase <- dbase %>% filter(hhmid %in% household.membres$hhmid)

# On ne conserve que les identifiants qui ne sont pas présent dans la
# base de Tirage
dbase <- dbase %>% filter(!(hhmid %in% dbTriplette$hhmid))

## Ajout dans la base des triplettes les identifiants non tirés mais présents dans
## les bases éligibilité et screening
if(dim(dbase)[1]){
  idlist <- as.character(dbase$hhmid)
  for(idPers in idlist) add.triplette(caseid = idPers)
  rm(dbase,idlist,idPers)
}

#unique(dbTriplette$hhmid[which(duplicated(dbTriplette$hhmid))])

# Premiére vérification des données
# check.Triplette()
checkerTriplette <- check.Triplette()

# Pour toutes les données n'ayant que des warning : pour pourvoir avancer on va les considérer
# comme toutes validées par le superviseur...
# Affectons : is.checked = TRUE
dbase <- bind_rows(checkerTriplette[unlist(lapply(checkerTriplette,function(elt){
  !elt$is.checked & elt$has.warning & !elt$is.inconsistency}))])
if(dim(dbase)[1]){
  inconsistency.setValues(idlist = as.character(dbase$hhmid),
                          values=list(is.checked=TRUE,check.date=20181115))
}


# Affectons des triplettes comme vérifié
# inconsistency.setValues(idlist=c("1031352120900102","3095950502100301"),
#                         values=list(is.checked=TRUE,check.date=20181115))
  
# Code sujet se répétant :
dbase <- eligibilite %>% filter(as.numeric(elig_commune)!=4 & 
                                !(as.numeric(statut_handicap) %in% c(3,4,NA)) &
                                !(as.numeric(statut_denombrement) %in% c(4,5)))
idlist <- unique(dbase$idlab01[duplicated(dbase$idlab01)])
idlist <- str_sort(idlist)
idlist <- idlist[str_trim(idlist)!=""]

(dbase %>% filter(idlab01 %in% idlist) %>% arrange(idlab01) %>%
  mutate(val=sprintf("%s : ID-%s",idlab01,elig_hhmid))
)$val
rm(idlist,dbase)

# Tirage de PH et PT dans les ZD finalisée qui n'ont pas d'incohérences screening identifiée
# list.zd <- setdiff(zd.finalized,c("559","558","519","556","545","515","523",
#                                   "500","496","534","501"))
list.zd <- zd.finalized
set.Triplette(zdList =list.zd); cat("\n")
set.Triplette(inclusion.PHQ129 = TRUE,zdList =list.zd); cat("\n")
rm(list.zd)

# Tirage de PT de remplacement
keylist <- c("1031352107900202","2042153402500208","1010751514300101")
 
# Tirage des triplettes pour les PT de remplacement
set.Triplette(idPHList = keylist,check=FALSE)
rm(keylist)

# 3117454804300101 - PT-1439 n'est pas apparié a un Ph, du moins pas de Ph avec le code
# B0439..

# write.handicapExcel(dbase=get.Triplette(startDate = 20181010,endDate = 20181010,
#                                         startTime =1055,endTime =1100))
write.handicapExcel()
write.handicapInfos()
# checkerTriplette.save <- checkerTriplette
# dbTriplette.save <- dbTriplette
# trashTriplette.save <- trashTriplette
save.image(sprintf("HandiSSR_%s.RData",format(Sys.time(),"%Y%m%d_%H%M"))); save.image()
