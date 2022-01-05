# Script d'initialisation des variables et autres fonctions utilitaires

# PFS
# Août 2021
# @version : 210826-1635
# @author  : Charles M. <charlesmoute@gmail.com>


#...........................................................................................................
# Chargement des librairies utilitaires
#...........................................................................................................
library(tidyverse)

library(purrr) # Pour l'amelioration des temps de calcul en evitant les boucles classiques
library(ggplot2) #Pour la production des graphiques
library(patchwork) # Pour la fusion/mise en page des graphiques produits avec ggplot2
library(dplyr) # Traitement des données
library(stringr) #Pour la manipulation aisee des chaines de caracteres
library(lubridate) #Pour la manipulation aisee des dates
library(forcats) # Pour la manipulation aisee des variables catégorielles
library(hrbrthemes) 


library(flextable) # Pour la manipulation des tableaux..
library(officer) # Pour l'automatisation de la redaction des rapports sous Word
library(magrittr)
# library(gtsummary) #Pour la production aisée de tableaux statistiques...a recours a flextable..


library(openxlsx) # Manipulation de fichiers excel
# library(readxl) #ppour la gestion des fichiers excel

library(haven) # lecture de fichier stata & autre
library(labelled) # gestion des etiquettes et autres notes des versions stata 12&plus

library(compareDF) # pour la comparaison des valeurs entre deux bases de donnees.

#----------------------------------------------------------------------------------------------->
# Chargement des variables immuables dans le temps
#----------------------------------------------------------------------------------------------->

# Nettoyage de l'emvironnement de travail
rm(list=ls())

# Templates
path_data <- "data_reporting"
path_dataFemme <- sprintf("%s/dataFemme.dta",path_data)
path_dataHomme <- sprintf("%s/dataHomme.dta",path_data)
template_report  <-  sprintf("%s/report.docx",path_data)
template_report2 <-  sprintf("%s/report_landscape.docx",path_data)
path_monitoring <- sprintf("%s/monitoring",path_data)

# Creation des dossiers ou les rapports seront stockes
dir_name <- strftime(today(),format="%y%m%d")
dir_reporting <- sprintf("./pfs_reporting/%s",dir_name)
if(!dir.exists(dir_reporting)) dir.create(dir_reporting,recursive = TRUE)

dir_agent <- sprintf("%s/agents",dir_reporting)
if(!dir.exists(dir_agent)) dir.create(dir_agent,recursive = TRUE)

dir_supervisor  <- sprintf("%s/supervisors",dir_reporting)
if(!dir.exists(dir_supervisor)) dir.create(dir_supervisor,recursive = TRUE)

dir_tow <- sprintf("%s/towns",dir_reporting)
if(!dir.exists(dir_tow)) dir.create(dir_tow,recursive = TRUE)

dir_monitoring <- sprintf("%s/monitoring",dir_reporting)
if(!dir.exists(dir_monitoring)) dir.create(dir_monitoring,recursive = TRUE)

rm(dir_name)

# Chargement des bases de données auxiliaires
cases <- read_csv(sprintf("%s/cases.csv",path_data))
cases <- cases %>% select(-X18)
# View(cases[problems(cases)[["row"]],])

# correction des noms des villages avec ce qui est effectivement implemente dans la base
towns <- read_csv(sprintf("%s/towns.csv",path_data))
cases <- left_join(cases %>% select(-town),y = towns %>% select(-townID),by="id_town")

target <- cases %>% 
  group_by(id_town,town,sample) %>% 
  summarise(nb_household=n()) %>%
  pivot_wider(names_from = sample,values_from = nb_household) %>%
  mutate(nbhh=ifelse(is.na(Primary),0,Primary),
         nbhh_replacement=ifelse(is.na(Replacements),0,Replacements),
         nbhh_byWomen = ceiling(nbhh/2)) %>%
  select(id_town,town,nbhh,nbhh_replacement,nbhh_byWomen) %>% ungroup() 

target <- bind_rows(target,
                    tibble(id_town=0,
                           town="TOTAL",
                           nbhh=sum(target$nbhh,na.rm = TRUE),
                           nbhh_replacement=sum(target$nbhh_replacement,na.rm = TRUE),
                           nbhh_byWomen=sum(target$nbhh_byWomen,na.rm = TRUE))
                    )
  
#Liste des identifiants des menages de remplacements
idlist_replacement <- (cases %>% filter(sample=="Replacements") %>% select(caseid))$caseid

# Liste des noms des agents et superviseurs
staff <- read_csv(sprintf("%s/staff.csv",path_data))

# Plan de deploiement => servira pour la mesure de la charge de travail restant au niveau
# de l'enquête total
deployment <- read_csv(sprintf("%s/deploiement.csv",path_data))

# # Liste des codes & labels des modalites par variables
# choices <- read_csv2(sprintf("%s/choices.csv",path_data))


# Variable de configuration des scripts

# 1ere evaluation......
# param$startdate <- "2021-08-12"
# param$enddate <- "2021-09-06"
# param$evaluationDate <- ymd("2021-09-07")

# 2e evaluation......
# param$startdate <- "2021-09-07"
# param$enddate <- "2021-09-13"
# param$evaluationDate <- ymd("2021-09-14")

param <- list(
  startdate=NULL, # "2021-09-07" <=> Toute la periode ou today() => date du jour 
  enddate = NULL, # "2021-09-13" ou sur le format "YYYY-MM-DD"
  evaluationDate=today(), #today() #1e evaluation => ymd("2021-09-07")
  agentList=NULL, # NULL=pour tous les agents ou, liste des noms des agents comme inscrit dans la BD
  supervisorList=NULL, # NULL=pour tous les agents ou, liste des noms des superviseurs
  townList=NULL # NULL=pour tous villages deja visites ou  liste des noms des villages (townList==id_town)
)


#----------------------------------------------------------------------------------------------->
# Chargement des bases de données à utiliser
#----------------------------------------------------------------------------------------------->
# Base de donnees femmes >
df <- haven::read_dta(path_dataFemme,encoding = "utf-8")
# Base de donnees hommes >
dh <- haven::read_dta(path_dataHomme,encoding = "utf-8")

# Correction de quelques incoherences mineures dans les donnees afin de faciliter l'exploitation
# des donnees

# Correction de l'identifiant du menage depuis l'identifiant au niveau individuel
df$caseid2 <- as.numeric(str_sub(df$caseid,start = 3,end = -1L))
dh$caseid2 <- as.numeric(str_sub(dh$caseid,start = 3,end = -1L))

# Conversion des durees en une duree lisible
varlist <- grep("duration",names(df),value = TRUE)
for(varname in varlist){
  df[varname] <- as.numeric(df[[varname]])
  df[sprintf("%s_lab",varname)] <- hms::as_hms(df[[varname]])
}

varlist <- grep("duration",names(dh),value = TRUE)
for(varname in varlist){
  dh[varname] <- as.numeric(dh[[varname]])
  dh[sprintf("%s_lab",varname)] <- hms::as_hms(dh[[varname]])
}
rm(varname, varlist)

# Noms des agents, des supperviseurs et des villages en majuscule
df["supervisorname"] <- str_to_upper(df[["supervisorname"]])
df["supervisorname"] <- str_replace_all(df[["supervisorname"]],"BISSENOHO / MVODO VICTOR","BISSENOHO / MVODO")
df["supervisorname"] <- str_replace_all(df[["supervisorname"]],"BETIMA / KOLOKO J.B.","BETIMA / KOLOKO")

dh["supervisorname"] <- str_to_upper(dh[["supervisorname"]])
dh["supervisorname"] <- str_replace_all(dh[["supervisorname"]],"BISSENOHO / MVODO VICTOR","BISSENOHO / MVODO")
dh["supervisorname"] <- str_replace_all(dh[["supervisorname"]],"BETIMA / KOLOKO J.B.","BETIMA / KOLOKO")

df["agentname"] <- str_to_upper(df[["agentname"]])
dh["agentname"] <- str_to_upper(dh[["agentname"]])

# Conversion au format date de la date de collecte..
df["survey_date"] <- ymd(df[["survey_date"]])
dh["survey_date"] <- ymd(dh[["survey_date"]])

# Ajout de certains champ manquant lors de l'exportation des fichiers au format stata
varlist <- c("submissiondate","starttime","endtime")

## Questionnaire femme
tmp <- read_csv(sprintf("%s/Questionnaire  Femme_WIDE.csv",path_data)) %>%
  select(key=KEY,submissiondate=SubmissionDate,starttime,endtime)
df <- left_join(x=df %>% select(-submissiondate,-starttime,-endtime),y=tmp,by="key")

## Questionnaire homme
tmp <- read_csv(sprintf("%s/Questionnaire  Homme_WIDE.csv",path_data)) %>%
  select(key=KEY,submissiondate=SubmissionDate,starttime,endtime)
dh <- left_join(x=dh %>% select(-submissiondate,-starttime,-endtime),y=tmp,by="key")

for(varname in varlist){
  df[varname] <- mdy_hms(df[[varname]])
  dh[varname] <- mdy_hms(dh[[varname]])
}

rm(varlist,tmp,varname)

# La variable respondent_name n'existe pas chez les hommes on va la cree pour
# faciliter le traitement...
dh$respondent_name <- sprintf("%s %s",dh$a14b,dh$a14a)

# Ajout d'une variable sexe pour distinguer les agents de sexe Femenin des autres
idlist <- unique(df[["agentname"]])
staff$sex <- "Homme"
staff$sex[which(staff$agent %in% idlist)] <- "Femme"
staff$sex <- factor(staff$sex,levels = c("Homme","Femme"))
rm(idlist)

# Seuil des donnees attendus par jour
tmp <- staff %>% group_by(sex) %>% summarise(eff=n())
param <- append(param,
                list(
                  threshold_manByDay = 8,
                  threshold_womanByDay = 4,
                  threshold_menByDay = 8*pluck(tmp,"eff")[1],
                  threshold_womenByDay = 4*pluck(tmp,"eff")[2],
                  threshold_investigatorsByDay = 8*pluck(tmp,"eff")[1]+4*pluck(tmp,"eff")[2],
                  threshold_timeInDailyActivity = 8*60, # en minute
                  threshold_timeInDailyActivities = 12*60, # en minute
                  threshold_numberOfSpouse = 4,
                  threshold_income = 2000000,
                  threshold_savings = 2000000,
                  threshold_debt = 2000000
                ))
rm(tmp)

# Conversion en numerique des indicateurs de recours au saut
varlist <- grep("prop_",names(df),value=TRUE)
for(varname in varlist) df[varname] <- as.numeric(df[[varname]])

varlist <- grep("prop_",names(dh),value=TRUE)
for(varname in varlist) dh[varname] <- as.numeric(dh[[varname]])

rm(varname, varlist)

# Conversion des durees passees dans les activites quotidiennes
for(varname in c("h12v","h13v","h14v","h15v","h16v")) df[varname] <- as.numeric(df[[varname]])
for(varname in c("f12v","f13v","f14v","f15v","f16v")) dh[varname] <- as.numeric(dh[[varname]])

# Correction du nom d'un des agents sur le questionnaire de key=uuid:f3b3986f-d1f7-441d-82e9-35f89a7dd281
# En lieu et place de "IYARCHIME LIMANGANA" mettre "SABIGNO NDENGUE DIKAKORO SHANICE SANDRA"
# df[["agentname"]][which(df[["agentname"]]=="IYARCHIME LIMANGANA")] <- "SABIGNO NDENGUE DIKAKORO SHANICE SANDRA"
df[["agentname"]][which(df[["key"]]=="uuid:f3b3986f-d1f7-441d-82e9-35f89a7dd281")] <- "SABIGNO NDENGUE DIKAKORO SHANICE SANDRA"


# Prise en compte des reaffectations des superviseurs ...
for(i in 1:nrow(staff)){
  if(staff$sex[i]=="Femme"){
    df[["supervisorname"]][which(df[["agentname"]]==staff$agent[i])] <- staff$supervisor_name[i]
  }else{
    dh[["supervisorname"]][which(dh[["agentname"]]==staff$agent[i])] <- staff$supervisor_name[i]
  }
}

# Conversion de l'identifiant de la ville
df$id_town <- as.numeric(df$a8)
dh$id_town <- as.numeric(dh$a8)

# Suppresion des questionnaires disponibles avant le debut effectif de la collecte
# Un seulquestionnaire concerne => agent de BELL IV à la date de "2021-08-02"
# Les applications n'avaient pas été reinitialiser comme demande...
dh <- dh %>% filter(starttime>=ymd("2021-08-12"))

# Correction des statuts d'enquête
##1. Manuellement 
dh$k12[which(dh$key=="uuid:818bac4c-f8af-4c55-871f-75bb6105adc5")] <- 2 # H-2221782413
dh$k12[which(dh$key=="uuid:3d916d50-88fa-47fd-9148-9ce020f88408")] <- 2 # H-2222252946
dh$k12[which(dh$key=="uuid:3fdb107d-b98a-4d1f-9027-2a9787ae7808")] <- 2 # H-2220391528
dh$k12[which(dh$key=="uuid:400a9d0a-b724-472d-8484-ea3c5376174f")] <- 2 # H-2221061856
dh$k12[which(dh$key=="uuid:64f1fce4-03d7-4ab6-9962-a3c9312aa223")] <- 2 # H-2221061846
df$n12[which(df$key=="uuid:d08f622a-9bb0-4377-a7e5-c31adb708018")] <- 2 # F-2221061852
#..........
df$n12[which(df$key=="uuid:951f9274-c8dc-4f97-b041-6cc8d39d0450")] <- 2 # F-1440110050
df$n12[which(df$key=="uuid:b6c08a07-1037-451d-b774-6f2cddeebf56")] <- 2 # F-1440110052
df$n12[which(df$key=="uuid:4d637429-fd15-43ae-82f4-d04ad9c07a6e")] <- 2 # F-1440430202
df$n12[which(df$key=="uuid:25f06529-a8e0-4756-96f8-1ccabde177fc")] <- 2 # F-1440430200
df$n12[which(df$key=="uuid:57c08071-1a3a-438a-823e-be03d4809cd9")] <- 2 # F-1440540234
df$n12[which(df$key=="uuid:d42f5a66-f7f5-40b6-8237-15436c4ab45e")] <- 2 # F-1440540238
df$n12[which(df$key=="uuid:338844a2-7ec4-4574-8ff1-58dc731e41eb")] <- 2 # F-1440540239
df$n12[which(df$key=="uuid:c55c3db5-ed9b-4902-a6da-44b8dd153bfd")] <- 2 # F-1440545623
df$n12[which(df$key=="uuid:71f293a9-1f3c-4afa-a7ce-e23ee6574339")] <- 2 # F-1441380623
df$n12[which(df$key=="uuid:22d89477-698e-40b7-9da1-5437fccc414f")] <- 2 # F-1441380632
df$n12[which(df$key=="uuid:744bf93a-90bb-4166-ad77-d4b38bc5527d")] <- 2 # F-1441380625
df$n12[which(df$key=="uuid:fab89c96-e555-4683-b7fb-3242f8a63c0f")] <- 2 # F-1441380653

rm(i,varname)

#----------------------------------------------------------------------------------------------->
# Chargement des fonctions utilitaires
#----------------------------------------------------------------------------------------------->

source("dataCheckingLibrary.R")
source("statisticsLibrary.R")
source("buildReports.R")

#----------------------------------------------------------------------------------------------->
# Identification à posteriori des observations qui devraient être affecte
#----------------------------------------------------------------------------------------------->
# ##2. Identification de tous les questionnaires menages dont l'un des questionnaires conjoints 
# # est complet et l'autre a un statut disntinct ... 
# idlistF <- unique(df$caseid2[which(df$n12==2)]) #caseid2= identifiant menage & caseid=identifiant individu
# idlistH <- unique(dh$caseid2[which(dh$k12==2)])
# 
# # formulaire homme avec statut different de complet alors que formulaire femme complet
# idlist <- unique((dh %>% filter(caseid2 %in% idlistF,k12!=2))$caseid2)
# extract.observations(filename="hommes_noncomplets.xlsx", idlist = idlist)
# 
# # formulaire femme avec statut different de complet alors que formulaire homme est complet
# idlist <- unique((df %>% filter(caseid2 %in% idlistH,n12!=2))$caseid2)
# extract.observations(filename="femmes_noncomplets.xlsx", idlist = idlist)
# 
# # Correction automatique des status...
# 
# rm(idlistF,idlistH,idlist)