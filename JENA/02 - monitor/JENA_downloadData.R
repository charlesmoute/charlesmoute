# Download & Traitement data for monitoring data
# charles.moute@gmail.com

#...............................................................................
# Chargement des librairies utilitaires
#...............................................................................
# Liste des packages utiliatau
library(openxlsx) # Pour exporter les donnees au format excel
library(robotoolbox) #pour telecharger les donnees depuis le serveur kobo
library(tidyverse)
# library(purrr)
# library(lubridate)
# library(dplyr) #pour manipuler les donnees

#...............................................................................
# Configuration de l'espace de travail
#...............................................................................
# Suppression des variables non desirables
rm(list=ls())
# configuration des parametres de connexion au serveur Kobo
kobo_setup()

# Variables utilitaires... les questnnaires d'interet ont tous ete cree le meme jour
databases <- kobo_asset_list() %>% 
  mutate(
    date_creation = lubridate::ymd(stringi::stri_sub(as.character(date_created),to=10)),
  ) %>% 
  filter(asset_type=="survey",owner_username=="charlesmoute",
         date_creation==lubridate::ymd("2023-05-12")) %>% 
  arrange(date_created) %>% select(uid,name,submissions) %>% 
  mutate(
    survey=c("quanti","quanti","quali","quali","quali",
             "quali","quali","quali","quali","quali","quali","quali"),
    agent=c("interviewer","interviewer","coordo_supervisor","interviewer","interviewer",
            "interviewer","interviewer","interviewer","interviewer","supervisor",
            "coordo","coordo"),
    order_id = c(1,2,5,7:12,6,3,4)
  ) %>% 
  arrange(order_id)
  
# Configuration des variables utilitaires permettant d'associer aisement
# le nom du base de données (name) avec son identifiant (uid)
tmp <- databases %>% filter(survey=="quanti") %>% select(uid,name)
dbname_quanti <- tmp$name ; names(dbname_quanti) <- tmp$uid

tmp <- databases %>% filter(agent=="coordo") %>% select(uid,name)
dbname_quali_coordo <- tmp$name ; names(dbname_quali_coordo) <- tmp$uid

tmp <- databases %>% filter(agent=="supervisor") %>% select(uid,name)
dbname_quali_supervisor <- tmp$name ; names(dbname_quali_supervisor) <- tmp$uid

tmp <- databases %>% filter(survey=="quali", agent=="interviewer") %>% select(uid,name)
dbname_quali_interviewer <- tmp$name ; names(dbname_quali_interviewer) <- tmp$uid
#Suppression des variables inutiles
rm(tmp)

# Test de visualisation d'une base de donnees afin de selection des variables
# caches comme le statut de validation
# print(databases) #glimpse(databases)
# db <- kobo_data(kobo_asset("aR5ZymMESrR6FBAm39RVKA"))
# glimpse(db)


# Objet regroupant tous les parametres d'interet pour le moniteur
param <- list(
  dbname = databases,
  dbname_quanti = dbname_quanti,
  dbname_quali_coordo = dbname_quali_coordo,
  dbname_quali_supervisor = dbname_quali_supervisor,
  dbname_quali_interviewer = dbname_quali_interviewer,
  survey_startdate = ymd("2023-05-30"),
  survey_enddate =  ymd("2023-05-30") + 16,
  pilote_startdate = ymd("2023-05-26"),
  pilote_enddate = ymd("2023-05-28"),
  nb_province = 6,
  nb_interviewer_quanti=40,
  nb_form_quantiy=3, #Minimun de questionnaire quanti par jour, par agent et par type. A partir de 5 tirer la sonnette
  nb_interviewer_quali= 15,
  nb_supervisor_quanti=6,
  nb_supervisor_quali=6,
  nb_form_quali_cluster = 20,
  nb_form_quali_mepst = 14,
  db_sample_quanti = read.xlsx("data/params.xlsx",sheet = "sample_quanti"),
  db_sample_quali = read.xlsx("data/params.xlsx",sheet = "sample_quali"),
  db_sample_quali_school = read.xlsx("data/params.xlsx",sheet = "sample_quali_school"),
  db_sample_data = read.xlsx("data/cases.xlsx",sheet = "echantillon"),
  db_agents = read.xlsx("data/agents.xlsx",sheet = "new_data"),
  forms_coordo = databases %>% filter(agent=="coordo") %>% select(uid) %>% unlist() %>%  as.character(),
  form_synthese = databases %>% filter(agent=="coordo_supervisor") %>% select(uid) %>% unlist() %>%  as.character(),
  forms_other = databases %>% filter(agent=="interviewer" | agent=="supervisor") %>% 
                  select(uid) %>% unlist() %>% as.character(),
  varlist = list(
    forms_coordo = c("start","end","today","hhid","enum_id","nom_agent","couverture","province",
                     "validation_status","validation_bywhom","xform_version","xform_id","telephone"),
     forms_other =c("start","end","today","hhid","enum_id","nom_agent","type_etablissement",
                     "province","division","subdivision","territoire","milres","secteur","ecole_sample0",
                    "ecole_sample0_estremplace","pourquoi_remplacement","ecole_sample1","autre_ecole_motif","schoolID","nom_ecole",
                    "validation_status","validation_bywhom","xform_version","xform_id","telephone"),
     form_synthese=c("start","end","today","hhid","enum_id","nom_agent","source_information","couverture_zone","couverture_province",
                     "type_etablissement","province","division","subdivision","territoire","milres","secteur","ecole_sample0",
                     "ecole_sample0_estremplace","pourquoi_remplacement","ecole_sample1","autre_ecole_motif","schoolID","nom_ecole",
                     "validation_status","validation_bywhom","xform_version","xform_id")
    #purrr::pluck(db,"_validation_status",1,"label")
    # sapply(1:49, function(x) purrr::pluck(db,"_validation_status",x,"label")) %>% unlist()
  ),
  validation_status = c("Aucun statut"="","En attente"="On Hold","Approuvé"="Approved","Non Approuvé"="Not Approved"),
  STATUT_APPROVED="Approved",
  STATUT_NOT_APPROVED="Not Approved",
  STATUT_UNDEFINED="",
  STAUT_ON_HOLD ="On Hold"
)
#Suppression des variables inutiles
rm(dbname_quali_coordo,dbname_quali_supervisor,dbname_quali_interviewer,dbname_quanti)

# validation_status <- c("Approuvé","Non approuvé","En attente")
# names(validation_status) <- c("Approved","Not Approved","On Hold")

#...............................................................................
# Traitement des données
#...............................................................................

# Traitement des donnees relatives aux deux formulaires du coordo
dbase_coordo <- NULL
for(formid in param$forms_coordo){
  db <- kobo_data(kobo_asset(formid)) %>% pluck("main")
  if(!is.null(db)){
    db <- db %>% 
      mutate(
        # validation_bywhom = map_chr(`_validation_status`, 
        #                             function(x){
        #                               result <- pluck(x,"by_whom")
        #                               if(is.null(result)){
        #                                 invisible("")
        #                               }else{
        #                                 invisible(result)
        #                               }
        #                             }),
        # validation_status=map_chr(`_validation_status`, 
        #                              function(x){
        #                                result <- pluck(x,"label")
        #                                if(is.null(result)){
        #                                  invisible("")
        #                                }else{
        #                                  invisible(result)
        #                                }
        #                              }),
        validation_bywhom = NA, # La nouvelle version du package ne le prend plus en charge
        validation_status=labelled::to_character(`_validation_status`), #revision nouvelle version package
        xform_version = `__version__`,xform_id=`_xform_id_string`,
      ) %>% 
      select(one_of(c(param$varlist[["forms_coordo"]]))) %>% #validation_bywhom
      mutate(
        form_id=formid,
        form_title = param$dbname_quali_coordo[formid]
      ) %>% 
      select(form_id,form_title,everything())
    dbase_coordo <- bind_rows(dbase_coordo,db)
  }
}

# Traitement des donnees relatives aux formulaires de synthese
dbase_synthese <- NULL
for(formid in param$form_synthese){
  db <- kobo_data(kobo_asset(formid)) %>% pluck("main") 
  if(!is.null(db)){
    db <- db %>% 
      mutate(
        # validation_bywhom = map_chr(`_validation_status`, 
        #                             function(x){
        #                               result <- pluck(x,"by_whom")
        #                               if(is.null(result)){
        #                                 invisible("")
        #                               }else{
        #                                 invisible(result)
        #                               }
        #                             }),
        # validation_status = map_chr(`_validation_status`, 
        #                              function(x){
        #                                result <- pluck(x,"label")
        #                                if(is.null(result)){
        #                                  invisible("")
        #                                }else{
        #                                  invisible(result)
        #                                }
        #                              }),
        validation_bywhom = NA, # La nouvelle version du package ne le prend plus en charge
        validation_status=labelled::to_character(`_validation_status`),
        xform_version = `__version__`,xform_id=`_xform_id_string`,
        ecole0 = as.character(ecole_sample0) %>% tidyr::replace_na(""),
        ecole1 = as.character(ecole_sample1) %>% tidyr::replace_na(""),
        schoolID = ifelse(readr::parse_number(ecole_sample0_estremplace)==1,as.character(ecole_sample1),
                          as.character(ecole_sample0)),
        id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID))
      ) %>% 
      select(one_of(c(param$varlist[["form_synthese"]]))) %>% 
      mutate(
        form_id=formid,
        form_title = param$dbname$name[which(param$dbname$uid==formid)]
      ) %>% 
      select(form_id,form_title,everything())
    dbase_synthese <- bind_rows(dbase_synthese,db)
  }
}

# Traitement des donnnees relatives aux autres questionnaires
dbase_others <- NULL
# dbnames <- c(param$dbname_quanti,param$dbname_quali_supervisor,param$dbname_quali_interviewer)
for(formid in param$forms_other){
  db <- kobo_data(kobo_asset(formid))
  if(formid!=names(param$dbname_quanti)[2]) db <- db %>% pluck("main") 
  if(!is.null(db)){
    
    if("source_telephone" %in% names(db)) db <- db %>% rename(telephone=source_telephone)
    if(!"telephone" %in% names(db)) db$telephone <- NA_character_
    
    db <- db %>% 
      mutate(
        # validation_bywhom = map_chr(`_validation_status`, 
        #                             function(x){
        #                               result <- pluck(x,"by_whom")
        #                               if(is.null(result)){
        #                                 invisible("")
        #                               }else{
        #                                 invisible(result)
        #                               }
        #                             }),
        # validation_status = map_chr(`_validation_status`, 
        #                              function(x){
        #                                result <- pluck(x,"label")
        #                                if(is.null(result)){
        #                                  invisible("")
        #                                }else{
        #                                  invisible(result)
        #                                }
        #                              } 
        #                              ),
        validation_bywhom = NA, # La nouvelle version du package ne le prend plus en charge
        validation_status=labelled::to_character(`_validation_status`),
        xform_version = `__version__`,xform_id=`_xform_id_string`,
        ecole0 = as.character(ecole_sample0) %>% tidyr::replace_na(""),
        ecole1 = as.character(ecole_sample1) %>% tidyr::replace_na(""),
        schoolID = ifelse(readr::parse_number(ecole_sample0_estremplace)==1,as.character(ecole_sample1),
                          as.character(ecole_sample0))
      ) %>% 
      select(one_of(c(param$varlist[["forms_other"]]))) %>% 
      mutate(
        form_id=formid,
        form_title = param$dbname$name[which(param$dbname$uid==formid)],#dbnames[formid],
        survey = ifelse(formid %in% names(param$dbname_quanti),"quanti","quali")
      ) %>% 
      select(form_id,form_title,everything())
    dbase_others <- bind_rows(dbase_others,db) 
  }
}


#...............................................................................
# Export des donnees
#...............................................................................

dbase <- list( # On ne conserve que les donnees valables depuis le debut de la collecte
  synthese=dbase_synthese %>% filter(start>=param$survey_startdate), 
  coordo=dbase_coordo %>% filter(start>=param$survey_startdate), # sans la base de donnees des fiches de synthese
  others=dbase_others %>% filter(start>=param$survey_startdate) # Toutes les autres basees quanti & 
)

rm(list = setdiff(ls(),c("param","dbase")))
# save.image(sprintf("JENA_Data_%s.RData",strftime(Sys.time(), "%y%m%d%H%M%S")))
save.image("JENA_Data.RData")
