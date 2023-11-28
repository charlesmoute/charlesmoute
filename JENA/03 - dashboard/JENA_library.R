# Fonctions utilitaires de monitoring de la collecte
# charles.moute@gmail.com

# ...............................................................................
# Chargement des librairies utilitaires
#...............................................................................

library(openxlsx) # Pour exporter les donnees au format excel
library(robotoolbox) #pour telecharger les donnees depuis le serveur kobo
library(tidyverse)

#...............................................................................
# Configuration de l'espace de travail
#...............................................................................

# Telechargement des donnnees depuis le serveur
# source("JENA_downloadData.R")

# Suppression des variables non desirables
rm(list=ls())

#Import des donnees du serveur
load("JENA_Data.RData")

#...............................................................................
# Fonction utilitaires...
#...............................................................................

# Export de donnees au format Excel [raccourci]
export.dataset <- function (database,filename,by_province=FALSE,without_manager_data=TRUE){
  
  # Export en un fichier 
  openxlsx::write.xlsx(database,file=filename,asTable = TRUE,creator="Charles Mouté",
    sheetName="data",keepNA=FALSE,colWidths="auto"
  )
  
  #Export des donnees par province
  if(by_province & "province" %in% names(database)){
    for(province_name in unique(database$province)){
      nom_fichier <- sprintf("%s_%s",province_name,filename)
      db <- database %>% filter(province==province_name)
      
      # Suppression des donnees des coordonnateur du volet quali
      if(without_manager_data & "form_id" %in% names(database))
        db <- db %>% filter(!form_id %in% names(param$dbname_quali_coordo))
      
      openxlsx::write.xlsx(db,file=nom_fichier,asTable = TRUE,creator="Charles Mouté",
        sheetName="data",keepNA=FALSE,colWidths="auto"
      )
    }
  }
}

# Fusion des bases de donnees qui exploitent l’échantillon des écoles
get.dataBasedOnSample <- function(statusOfValidation=c(param$STATUT_APPROVED,param$STATUT_UNDEFINED, param$STAUT_ON_HOLD)){
  result <- dbase$others %>%
    bind_rows(
      dbase$synthese %>% filter(readr::parse_number(source_information)>=3) %>%
        mutate(survey="quali",telephone=NA) %>% 
        select(one_of(names(dbase$others)))
    )
  
  if(!is.null(result) & nrow(result)>0){
    result <- result %>% 
      filter(validation_status %in% statusOfValidation)
  }
  invisible(result)
}

# Extraction de la liste des écoles de remplacements qui figure dans l'echantillon des 
# ecoles de remplacements
get.replacementSchoolsInSample <- function(statusOfValidation=c(param$STATUT_APPROVED,param$STATUT_UNDEFINED, param$STAUT_ON_HOLD)){
  db <- get.dataBasedOnSample(statusOfValidation) 
  result <- NULL
  
  if(!is.null(db) & nrow(db)>0){
    result <- db %>% filter(!is.na(ecole_sample1) & schoolID!='R-000') %>% 
      arrange(form_id,enum_id,today) %>% 
      mutate(nom_ecole_primaire=ecole_sample0 %>% labelled::to_character(),
             nom_ecole_remplacement=ecole_sample1 %>% labelled::to_character(),
             msg = sprintf("L'établissement %s a été remplacé par %s au motif suivant %s.",
                           nom_ecole_primaire,
                           nom_ecole_remplacement,
                           pourquoi_remplacement)
             ) %>% 
      select(province,division,subdivision,territoire,form_title,enum_id,nom_agent,today,nom_ecole_primaire,
             nom_ecole_remplacement,pourquoi_remplacement,msg,validation_status,form_id,hhid) %>% 
      distinct()
  }
  invisible(result)
}

# Extraction de la liste des écoles de remplacements en dehors de la liste de 
# remplacement disponible
get.replacementSchoolsOutSample <- function(statusOfValidation=c(param$STATUT_APPROVED,param$STATUT_UNDEFINED, param$STAUT_ON_HOLD)){
  db <- get.dataBasedOnSample(statusOfValidation) 
  result <- NULL
  
  if(!is.null(db) & nrow(db)>0){
    result <- db %>% 
      filter(schoolID=="R-000") %>% 
      # Prise en compte d'une adaptation stratégique sur le terrain
      mutate(
        to_exclude = str_starts(str_to_upper(str_trim(autre_ecole_motif)),"((\\[){0,1}( )*NEW( )*(\\])?)") | str_starts(str_to_upper(str_trim(nom_ecole)),"((\\[){0,1}( )*NEW( )*(\\])?)")
      ) %>% 
      filter(!to_exclude) %>% 
      arrange(form_id,enum_id,today) %>% 
      mutate(nom_ecole_primaire=ecole_sample0 %>% labelled::to_character(),
             nom_ecole_remplacement=nom_ecole,
             pourquoi_remplacement=autre_ecole_motif,
             msg = sprintf("L'établissement %s a été remplacé par %s au motif suivant %s.",
                           nom_ecole_primaire,
                           nom_ecole_remplacement,
                           pourquoi_remplacement)
      ) %>% 
      select(province,division,subdivision,territoire,form_title,enum_id,nom_agent,today,nom_ecole_primaire,
             nom_ecole_remplacement,pourquoi_remplacement,msg,validation_status,form_id,hhid) %>% 
      distinct()
  }
  invisible(result)
}

# Extraction de la liste des écoles qui ne sont pas listés parmi les informateurs cles
# et inversement
get.schoolWithoutEquivalentInformants <- function(statusOfValidation=c(param$STATUT_APPROVED,param$STATUT_UNDEFINED, param$STAUT_ON_HOLD)){
  db <- get.dataBasedOnSample(statusOfValidation) %>% 
    filter(form_id %in% names(param$dbname_quanti)) %>% 
    mutate(id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID)))
  result <- NULL
  
  if(!is.null(db) & nrow(db)>0){
    listOfSchools <- unique(db$id_ecole[db$form_id==(names(param$dbname_quanti)[2])])#2 => formid school
    listOfInformants <- unique(db$id_ecole[db$form_id %in% names(param$dbname_quanti)[1]]) #formid informants
    idlist_school <- setdiff(listOfSchools,listOfInformants)
    idlist_informants <- setdiff(listOfInformants,listOfSchools)
    
    if(!is.null(idlist_school) & length(idlist_school)>0)
      result <- result %>% bind_rows(
        db %>% filter(
          db$form_id==(names(param$dbname_quanti)[2]),
          id_ecole %in% unique(idlist_school)
        ) %>% 
          arrange(today) %>% 
          mutate(
            today=as.character(today),
            error_message= sprintf("L'établissement [ %s ] n'a pas de questionnaire informateur clé équivalent",nom_ecole)
          ) %>% 
          select(province,division,subdivision,territoire,form_title,type_etablissement,
                 enum_id,nom_agent,today,schoolID,nom_ecole,error_message,validation_status,hhid,form_id)
      )
    
    if(!is.null(idlist_informants) & length(idlist_informants)>0)
      result <- result %>% bind_rows(
        db %>% filter(
          db$form_id==(names(param$dbname_quanti)[1]),
          id_ecole %in% unique(idlist_informants)
        ) %>% 
          arrange(today) %>% 
          mutate(
            today=as.character(today),
            error_message= sprintf("L'établissement [ %s ] n'a pas de questionnaire école équivalent",nom_ecole)
          ) %>% 
          select(province,division,subdivision,territoire,form_title,type_etablissement,
                 enum_id,nom_agent,today,schoolID,nom_ecole,error_message,validation_status,hhid,form_id)
      )
    
  }
  invisible(result)
}

# Liste des questionnaires qui ne sont pas encore validées "On Hold", ""
# Questionnaire des coordo quali exclus.
get.unapprovedForms <- function(){
  result <- get.dataBasedOnSample(c(param$STATUT_UNDEFINED, param$STAUT_ON_HOLD)) %>% 
    mutate(error_message="[JENA] Questionnaire en attente d'examen") %>% 
    select(province,division,subdivision,territoire,form_title,enum_id,nom_agent,today,schoolID,nom_ecole,
           error_message,validation_status,hhid,form_id)
  invisible(result)
}

# Extraction des doublons dans l'ensemble des questionnaires
# administré par les agents et les superviseurs. Le doublon est au nivo
get.duplicatedData <- function (statusOfValidation=c(param$STATUT_APPROVED,param$STATUT_UNDEFINED, param$STAUT_ON_HOLD)){
  
  db <- get.dataBasedOnSample(statusOfValidation) 
  result <- NULL
  
  if(nrow(db)>0){
    result <- db %>% 
      mutate(
        id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID))
      ) %>% 
      group_by(form_id,id_ecole) %>% mutate(nb_form=n()) %>% 
      filter(nb_form>1) %>% ungroup() %>% 
      arrange(form_id,schoolID,enum_id,desc(today),desc(hhid)) %>% 
      select(province,division,subdivision,territoire,form_title,today,nom_ecole,enum_id,
             nom_agent,validation_status,schoolID,hhid,form_id) %>% 
      distinct()
  }
  invisible(result)
}

# Obtenir la liste des questionnaires qualitatifs qui n'ont pas de formulaire de
# synthèse et inversement, uniquement pour les agents et les superviseurs
# nous ne disposons pas d'information necessaires pour controler les questionnaires remplis par les coordo
# Entretien Cluster Education & MEPST
get.formsWithoutSummary <- function(statusOfValidation=c(param$STATUT_APPROVED,param$STATUT_UNDEFINED, param$STAUT_ON_HOLD)){
  form_list <- with(param$dbname,uid[which(survey=="quali" & agent!="coordo_supervisor")])
  varlist <- c("province","division","subdivision","territoire","today","form_title","nom_ecole","enum_id",
               "nom_agent","validation_status","schoolID","hhid","form_id","id_ecole")
  
  # Traitement des questionnaires qui exploitent l'echantillon d'ecole.. l'identifiant de l'ecole
  # sert a cette fin
  db_summary <- dbase$synthese %>% 
    filter(validation_status %in% statusOfValidation,
           readr::parse_number(source_information)>=3) %>% #MOUTE
    mutate(id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID))) %>%
    select(one_of(varlist))
  databases <- dbase$others %>% 
    filter(validation_status %in% statusOfValidation,form_id %in% form_list) %>% 
    mutate(id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID))) %>%
    select(one_of(varlist))
  
  idlist_01 <- setdiff(unique(db_summary$id_ecole),unique(databases$id_ecole))
  idlist_02 <- setdiff(unique(databases$id_ecole),unique(db_summary$id_ecole))
  
  result <- NULL
  if(!is.null(idlist_01) & length(idlist_01)>0)
    result <- db_summary %>% filter(id_ecole %in% idlist_01) %>% 
    mutate(msg=sprintf("Le formulaire de synthèse associé à l'établissement %s n'a pas de formulaire d'analyse",nom_ecole)) %>% 
    select(-id_ecole)
  
  if(!is.null(idlist_02) & length(idlist_02)>0)
    result <- result %>% bind_rows(
      databases %>% filter(id_ecole %in% idlist_02) %>%
        mutate(msg=sprintf("Le formulaire d'analyse associé à l'établissement %s n'a pas de formulaire de synthèse",nom_ecole)) %>% 
        select(-id_ecole)
    ) 
  
  # Traitement des formulaire de synthese qui n'exploite pas l'echantillon d'ecole
  # Exclut de ce controle car pas d'element sur lesquel on peut s'appuyer
  # pour verifier...
  
}

# Pour identifier de potentielles doublons surtout au niveau des formulaires d'analyse
# de la coordination, on vérifie si il y'a des formulaires distincts qui 
# aurait le même numeros de telephone.. 
check.duplicatedPhonenumber <- function(statusOfValidation=c(param$STATUT_APPROVED,param$STATUT_UNDEFINED, param$STAUT_ON_HOLD)){
  varlist <- c("province","division","subdivision","territoire","today","form_title","nom_ecole","enum_id",
               "nom_agent","telephone","validation_status","schoolID","hhid","form_id")
  
  # form_teacher <- with(param$dbname,uid[agent=="supervisor"])
    
  result <- dbase$others %>% 
    # filter(validation_status %in% statusOfValidation, !is.na(telephone),!form_id %in% form_teacher) %>% 
    filter(validation_status %in% statusOfValidation, !is.na(telephone) & telephone!='000') %>% 
    # mutate(id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID))) %>%
    select(one_of(varlist)) %>% 
    bind_rows(dbase$coordo %>% filter(validation_status %in% statusOfValidation, !is.na(telephone) & telephone!='000') %>% 
                mutate(division=NA,subdivision=NA,territoire=NA,nom_ecole=NA,schoolID=NA) %>% 
                select(one_of(varlist))) %>% 
    group_by(form_id,telephone) %>% mutate(nb_form=n()) %>% filter(nb_form>1) %>% ungroup() %>%
    arrange(telephone,form_id,enum_id,desc(hhid)) %>% select(-nb_form)
  invisible(result)
}

# Verifie qu'un numero de telephone n'est pas definie. Autrement dit qu'il 000
check.undefinedPhonenumber  <- function(statusOfValidation=c(param$STATUT_APPROVED,param$STATUT_UNDEFINED, param$STAUT_ON_HOLD)){
  varlist <- c("province","division","subdivision","territoire","today","form_title","nom_ecole","enum_id",
               "nom_agent","telephone","validation_status","schoolID","hhid","form_id")
  result <- dbase$others %>% 
    filter(validation_status %in% statusOfValidation, !is.na(telephone) & stringr::str_trim(telephone)=="000") %>% 
    # mutate(id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID))) %>%
    select(one_of(varlist)) 
  db <- dbase$coordo %>% 
                filter(validation_status %in% statusOfValidation, 
                       !is.na(telephone) & stringr::str_trim(telephone)=="000") %>% 
                mutate(division=NA,subdivision=NA,territoire=NA,nom_ecole=NA,schoolID=NA) %>% 
                select(one_of(varlist))
  if(nrow(result)>0 & nrow(db)>0) result <- result %>% bind_rows(db) 
  if(nrow(result)==0 & nrow(db)>0) result <- db 
  result <- result %>% 
    arrange(telephone,form_id,enum_id,desc(hhid))
  invisible(result)
}

# Pour l'ensemble des questionnaires quali, on indique si les fichiers audio et images sont disponibles ou non
get.infosOnAttachements <- function(statusOfValidation=c(param$STATUT_APPROVED,param$STATUT_UNDEFINED, param$STAUT_ON_HOLD),
                                    all_data=TRUE){
  varlist_full <- c("couverture","province","division","subdivision","territoire","today","form_title","nom_ecole",
                    "enum_id","nom_agent","validation_status","schoolID","form_id","hhid","jena_attachments")
  # "Audio","Image",
  varlist_monitoring <- c("form_id","form_title","hhid","jena_attachments") #"Audio","Image",
  result <- dbase$coordo %>%
    filter(validation_status %in% statusOfValidation) %>% 
    # Audio=NA,Image=NA,
    mutate(jena_attachments=NA,province=NA,division=NA,subdivision=NA,territoire=NA,
           schoolID=NA,nom_ecole=NA) %>%
    select(one_of(varlist_full)) %>% 
    bind_rows(
      dbase$others %>%
        filter(validation_status %in% statusOfValidation,
               form_id %in% c(names(param$dbname_quali_interviewer),names(param$dbname_quali_supervisor))) %>%
        mutate(jena_attachments=NA,couverture=NA) %>% #Audio=NA,Image=NA,
        select(one_of(varlist_full))
    )
  
  # On essaye de mettre a jour le fichier et de supprimer les elements deja update..
  if(!is.null(result) & nrow(result)>0){
    result$jena_attachments <- "EN ATTENTE"
    filename <- file.path("data","JENA_QUALI_PIECES_JOINTES_TREATED.xlsx")
    if(file.exists(filename)){
      wb <- loadWorkbook(filename)
      sheetname <- "data"
      # db <- read.xlsx(wb,sheet = sheetname) %>% filter(!is.na(Audio) | !is.na(Image))
      db <- read.xlsx(wb,sheet = sheetname) %>% filter(!is.na(jena_attachments))
      
      # On va considérer que toutes les grilles d'analyse ayant au moins un audio disponible
      # a toutes les pieces jointes attendus. Il y'a lieu de preciser que tous les non-disponibles
      # ce sont des grilles d'analyse pour lesquelles généralement les participants ont refusés
      # l'enregistrement audio de l'entretien 
      dbtmp <- result %>% select(-jena_attachments) %>% distinct() %>% 
        left_join(
          # db %>% rename(jena_attachments=Audio) %>% select(-c(form_title,Image)),
          db %>% select(form_id,hhid,jena_attachments) %>% distinct(),
          by=join_by(form_id,hhid),#multiple="last",
        ) %>% replace_na(list(jena_attachments="EN ATTENTE"))
      
      # On ne retourne que les données en attente de traitement...
      
      if(!all_data){
        result <- dbtmp %>% filter(jena_attachments=="EN ATTENTE")
      } else{
        result <- dbtmp
      }
        
      # On met a jour le fichier de suivi. Autrement dit on lui ajoute toutes les donnees
      # en attente de validation 
      db <- bind_rows(db,
                      dbtmp %>% 
                        filter(jena_attachments=="EN ATTENTE") %>% 
                        select(all_of(varlist_monitoring)) %>% 
                        mutate(across(where(is.logical),as.character))
                      )
      #Export des donnees dans le document externes
      # export.dataset(db %>% arrange(form_id,Audio,Image,hhid),filename)
      export.dataset(db %>% arrange(form_id,jena_attachments,hhid),filename)
    }
  }
  invisible(result)
}

# Fusionne toutes les donnees du projet en un seul fichier pour faciliter le contrôle en une fois
# on gardera les elements propre au plan d'echantillonnge, on ajoutera
# des colonnes pour chaque type de controle ci-dessus pour faciliter le calcul des indicatauer de performance
# comme le taux de non examen des questionnaires, taux de non appariement, taux de remplacememt
# etc...
get.dataForMonitoring <- function(){ #statusOfValidation=NULL

  varlist <- c("form_title","today",
               "couverture","province","division","subdivision","territoire","milres","secteur",
               "enum_id","nom_agent","type_etablissement","ecole_sample0_estremplace",
               "schoolID","validation_status","hhid","form_id")
  db <- NULL
  
  # Traitement de la base de donnees des coordo
  dbtmp <- dbase$coordo
  if(!is.null(dbtmp) & nrow(dbtmp)>0){
    sublist <- setdiff(varlist,names(dbtmp))
    if(!is.null(sublist)) for(varname in sublist) dbtmp[,varname] <- NA
    db <- dbtmp %>% select(all_of(varlist)) %>% mutate(province=NA) # Le champ province n'a pas ete traite on l'annule
  }
  
  #Traitement des autres bases de donnees
  dbtmp <- dbase$others
  if(!is.null(dbtmp) & nrow(dbtmp)>0){
    sublist <- setdiff(varlist,names(dbtmp))
    if(!is.null(sublist)) for(varname in sublist) dbtmp[,varname] <- NA
    if(is.null(db) | nrow(dbtmp)==0){
      db <- dbtmp %>% select(all_of(varlist))
    }else{
      db <- bind_rows(db,dbtmp %>% select(all_of(varlist)))
    }
  }
  
  if(!is.null(db) & nrow(db)>0){
    
    # if(!is.null(statusOfValidation) & length(statusOfValidation)>0)
    #   db <- db %>% filter(validation_status %in% statusOfValidation)
    
    # Si la base est null on arrete le traitement
    if(is.null(db) | nrow(db)==0) invisible(db)
    
    #Creation d'un identifiant tampon
    db <- db %>% 
      arrange(form_id,couverture,province,division,subdivision,territoire,milres,secteur,hhid) %>% 
      mutate(caseid=sprintf("%s-%s",form_id,hhid))
    
    # Ajout de quelques indices
    db <- db %>% 
      mutate(
        ecole_remplace = parse_number(ecole_sample0_estremplace),
        remplace_hors_echantillon = as.numeric(schoolID=="R-000"),
        form_approve = as.numeric(validation_status==param$STATUT_APPROVED),
        form_non_approve = as.numeric(validation_status==param$STATUT_NOT_APPROVED),
        form_en_attente = as.numeric(validation_status %in% c(param$STATUT_UNDEFINED, param$STAUT_ON_HOLD)),
        form_collected = 1) %>% 
      left_join(
        get.schoolWithoutEquivalentInformants() %>% 
          mutate(
            caseid=sprintf("%s-%s",form_id,hhid),
            schoolWithoutEquivalentInformants=1#,
            # schoolWithoutEquivalentInformants_msg=error_message
          ) %>% 
          # select(caseid,schoolWithoutEquivalentInformants,schoolWithoutEquivalentInformants_msg),
          select(caseid,schoolWithoutEquivalentInformants) %>% distinct(),
        by="caseid",multiple="last"
      ) %>% 
      left_join(
        get.duplicatedData() %>% 
          mutate(
            caseid=sprintf("%s-%s",form_id,hhid),
            duplicatedData=1
          ) %>% 
          select(caseid,duplicatedData) %>% distinct(),
        by="caseid",multiple="last"
      ) %>% 
      left_join(
        get.formsWithoutSummary() %>% 
          mutate(
            caseid=sprintf("%s-%s",form_id,hhid),
            formsWithoutSummary=1#,
            # formsWithoutSummary_msg=msg
          ) %>% 
          # select(caseid,formsWithoutSummary,formsWithoutSummary_msg),
          select(caseid,formsWithoutSummary) %>% distinct(),
        by="caseid",multiple="last"
      ) %>% 
      left_join(
        check.duplicatedPhonenumber() %>% 
          mutate(
            caseid=sprintf("%s-%s",form_id,hhid),
            duplicatedPhonenumber=1
          ) %>% 
          select(caseid,duplicatedPhonenumber) %>% distinct(),
        by="caseid",multiple="last"
      ) %>% 
      left_join(
        check.undefinedPhonenumber() %>% 
          mutate(
            caseid=sprintf("%s-%s",form_id,hhid),
            undefinedPhonenumber=1
          ) %>% 
          select(caseid,undefinedPhonenumber) %>% distinct(),
        by="caseid",multiple="last"
      ) %>% 
      left_join(
        get.infosOnAttachements() %>% 
          mutate(
            caseid=sprintf("%s-%s",form_id,hhid),
            attachements_unavailable=as.numeric(jena_attachments!="DISPONIBLE"),
            # audio_unavailable=ifelse(is.na(Audio),1,0),
            # image_unavailable=ifelse(is.na(Image),1,0),
          ) %>% 
          # select(caseid,audio_unavailable,image_unavailable) %>% distinct(),
          select(caseid,attachements_unavailable) %>% distinct(),
        by="caseid",multiple="last"
      ) %>% 
      replace_na(replace = list(
        schoolWithoutEquivalentInformants=0,duplicatedData=0,formsWithoutSummary=0,
        duplicatedPhonenumber=0,undefinedPhonenumber=0,attachements_unavailable=0
        # audio_unavailable=0,image_unavailable=0
      ))
    
    if(!is.null(db) & nrow(db)>0){
      
      # Liste des questionnaires quanti
      rowid <- which(db$form_id %in% names(param$dbname_quanti))
      if(length(rowid)>0){
        db$formsWithoutSummary[rowid] <- NA
        db$attachements_unavailable[rowid] <- NA
        # db$audio_unavailable[rowid] <- NA
        # db$image_unavailable[rowid] <- NA
      }
      
      # Liste des formulaires de synthese
      rowid <- which(db$form_id %in% param$form_synthese)
      if(length(rowid)>0){
        db$schoolWithoutEquivalentInformants[rowid] <- NA
        db$attachements_unavailable[rowid] <- NA
        # db$audio_unavailable[rowid] <- NA
        # db$image_unavailable[rowid] <- NA
      }
      
      # Liste des formulaires quali
      formlist<- c(names(param$dbname_quali_coordo),
                   names(param$dbname_quali_supervisor),
                   names(param$dbname_quali_interviewer))
      rowid <- which(db$form_id %in% formlist)
      if(length(rowid)>0){
        db$schoolWithoutEquivalentInformants[rowid] <- NA
      }
    }
  }
  invisible(db) 
}

# Comparaison des effectifs collectés avec celles des echantillons...
comparaison.expectedData_collectedData <- function(monitoringDate=strftime(Sys.time(), "%Y-%m-%d")){
  
  # Variables d'intérêt pour le programme
  db <- get.dataForMonitoring()
  nb_days_target <- as.numeric(param$survey_enddate - param$survey_startdate)
  nb_days_survey<-  as.numeric(lubridate::today()-param$survey_startdate)
  statusOfValidation <- c(param$STATUT_APPROVED,param$STATUT_UNDEFINED, param$STAUT_ON_HOLD)
  result <- list()
  
  #...........................................................................................
  # (1) Comparaison avec l'echantillon des donnees quanti d'origine
  #...........................................................................................
  dbtmp <- db %>% 
    filter(
      form_id %in% names(param$dbname_quanti),
      validation_status %in% statusOfValidation
    )
  
  db_sample <- param$db_sample_quanti %>% 
    arrange(province,etablissement) %>% 
    select(-survey) %>% rename(school_target=nb_school,keyInformant_target=nb_key_informer)
  
  # Taux de progression de la collecte...
  db_resultat <- dbtmp %>% 
    rename(etablissement=type_etablissement) %>% 
    mutate(form_lab=ifelse(form_id==names(param$dbname_quanti)[2],"school_survey",
                           ifelse(form_id==names(param$dbname_quanti)[1],
                                  "keyInformant_survey","nothing")),
           # province_label = labelled::to_character(province),
           province = parse_number(province),
           # etablissement_label=labelled::to_character(etablissement),
           etablissement=parse_number(etablissement)) %>% 
    group_by(province,etablissement,form_lab) %>% 
    summarise(nb_form=n(),.groups="drop") %>% 
    pivot_wider(names_from = form_lab, values_from = nb_form) %>% 
    arrange(province,etablissement) %>% 
   full_join(
     # Taux d'approbation de la collecte > mesure la fiabilite  theorique des
     # donnees
     dbtmp %>% 
       filter(validation_status==param$STATUT_APPROVED) %>% 
       rename(etablissement=type_etablissement) %>% 
       mutate(form_lab=ifelse(form_id==names(param$dbname_quanti)[2],"school_approbation",
                              ifelse(form_id==names(param$dbname_quanti)[1],
                                     "keyInformant_approbation","nothing_approbation")),
              # province_label = labelled::to_character(province),
              province = parse_number(province),
              # etablissement_label=labelled::to_character(etablissement),
              etablissement=parse_number(etablissement)) %>% 
       group_by(province,etablissement,form_lab) %>% 
       summarise(nb_form=n(),.groups="drop") %>% 
       pivot_wider(names_from = form_lab, values_from = nb_form) %>% 
       arrange(province,etablissement),
     by=c("province","etablissement")
   )
  
  # Au cas où on aurait jamais approuve des donnnees
  if(!"school_approbation" %in% names(db_resultat)){
    db_resultat$school_approbation <- NA
    db_resultat$keyInformant_approbation <- NA
  }
  
  if(!is.null(db_resultat) & nrow(db_resultat)>0){
    db_resultat <- db_sample %>% 
      left_join(
        db_resultat,
        by=c("province","etablissement")
      ) %>% 
      replace_na(list(keyInformant_survey=0,school_survey=0,
                      school_approbation=0,keyInformant_approbation=0)) %>% 
      select(
        province=province_label,etablissement=etablissement_label,
        school_target,school_survey,school_approbation,
        keyInformant_target,keyInformant_survey,keyInformant_approbation
      )
  }else{
    db_resultat <- db_sample %>% 
      mutate(
        school_survey = 0,
        school_approbation=0,
        keyInformant_survey=0,
        keyInformant_approbation=0
      ) %>% 
      select(
        province=province_label,etablissement=etablissement_label,
        school_target,school_survey,school_approbation,
        keyInformant_target,keyInformant_survey,keyInformant_approbation
      )
  }
  
  # Ajout des totaux
  db_resultat <- db_resultat %>% 
    bind_rows(
      db_resultat %>% 
        group_by(province) %>% 
        summarise(school_target=sum(school_target),
                  school_survey=sum(school_survey),
                  school_approbation=sum(school_approbation),
                  keyInformant_target=sum(keyInformant_target),
                  keyInformant_survey=sum(keyInformant_survey),
                  keyInformant_approbation=sum(keyInformant_approbation)) %>% 
        mutate(etablissement="TOTAL") %>% 
        select(all_of(names(db_resultat))),
      db_resultat %>% 
        summarise(school_target=sum(school_target),
                  school_survey=sum(school_survey),
                  school_approbation=sum(school_approbation),
                  keyInformant_target=sum(keyInformant_target),
                  keyInformant_survey=sum(keyInformant_survey),
                  keyInformant_approbation=sum(keyInformant_approbation)) %>% 
        mutate(province="TOTAL",etablissement="TOTAL") %>% 
        select(all_of(names(db_resultat)))
    ) %>% 
    arrange(province,etablissement)
    
  
  #Calcul des indicateurs associées
  db_resultat <- db_resultat %>% 
    mutate(
      total_target = school_target + keyInformant_target,
      total_survey = school_survey + keyInformant_survey,
      total_approbation = school_approbation + keyInformant_approbation,
      total_waited = round((total_target*nb_days_survey)/nb_days_target),
      tx_progression_global = round(100*total_survey/total_target,2),
      tx_approbation_global = ifelse(total_survey==0,0,round(100*total_approbation/total_survey,2)),
      tx_progression_global_msg=sprintf("[%s] %d questionnaires sur %d ciblées. %d questionnaires étaient attendues aprés %d jours de collecte.",
                                        ifelse(total_survey<total_waited,"EN RETARD",
                                               ifelse(total_survey>=total_target,"ACHEVE","EN COURS")),
                                        total_survey,total_target,total_waited,nb_days_survey),
      school_waited = round((school_target*nb_days_survey)/nb_days_target),
      tx_progression_school = round(100*school_survey/school_target,2),
      tx_approbation_school = ifelse(school_survey==0,0,round(100*school_approbation/school_survey,2)),
      tx_progression_school_msg = sprintf("[%s] %d questionnaires écoles sur %d ciblées. %d questionnaires étaient attendues aprés %d jours de collecte.",
                                         ifelse(school_survey<school_waited,"EN RETARD",
                                                ifelse(school_survey>=school_target,"ACHEVE","EN COURS")),
                                         school_survey,school_target,school_waited,nb_days_survey),
      keyInformant_waited = round((keyInformant_target*nb_days_survey)/nb_days_target),
      tx_progression_keyInformant = round(100*keyInformant_survey/keyInformant_target,2),
      tx_approbation_keyInformant = ifelse(keyInformant_survey==0,0,round(100*keyInformant_approbation/keyInformant_survey,2)),
      tx_progression_keyInformant_msg = sprintf("[%s] %d questionnaires informateurs clés sur %d ciblées. %d questionnaires étaient attendues aprés %d jours de collecte.",
                                          ifelse(keyInformant_survey<keyInformant_waited,"EN RETARD",
                                                 ifelse(keyInformant_survey>=keyInformant_target,"ACHEVE","EN COURS")),
                                          keyInformant_survey,keyInformant_target,keyInformant_waited,nb_days_survey)
    )

  result[["monitor_quanti"]] <- db_resultat %>% mutate(monitoring_date=monitoringDate)
  
  #...........................................................................................
  # (2) Comparaison avec l’échantillon des donnees qualitative d'origine sans les coordo
  #...........................................................................................
  
  # Variables utilitaires
  form_teacher <- names(param$dbname_quali_supervisor)
  
  dbtmp <- db %>% 
    filter(
      form_id %in% c(names(param$dbname_quali_supervisor),names(param$dbname_quali_interviewer)),
      validation_status %in% statusOfValidation
    ) %>% 
    mutate(
      form_title=ifelse(parse_number(type_etablissement)==3,sprintf("%s PRIMAIRE",form_title),
                        ifelse(parse_number(type_etablissement)==4,sprintf("%s SECONDAIRE",form_title),
                               form_title)),
      form_id = ifelse(form_id==form_teacher & parse_number(type_etablissement)==3,sprintf("P-%s",form_id),
                       ifelse(form_id==form_teacher & parse_number(type_etablissement)==4,sprintf("S-%s",form_id),
                              form_id))
    )
  
  db_sample <- param$db_sample_quali %>% 
    arrange(province,formid) %>% 
    select(-survey) %>% 
    rename(form_target=nb_form,rural_target=nb_form_rural,urban_target=nb_form_urban)
  
  # Taux de progression de la collecte...
  db_resultat <- dbtmp %>% 
    rename(formid=form_id) %>% 
    mutate(form_lab=ifelse(parse_number(milres)==1,"urban_survey",
                           ifelse(parse_number(milres)==2,
                                  "rural_survey","nothing_survey")),
           # province_label = labelled::to_character(province),
           province = parse_number(province),
           # milres=labelled::to_character(milres),
           milres=parse_number(milres)) %>% 
    group_by(province,formid,form_lab) %>% 
    summarise(nb_form=n(),.groups="drop") %>% 
    pivot_wider(names_from = form_lab, values_from = nb_form) %>% 
    arrange(province,formid) %>% 
    full_join(
      # Taux d'approbation de la collecte > mesure la fiabilite  theorique des
      # donnees
      dbtmp %>% 
        filter(validation_status==param$STATUT_APPROVED) %>% 
        rename(formid=form_id) %>% 
        mutate(form_lab=ifelse(parse_number(milres)==1,"urban_approbation",
                               ifelse(parse_number(milres)==2,
                                      "rural_approbation","nothing_approbation")),
               # province_label = labelled::to_character(province),
               province = parse_number(province),
               # milres=labelled::to_character(milres),
               milres=parse_number(milres)) %>% 
        group_by(province,formid,form_lab) %>% 
        summarise(nb_form=n(),.groups="drop") %>% 
        pivot_wider(names_from = form_lab, values_from = nb_form) %>% 
        arrange(province,formid), 
      by=c("province","formid")
    )
  
  # Au cas où on aurait jamais approuve des donnnees
  if(!"urban_approbation" %in% names(db_resultat)){
    db_resultat$rural_approbation <- NA
    db_resultat$urban_approbation <- NA
  }
  
  if(!is.null(db_resultat) & nrow(db_resultat)>0){
    db_resultat <- db_sample %>% 
      left_join(
        db_resultat,
        by=c("province","formid")
      ) %>% 
      replace_na(list(rural_survey=0,urban_survey=0,rural_approbation=0,
                      urban_approbation=0)) %>% 
      select(
        province=province_label,form_title,
        rural_target,rural_survey,rural_approbation,
        urban_target,urban_survey,urban_approbation
      )
  }else{
    db_resultat <- db_sample %>% 
      mutate(
        rural_survey = 0,
        rural_approbation=0,
        urban_survey=0,
        urban_approbation=0
      ) %>% 
      select(
        province=province_label,form_title,
        rural_target,rural_survey,rural_approbation,
        urban_target,urban_survey,urban_approbation
      )
  }
  
  # Ajout des totaux
  db_resultat <- db_resultat %>% 
    bind_rows(
      db_resultat %>% 
        group_by(province) %>% 
        summarise(rural_target=sum(rural_target),
                  rural_survey=sum(rural_survey),
                  rural_approbation=sum(rural_approbation),
                  urban_target=sum(urban_target),
                  urban_survey=sum(urban_survey),
                  urban_approbation=sum(urban_approbation)) %>% 
        mutate(form_title="TOTAL") %>% 
        select(all_of(names(db_resultat))),
      db_resultat %>% 
        group_by(form_title) %>% 
        summarise(rural_target=sum(rural_target),
                  rural_survey=sum(rural_survey),
                  rural_approbation=sum(rural_approbation),
                  urban_target=sum(urban_target),
                  urban_survey=sum(urban_survey),
                  urban_approbation=sum(urban_approbation)) %>% 
        mutate(province="TOTAL") %>% 
        select(all_of(names(db_resultat))),
      db_resultat %>% 
        summarise(rural_target=sum(rural_target),
                  rural_survey=sum(rural_survey),
                  rural_approbation=sum(rural_approbation),
                  urban_target=sum(urban_target),
                  urban_survey=sum(urban_survey),
                  urban_approbation=sum(urban_approbation)) %>% 
        mutate(province="TOTAL",form_title="TOTAL") %>% 
        select(all_of(names(db_resultat)))
    ) %>% 
    arrange(province,form_title)
  
  
  #Calcul des indicateurs associées
  db_resultat <- db_resultat %>% 
    mutate(
      
      total_target = urban_target + rural_target,
      total_survey=urban_survey+rural_survey,
      total_approbation=urban_approbation+rural_approbation,
      total_waited = round((total_target*nb_days_survey)/nb_days_target),
      tx_progression_global = round(100*total_survey/total_target,2),
      tx_approbation_global = ifelse(total_survey==0,0,round(100*total_approbation/total_survey,2)),
      tx_progression_global_msg=sprintf("[%s] %d formulaires sur %d ciblées. %d fomulaires étaient attendues aprés %d jours de collecte.",
                                        ifelse(total_survey<total_waited,"EN RETARD",
                                               ifelse(total_survey>=total_target,"ACHEVE","EN COURS")),
                                        total_survey,total_target,total_waited,nb_days_survey),
      rural_waited = round((rural_target*nb_days_survey)/nb_days_target),
      tx_progression_rural = round(100*rural_survey/rural_target,2),
      tx_approbation_rural = ifelse(rural_survey==0,0,round(100*rural_approbation/rural_survey,2)),
      tx_progression_rural_msg = sprintf("[%s] %d formulaires sur %d ciblées. %d formulaires étaient attendues aprés %d jours de collecte.",
                                         ifelse(rural_survey<rural_waited,"EN RETARD",
                                                ifelse(rural_survey>=rural_target,"ACHEVE","EN COURS")),
                                         rural_survey,rural_target,
                                         rural_waited,nb_days_survey),
      urban_waited = round((urban_target*nb_days_survey)/nb_days_target),
      tx_progression_urban = round(100*urban_survey/urban_target,2),
      tx_approbation_urban = ifelse(urban_survey==0,0,round(100*urban_approbation/urban_survey,2)),
      tx_progression_urban_msg = sprintf("[%s] %d formulaires sur %d ciblées. %d formulaires étaient attendues aprés %d jours de collecte.",
                                         ifelse(urban_survey<urban_waited,"EN RETARD",
                                                ifelse(urban_survey>=urban_target,"ACHEVE","EN COURS")),
                                         urban_survey,urban_target,urban_waited,nb_days_survey)
    )
  result[["monitor_quali"]] <- db_resultat %>% mutate(monitoring_date=monitoringDate)
  
  #...........................................................................................
  # (3) Comparaison avec l'echantillon des ecoles 
  # dans la mesure ou on a pas
  #...........................................................................................
  
  dbtmp <- db %>% 
    filter(
      form_id %in% names(param$dbname_quali_supervisor),
      validation_status %in% statusOfValidation
    ) %>% 
    mutate(
      form_title=ifelse(parse_number(type_etablissement)==3,sprintf("%s PRIMAIRE",form_title),
                        ifelse(parse_number(type_etablissement)==4,sprintf("%s SECONDAIRE",form_title),
                               form_title)),
      form_id = ifelse(form_id==form_teacher & parse_number(type_etablissement)==3,sprintf("P-%s",form_id),
                       ifelse(form_id==form_teacher & parse_number(type_etablissement)==4,sprintf("S-%s",form_id),
                              form_id))
    )
  
  db_sample <- param$db_sample_quali_school %>% 
    arrange(province,formid,milres,secteur,type_etablissement) %>% 
    select(-survey) %>% rename(form_target=nb_form)
  
  # Taux de progression de la collecte...
  db_resultat <- dbtmp %>% 
    rename(formid=form_id) %>% 
    mutate(# province_label = labelled::to_character(province),
      province = parse_number(province),
      # milres=labelled::to_character(milres),
      milres=parse_number(milres),
      # milres=labelled::to_character(milres),
      secteur=parse_number(secteur),
      # milres=labelled::to_character(milres),
      type_etablissement=parse_number(type_etablissement)) %>% 
    group_by(province,formid,milres,secteur,type_etablissement) %>% 
    summarise(form_survey=n(),.groups="drop") %>% 
    arrange(province,formid,milres,secteur,type_etablissement) %>% 
    full_join(
      # Taux d'approbation de la collecte > mesure la fiabilite  theorique des
      # donnees
      dbtmp %>% 
        filter(validation_status==param$STATUT_APPROVED) %>% 
        rename(formid=form_id) %>% 
        mutate(# province_label = labelled::to_character(province),
          province = parse_number(province),
          # milres=labelled::to_character(milres),
          milres=parse_number(milres),
          # milres=labelled::to_character(milres),
          secteur=parse_number(secteur),
          # milres=labelled::to_character(milres),
          type_etablissement=parse_number(type_etablissement)) %>% 
        group_by(province,formid,milres,secteur,type_etablissement) %>% 
        summarise(form_approbation=n(),.groups="drop") %>% 
        arrange(province,formid,milres,secteur,type_etablissement), 
      by=c("province","formid","milres","secteur","type_etablissement")
    )
  
  # Au cas où on aurait jamais approuve des donnnees
  if(!"form_approbation" %in% names(db_resultat)){
    db_resultat$form_approbation <- NA
  }
  
  if(!is.null(db_resultat) & nrow(db_resultat)>0){
    db_resultat <- db_sample %>% 
      left_join(
        db_resultat,
        by=c("province","formid","milres","secteur","type_etablissement")
      ) %>% 
      replace_na(list(form_survey=0,form_approbation=0)) %>% 
      select(
        province=province_label,form_title,milres=milres_lab,secteur=secteur_lab,
        type_etablissement = type_etablissement_lab,
        form_target,form_survey,form_approbation
      )
  }else{
    db_resultat <- db_sample %>% 
      mutate(
        form_survey = 0,
        form_approbation=0
      ) %>% 
      select(
        province=province_label,form_title,milres=milres_lab,secteur=secteur_lab,
        type_etablissement = type_etablissement_lab,
        form_target,form_survey,form_approbation
      )
  }
  
  # Ajout des totaux
  db_resultat <- db_resultat %>% 
    bind_rows(
      db_resultat %>% 
        group_by(province) %>% 
        summarise(form_target=sum(form_target),
                  form_survey=sum(form_survey),
                  form_approbation=sum(form_approbation),
                  .groups="drop") %>% 
        mutate(form_title="TOTAL",milres="TOTAL",secteur="TOTAL",type_etablissement="TOTAL") %>% 
        select(all_of(names(db_resultat))),
      db_resultat %>% 
        group_by(milres,secteur,type_etablissement) %>% 
        summarise(form_target=sum(form_target),
                  form_survey=sum(form_survey),
                  form_approbation=sum(form_approbation),
                  .groups="drop") %>% 
        mutate(province="TOTAL",form_title="TOTAL") %>% 
        select(all_of(names(db_resultat)))
    ) %>%
    arrange(province,desc(milres),desc(secteur),type_etablissement) %>%
    bind_rows(
      db_resultat %>% 
        summarise(form_target=sum(form_target),
                  form_survey=sum(form_survey),
                  form_approbation=sum(form_approbation),
                  .groups="drop") %>% 
        mutate(province="TOTAL",form_title="TOTAL",milres="TOTAL",secteur="TOTAL",type_etablissement="TOTAL") %>% 
        select(all_of(names(db_resultat)))
    ) %>% 
    select(-form_title) %>% 
    mutate(milres=str_to_upper(milres),secteur=str_to_upper(secteur))
  
  
  
  #Calcul des indicateurs associées
  db_resultat <- db_resultat %>% 
    mutate(
      form_waited = round((form_target*nb_days_survey)/nb_days_target),
      tx_progression_global = round(100*form_survey/form_target,2),
      tx_approbation_global = ifelse(form_survey==0,0,round(100*form_approbation/form_survey,2)),
      tx_progression_global_msg=sprintf("[%s] %d formulaires sur %d ciblées. %d fomulaires étaient attendues aprés %d jours de collecte.",
                                        ifelse(form_survey<form_waited,"EN RETARD",
                                               ifelse(form_survey>=form_target,"ACHEVE","EN COURS")),
                                        form_survey,form_target,form_waited,nb_days_survey)
    )
  result[["monitor_quali_school"]] <- db_resultat %>% mutate(monitoring_date=monitoringDate)
  
  #...........................................................................................
  # (4) Evaluation de la collecte des donnees des coordo du quali .. pas de reels comparaison
  # dans la mesure ou on a pas
  #...........................................................................................
  
  # Base de donnees d'analyse
  dbtmp <- db %>% 
    filter(
      form_id %in% names(param$dbname_quali_coordo),
      validation_status %in% statusOfValidation
    )
  
  #?tribble
  db_sample <- tibble(
    formid = names(param$dbname_quali_coordo),
    form_title = param$dbname_quali_coordo,
    form_target = c(param$nb_form_quali_mepst,param$nb_form_quali_cluster)
  )
  
  # Base de donnees resultat
  db_resultat <-  dbtmp %>% 
    group_by(form_title,couverture) %>% 
    summarise(form_survey=sum(form_collected),
              form_approbation=sum(form_approve),
              .groups="drop") %>% 
    mutate(form_target=NA,form_waited=NA,tx_progression=NA,
           tx_approbation=ifelse(form_survey==0,0,round(100*form_approbation/form_survey,2)),
           tx_progression_msg=NA) %>% 
    select(form_title,couverture,
           form_target,form_waited,form_survey,form_approbation,
           tx_progression,tx_approbation,tx_progression_msg) %>% 
    arrange(form_title,couverture) 
  
  
  db_resultat <- db_resultat %>% 
    # Ajout du total
    bind_rows(
      dbtmp %>% 
        group_by(form_title) %>% 
        summarise(
          form_survey=sum(form_collected),
          form_approbation=sum(form_approve),
          .groups="drop"
        ) %>% 
        full_join(db_sample %>% select(-formid),by=c("form_title")) %>% 
        replace_na(list(form_survey=0,form_approbation=0)) %>% 
        mutate(
          couverture="TOTAL",
          # form_target = ifelse(form_title==param$dbname_quali_coordo[1],param$nb_form_quali_mepst,param$nb_form_quali_cluster),
          form_waited = round((form_target*nb_days_survey)/nb_days_target),
          tx_progression = round(100*form_survey/form_target,2),
          tx_approbation = round(100*form_approbation/form_survey,2),
          tx_progression_msg = sprintf("[%s] %d formulaires sur %d ciblées. %d formulaires étaient attendues aprés %d jours de collecte.",
                                             ifelse(form_survey<form_waited,"EN RETARD",
                                                    ifelse(form_survey>=form_target,"ACHEVE","EN COURS")),
                                       form_survey,form_target,form_waited,nb_days_survey)
        ) %>% 
        select(all_of(names(db_resultat))),
      dbtmp %>% 
        summarise(
          form_survey=sum(form_collected),
          form_approbation=sum(form_approve),
          .groups="drop"
        ) %>% 
        mutate(
          form_title="TOTAL",
          couverture="TOTAL",
          form_target = param$nb_form_quali_mepst+param$nb_form_quali_cluster,
          form_waited = round((form_target*nb_days_survey)/nb_days_target),
          tx_progression = round(100*form_survey/form_target,2),
          tx_approbation = round(100*form_approbation/form_survey,2),
          tx_progression_msg = sprintf("[%s] %d formulaires sur %d ciblées. %d formulaires étaient attendues aprés %d jours de collecte.",
                                       ifelse(form_survey<form_waited,"EN RETARD",
                                              ifelse(form_survey>=form_target,"ACHEVE","EN COURS")),
                                       form_survey,form_target,form_waited,nb_days_survey)
        ) %>% 
        select(all_of(names(db_resultat)))
    ) %>% 
    arrange(form_title,couverture)
  
  # Export des resultats
  result[["monitor_quali_manager"]] <- db_resultat %>% mutate(monitoring_date=monitoringDate)
  
  invisible(result)
   
}

# Calcul des indicateur de performance de l'enquête pour le dashboard sur R-Shiny
compute.keyPerformanceIndicator <- function(){
  
  # Ici on pourra utilisé des sous-fusion qui extrait de 
  # comparaison.expectedData_collectedData
  # les informations selon la province en minuscule : ituri,kasai-central, kasai-oriental,... any
  # l'etablissemnent : crs,prescolaire,primaire,secondaire, any <=> total
  # type de questionnaire : school,keyInformant, any <=> total
  
  # Calcul des indicateurs de performances dans l'ensemble et 
  # Une fonction calculera les numerateurs et denominateur par agent
  # apres on sommera by province et ensuite pour le global comme si haut
  # et apres on fera des fonctions d'appels des resultats indivdiduels
  
  
  # Ensuite on utilisera une fonction
  # Taux de progression
  # Taux d'approbation des données 
  # Taux de non approbation des donnees = Not approved
  # Taux de non validation des donnees => On Hold & Empty
  # Taux de remplacement intra & extra echantillon
  # Taux de duplication des données, taux de duplication des contacts
  # Proportion des formulaires quali avec des pièces jointes manquantes
  
  # result <- list(global=NULL,by_supervisor=NULL,by_interviewer=NULL)
  # by_supervisor =by_province
  
  # Verification que les donnees valides l'echantillon des ecoles
  # nb_form_targeted
  # nb_form_collected [non approved eclus], 
  # nb_form_approved
  # nb_days_survey = as.numeric(param$survey_enddate - param$survey_startdate)
  # nb_days_collection = as.numeric(lubridate::today()-param$survey_startdate)
  # nb_form_waited = round((nb_form_targeted*nb_days_collection)/nb_days_survey)
  # En retard si nb_form_collected < nb_form_waited
  # tx_progression_waited = 100* nb_form_waited/nb_form_targeted
  # tx_progression_extended = 100*nb_form_collected/nb_form_targeted
  # tx_progression_restricted = 100*nb_form_collected/nb_form_targeted
  # tx_progression_extended < tx_progression_waited => Retard
  # Risque élevé de non atteinte des objectif si [voir ci-dessous]
  # tx_progression_restricted!=tx_progression_extended & tx_progression_restricted < tx_progression_waited 
  
}

# Mise a jour du dashboard fait sur Excel. 
# [Trop la flemme pour finaliser le dashboard avec RShiny]
update.jenaDashboard <- function(monitoringDate=strftime(Sys.time(), "%Y-%m-%d")){

  jena_data_name <- str_sort(list.files(pattern = "^(JENA_DATA).*(\\.xlsx)$",recursive = TRUE),
                             decreasing = TRUE)[1]
  monitoring_date <- parse_number(jena_data_name)

  jena_dashboard_name <- "JENA_DASHBOARD"
  jena_dashboard_file <- file.path("output",sprintf("%s.xlsx",jena_dashboard_name))
  if(file.exists(jena_dashboard_file)){

    wb <- loadWorkbook(jena_dashboard_file)

    #sauvegarde d'abord le fichier, si la sauvegarde n'existe pas
    jena_dashboard_save <- file.path("output","save",sprintf("%s_%s.xlsx",jena_dashboard_name,monitoringDate))
    jena_dashboard_copy <- file.path(sprintf("%s_%0.0f.xlsx",jena_dashboard_name,monitoring_date))
    if(!file.exists(jena_dashboard_save)){
      result <- saveWorkbook(wb,jena_dashboard_save,overwrite = TRUE)
      if(result) cat("\nSauvegarde effectuée : ",jena_dashboard_save)
      if(!result) cat("\nSauvegarde non effectuée : ",jena_dashboard_save)
    }else{
      cat("\nAucune sauvegarde de ",jena_dashboard_save, "n'a été effectuée")
    }

    # Traitement des donnees
    jena_data <- readxl::read_excel(jena_data_name)
    if(!is.null(jena_data) & nrow(jena_data)>0){
      jena_data$date_collecte <- lubridate::ymd(jena_data$date_collecte)
      sheetname <- "data"
      removeTable(wb,sheetname,getTables(wb,sheetname))
      writeDataTable(wb,sheetname,jena_data,tableName = "dataset",withFilter=TRUE,keepNA=FALSE)
      setColWidths(wb,sheetname,1:ncol(jena_data),widths = "auto")
      freezePane(wb,sheetname,firstActiveRow=2,firstActiveCol=2)
      suppressWarnings(result <- saveWorkbook(wb,jena_dashboard_file,overwrite = TRUE))
      if(result){
        cat("\nMise à jour effectuée : ",jena_dashboard_file)
        suppressWarnings(result <- saveWorkbook(wb,jena_dashboard_copy,overwrite = TRUE))
        if(result){
          cat("\nCopie ",jena_dashboard_copy, " effectuée avec succés")
        }else{
          cat("\nEchec de la copie ",jena_dashboard_copy, " effectuée avec succés")
        }
      }else{
        cat("\nEchec de la mise à jour : ",jena_dashboard_file)
      }

      cat("\nMise à jour effectuée : ",jena_dashboard_file)
      #
    }else{
      cat("\nFaute de données aucune mise à jour n'a été effectuée")
    }
  }else{
    cat("\nAucune mise à jour n'a été effectuée")
  }
}
