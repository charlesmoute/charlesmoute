# Reporting inconsistencies/errors
# Charles M. <charlesmoute@gmail.com>
# version 210827-0630

#----------------------------------------------------------------------------------------------->
# Fonctions utilitaires
#----------------------------------------------------------------------------------------------->

# Identification des doublons parmi les identifiants.
# Exemple 1 :
#     result <- check.duplicatedID(df,startdate = today(),enddate=today())
#     if(nrow(result$value) print(result$msg)
#     if(!nrow(result$value)) print(result$msg)
# Exemple 2 :
#     result <- check.duplicatedID(df,startdate =ymd("2021-08-15"),enddate=today())
#     if(nrow(result$value)) print(result$msg)
check.duplicatedID <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,supervisorList=NULL, 
                               townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  } else {
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    statut <- as.name(ifelse(is_womenForm,'n12','k12'))
    
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList)
    # result <- list()
    if(nrow(dbase)){
      tmp <- str_sort(dbase[["caseid"]][which(duplicated(dbase[["caseid"]]))])
      result[["msg"]] <- c(sprintf("%d cas de doublons ont été identifiés pour %d identifiants dont la liste est la suivante : %s. ",
                                   length(tmp),length(unique(tmp)),paste(unique(tmp),collapse ="; ")),
                           "Bien vouloir indiquer dans la colonne <todelete> quelle observation doit être supprimée ou non.")
      result[["value"]] <- dbase %>% 
        filter(caseid %in% unique(tmp)) %>% 
        select(caseid,town,supervisorname,agentname,pfs_info=pfs_hoh_name,respondent_name,
               starttime,endtime,submissiondate,survey_statut=!!statut,key) %>%
        arrange(caseid,starttime,endtime,submissiondate) %>%
        mutate(todelete="") %>%
        labelled::to_factor()
    }
  }
  #if(dim(result$value)[1]) print(result$msg)
  #rm(enddate,startdate,data,start,end,dbase,result,tmp,statut,agentList,supervisorList,townList,is_womenForm)
  invisible(result)
}


#Identification des questionnaires qui n'ont pas de consentement
check.consentment <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,supervisorList=NULL, 
                              townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  } else {
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList)
    
    # result <- list()
    if(nrow(dbase)){
      statut <- as.name(ifelse(is_womenForm,'n12','k12'))
      consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
      
      result[["value"]] <- dbase %>% 
        filter(!!consentment==0) %>% #!is.na(!!consentment) & 
        select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
               starttime,submissiondate,survey_statut=!!statut,key) %>%
        mutate(comment="")%>%
        arrange(town,agentname,starttime,submissiondate) %>%
        labelled::to_factor()
      
      idlist <- str_sort(unique(result[[c("value","caseid")]]))
      result[["msg"]] <- c(sprintf("Les ménages suivants n'ont pas consenti à participer à l'enquête : %s. ",
                                   paste(idlist,collapse ="; ")),
                           "Bien vouloir documenter chaque cas dans la colonne << comment >>.")
    }
  }
  invisible(result)
}

# Verification de l'age du repondant, notamment qu'il n'est pas inferieur à 14 ans ou >a 90 ans
check.respondentAge <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                                supervisorList=NULL,townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  }else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList,
             !!consentment==1)
    
    # result <- list()
    if(nrow(dbase)){
      statut <- as.name(ifelse(is_womenForm,'n12','k12'))
      age <- as.name(ifelse(is_womenForm,'c13','b6'))
      
      result[["value"]] <- dbase %>% 
        filter(!!age<15 | !!age>90) %>% #is.na(!!age) |
        select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
               respondent_age=!!age,starttime,submissiondate,survey_statut=!!statut,key) %>%
        mutate(comment="")%>%
        arrange(town,agentname,starttime,submissiondate) %>%
        labelled::to_factor()
      
      idlist <- str_sort(unique(result[[c("value","caseid")]]))
      result[["msg"]] <- c(sprintf("Dans les ménages suivants : %s au moins un des conjoints à moins de 15 ans ou plus de 90 ans. ",
                                   paste(idlist,collapse ="; ")),
                           "Bien vouloir documenter chaque cas dans la colonne << comment >>. ",
                           "Notament pour les cas de moins de 15 ans.")
    }
  } 
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,
  #    statut,consentment,age,idlist)
}

# Verification de l'age au marriage du repondant,notamment s'il est inferieur a 15 ans
check.respondentAgeAtMarriage <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                                          supervisorList=NULL,townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  } else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    if(!is_womenForm){
      stop("[PFS] La base de données fournie n'est pas celle des femmes.")
    } else{
      start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                      ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
      end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                    ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
      if(is.null(agentList))
        agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
      
      if(is.null(supervisorList))
        supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
      
      if(is.null(townList))
        townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
      
      consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
      dbase <- data %>% 
        filter(survey_date>=start & survey_date<=end,
               agentname %in% agentList,
               supervisorname %in% supervisorList,
               id_town %in% townList,
               b4 %in% c(1,2,-96),
               b6a==1 | b6b==2,
               !!consentment==1)
      
      # result <- list()
      if(nrow(dbase)){
        result[["value"]] <- dbase %>% 
          filter(c15<15 | (!is.na(c17) & c17<15)) %>%
          select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
                 age_currentMarriage=c15, age_previousMarriage=c17,starttime,submissiondate,
                 survey_statut=n12,key) %>%
          mutate(comment="")%>%
          arrange(town,agentname,starttime,submissiondate) %>%
          labelled::to_factor()
        
        idlist <- str_sort(unique(result[[c("value","caseid")]]))
        result[["msg"]] <- c(sprintf("Dans les ménages suivants, au moins un des conjoints à moins de 15 ans lors de son entree en union,  : %s . ",
                                     paste(idlist,collapse ="; ")),
                             "Bien vouloir documenter chaque cas dans la colonne << comment >>.")
      }
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,consentment,idlist)
}

# Verifie si le/la repondante exerce au moins une activite
# Ce HFC est de peu d'interet ce d'autant plus que les raisons de la non-activite sont
# listes dans l'application..
check.ifActivityAvailabled <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                                       supervisorList=NULL,townList=NULL){
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  }else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList,
             !!consentment==1)
    
    # La verification ne porte que sur les personnes concernêes par la question...
    if(is_womenForm)
      dbase <- dbase %>% filter(b4 %in% c(1,2,-96), b6a==1 | b6b==2)
    
    # result <- list()
    if(nrow(dbase)){
      statut <- as.name(ifelse(is_womenForm,'n12','k12'))
      activity_recent <- as.name(ifelse(is_womenForm,'e7','c1'))
      activity_lastyear <- as.name(ifelse(is_womenForm,'e9','c3'))
      
      result[["value"]] <- dbase %>% 
        filter(!!activity_recent==0 & !!activity_lastyear==0) %>%
        select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
               activity_7days=!!activity_recent,activity_12months=!!activity_lastyear,
               starttime,submissiondate,survey_statut=!!statut,key) %>%
        mutate(comment="")%>%
        arrange(town,agentname,starttime,submissiondate) %>%
        labelled::to_factor()
      
      idlist <- str_sort(unique(result[[c("value","caseid")]]))
      result[["msg"]] <- c(sprintf("Dans les ménages suivants, au moins un des conjoints n'a pas travaillé : %s. ",
                                   paste(idlist,collapse ="; ")),
                           "Bien vouloir documenter chaque cas dans la colonne << comment >>. ")
    }
  }
  
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,
  #    statut,consentment,age,idlist)
}

# Verifie si le/la repondante a indique avoir une activite non listee
# Modalite autre que celles listées..
check.ifOtherActivity <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                                  supervisorList=NULL,townList=NULL){
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  }else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList,
             !!consentment==1)
    
    # La verification ne porte que sur les personnes concernêes par la question...
    if(is_womenForm){
      dbase <- dbase %>% filter(e7==1 | e9==1)
    }else{
      dbase <- dbase %>% filter(c1==1 | c3==1)
    }
    
    # result <- list()
    if(nrow(dbase)){
      statut <- as.name(ifelse(is_womenForm,'n12','k12'))
      principal_activity <- as.name(ifelse(is_womenForm,'e13','c7')) #e13/c7 %in% c(7,20)
      principal_activity2 <- as.name(ifelse(is_womenForm,'e13a','c7a'))
      secondary_activity <- as.name(ifelse(is_womenForm,'e21','c14')) #e21/c14 %in% c(7,20)
      secondary_activity2 <- as.name(ifelse(is_womenForm,'e21a','c14a'))
      
      result[["value"]] <- dbase %>% 
        filter(!!principal_activity %in% c(7,20) | !!secondary_activity %in% c(7,20)) %>%
        mutate(comment01="",comment02="") %>%
        select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
               other_principalActivity=!!principal_activity2,comment01,
               other_secondaryActivity=!!secondary_activity2,comment02,
               starttime,submissiondate,survey_statut=!!statut,key) %>%
        arrange(town,agentname,starttime,submissiondate) %>%
        labelled::to_factor()
      
      idlist <- str_sort(unique(result[[c("value","caseid")]]))
      result[["msg"]] <- c(sprintf("Dans les ménages suivants, au moins un des conjoints a une activité non listée : %s. ",
                                   paste(idlist,collapse ="; ")),
                           "Bien vouloir reclasser les activités parmi celle listée dans les colonnes << comment01 >> ou << comment02 >>.")
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,
  #    statut,consentment,age,idlist)
}

# Verification du montant du revenu des activites principales et secondaires.
check.incomeAmont <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                              supervisorList=NULL,townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  }else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList,
             !!consentment==1)
    
    # La verification ne porte que sur les personnes concernêes par la question...
    if(is_womenForm){
      dbase <- dbase %>% filter(e7==1 | e9==1)
    }else{
      dbase <- dbase %>% filter(c1==1 | c3==1)
    }
    
    # result <- list()
    if(nrow(dbase)){
      statut <- as.name(ifelse(is_womenForm,'n12','k12'))
      principal_income  <- as.name(ifelse(is_womenForm,'e18','c11')) #e18/c11 >200.000 |=0
      principal_income2 <- as.name(ifelse(is_womenForm,'e19','c12')) #e19/c12=5
      secondary_income <- as.name(ifelse(is_womenForm,'e25','c18')) #e25/c18 >200.000|=0
      secondary_income2 <- as.name(ifelse(is_womenForm,'e26','c19')) #e26/c19=5
      
      result[["value"]] <- dbase %>% 
        filter(!!principal_income==0 | !!principal_income>param$threshold_income | 
                 !!principal_income2==5 | !!secondary_income2==5 |
                 !!secondary_income==0 | !!secondary_income>param$threshold_income) %>%
        mutate(comment01="",comment02="") %>%
        select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
               !!principal_income,!!principal_income2,comment01,
               !!secondary_income,!!secondary_income2,comment02,
               starttime,submissiondate,survey_statut=!!statut,key) %>%
        arrange(town,agentname,starttime,submissiondate) %>%
        labelled::to_factor()
      
      idlist <- str_sort(unique(result[[c("value","caseid")]]))
      result[["msg"]] <- c(sprintf("Dans les ménages suivants, au moins un des conjoints a un revenu null ou > à 200.000FCFA : %s. ",
                                   paste(idlist,collapse ="; ")),
                           "Bien vouloir documenter dans les colonnes << comment01 >> ou << comment02 >>.")
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,
  #    statut,consentment,age,idlist)
}


# Verification du montant de l'epargne
check.savingsAmont <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                               supervisorList=NULL,townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  }else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList,
             !!consentment==1)
    
    # La verification ne porte que sur les personnes concernêes par la question...
    if(is_womenForm)
      dbase <- dbase %>% filter(b4 %in% c(1,2,-96), b6a==1 | b6b==2)
    
    # result <- list()
    if(nrow(dbase)){
      statut <- as.name(ifelse(is_womenForm,'n12','k12'))
      savings  <- as.name(ifelse(is_womenForm,'e32','c25')) #e32/c25 >200.000
      
      result[["value"]] <- dbase %>% 
        filter(!!savings>param$threshold_savings) %>%
        mutate(comment="") %>%
        select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
               savings_amount=!!savings,comment,starttime,submissiondate,survey_statut=!!statut,key) %>%
        arrange(town,agentname,starttime,submissiondate) %>%
        labelled::to_factor()
      
      idlist <- str_sort(unique(result[[c("value","caseid")]]))
      result[["msg"]] <- c(sprintf("Dans les ménages suivants, au moins un des conjoints a une épargne  > à 200.000FCFA : %s. ",
                                   paste(idlist,collapse ="; ")),
                           "Bien vouloir documenter dans la colonne << comment >>.")
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,
  #    statut,consentment,age,idlist)
}

# Verification du montant de la dette
check.debtAmont <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                            supervisorList=NULL,townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  }else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList,
             !!consentment==1)
    
    # La verification ne porte que sur les personnes concernêes par la question...
    if(is_womenForm)
      dbase <- dbase %>% filter(b4 %in% c(1,2,-96), b6a==1 | b6b==2)
    
    # result <- list()
    if(nrow(dbase)){
      statut <- as.name(ifelse(is_womenForm,'n12','k12'))
      debt  <- as.name(ifelse(is_womenForm,'e37','c30')) #e37/c30 >200.000
      
      result[["value"]] <- dbase %>% 
        filter(!!debt>param$threshold_debt) %>%
        mutate(comment="") %>%
        select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
               debt_amount=!!debt,comment,starttime,submissiondate,survey_statut=!!statut,key) %>%
        arrange(town,agentname,starttime,submissiondate) %>%
        labelled::to_factor()
      
      idlist <- str_sort(unique(result[[c("value","caseid")]]))
      result[["msg"]] <- c(sprintf("Dans les ménages suivants, au moins un des conjoints a une dette  > à 200.000FCFA : %s. ",
                                   paste(idlist,collapse ="; ")),
                           "Bien vouloir documenter dans la colonne << comment >>.")
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,
  #    statut,consentment,age,idlist)
}

# Verification de l'existence d'au moins un TMO (uniquement chez les femmes) et
# d'au plus 3 TMO
check.numberOfTMO <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                              supervisorList=NULL,townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  } else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    if(!is_womenForm){
      stop("[PFS] La base de données fournie n'est pas celle des femmes.")
    } else{
      start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                      ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
      end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                    ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
      if(is.null(agentList))
        agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
      
      if(is.null(supervisorList))
        supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
      
      if(is.null(townList))
        townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
      
      consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
      dbase <- data %>% 
        filter(survey_date>=start & survey_date<=end,
               agentname %in% agentList,
               supervisorname %in% supervisorList,
               id_town %in% townList,
               b4 %in% c(1,2,-96),
               b6a==1 | b6b==2,
               !!consentment==1)
      
      # result <- list()
      if(nrow(dbase)){
        result[["value"]] <- dbase %>% 
          filter(q1==0 | q1>3) %>%
          mutate(comment="") %>%
          select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
                 number_TM0=q1,comment,starttime,submissiondate,
                 survey_statut=n12,key) %>%
          arrange(town,agentname,starttime,submissiondate) %>%
          labelled::to_factor()
        
        idlist <- str_sort(unique(result[[c("value","caseid")]]))
        result[["msg"]] <- c(sprintf("Dans les ménages suivants, aucun TMO ou plus de 3 TMO ont été déclarés,  : %s . ",
                                     paste(idlist,collapse ="; ")),
                             "Bien vouloir documenter chaque cas dans la colonne << comment >>.")
      }
    }
  } 
  
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,consentment,idlist)
}

# Verification du taux de recours aux sauts
check.useOfJumps <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                             supervisorList=NULL,townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  }else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList,
             !!consentment==1)
    
    # La verification ne porte que sur les personnes concernêes par la question...
    if(is_womenForm)
      dbase <- dbase %>% filter(b4 %in% c(1,2,-96), b6a==1 | b6b==2)
    
    # result <- list()
    if(nrow(dbase)){
      statut <- as.name(ifelse(is_womenForm,'n12','k12'))
      result[["value"]] <- dbase %>% 
        filter(prop_saut>=0.5) %>%
        mutate(comment="",
               tx_skip=sprintf("%.01f%%",100*prop_saut)) %>%
        select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
               tx_skip,comment,starttime,submissiondate,survey_statut=!!statut,key) %>%
        arrange(town,agentname,starttime,submissiondate) %>%
        labelled::to_factor()
      
      idlist <- str_sort(unique(result[[c("value","caseid")]]))
      result[["msg"]] <- c(sprintf("Pour les questionnaires suivants, le taux de recours aux sauts/filtres est relativement important : %s. ",
                                   paste(idlist,collapse ="; ")),
                           "Bien vouloir attirer l'attention des agents et le cas échéant documentez dans la colonne << comment >>.")
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,
  #    statut,consentment,age,idlist)
}

# Verification du taux de recours à la modalite "autre"
check.useOfOther <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                             supervisorList=NULL,townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  }else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList,
             !!consentment==1)
    
    # La verification ne porte que sur les personnes concernêes par la question...
    if(is_womenForm)
      dbase <- dbase %>% filter(b4 %in% c(1,2,-96), b6a==1 | b6b==2)
    
    # result <- list()
    if(nrow(dbase)){
      statut <- as.name(ifelse(is_womenForm,'n12','k12'))
      result[["value"]] <- dbase %>% 
        filter(prop_autre>=0.5) %>%
        mutate(comment="",
               tx_other=sprintf("%.01f%%",100*prop_autre)) %>%
        select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
               tx_other,comment,starttime,submissiondate,survey_statut=!!statut,key) %>%
        arrange(town,agentname,starttime,submissiondate) %>%
        labelled::to_factor()
      
      idlist <- str_sort(unique(result[[c("value","caseid")]]))
      result[["msg"]] <- c(sprintf("Pour les questionnaires suivants, le taux de recours à la modalité << autre >> est relativement important : %s. ",
                                   paste(idlist,collapse ="; ")),
                           "Bien vouloir attirer l'attention des agents et le cas échéant documentez dans la colonne << comment >>.")
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,
  #    statut,consentment,age,idlist)
}

# Verification du taux de recours à la modalite "ne sait pas"
check.useOfDontKnow <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                             supervisorList=NULL,townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  }else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList,
             !!consentment==1)
    
    # La verification ne porte que sur les personnes concernêes par la question...
    if(is_womenForm)
      dbase <- dbase %>% filter(b4 %in% c(1,2,-96), b6a==1 | b6b==2)
    
    # result <- list()
    if(nrow(dbase)){
      statut <- as.name(ifelse(is_womenForm,'n12','k12'))
      result[["value"]] <- dbase %>% 
        filter(prop_nsp>=0.5) %>%
        mutate(comment="",
               tx_dnk=sprintf("%.01f%%",100*prop_nsp)) %>%
        select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
               tx_dnk,comment,starttime,submissiondate,survey_statut=!!statut,key) %>%
        arrange(town,agentname,starttime,submissiondate) %>%
        labelled::to_factor()
      
      idlist <- str_sort(unique(result[[c("value","caseid")]]))
      result[["msg"]] <- c(sprintf("Pour les questionnaires suivants, le taux de recours à la modalité << Ne sait pas >> est relativement important : %s. ",
                                   paste(idlist,collapse ="; ")),
                           "Bien vouloir attirer l'attention des agents et le cas échéant documentez dans la colonne << comment >>.")
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,
  #    statut,consentment,age,idlist)
}


# Verification du taux de recours à la modalite "ne veut pas repondre"
check.useOfDontAnswer <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                                supervisorList=NULL,townList=NULL){
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  }else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList,
             !!consentment==1)
    
    # La verification ne porte que sur les personnes concernêes par la question...
    if(is_womenForm)
      dbase <- dbase %>% filter(b4 %in% c(1,2,-96), b6a==1 | b6b==2)
    
    # result <- list()
    if(nrow(dbase)){
      statut <- as.name(ifelse(is_womenForm,'n12','k12'))
      result[["value"]] <- dbase %>% 
        filter(prop_nvpr>=0.5) %>%
        mutate(comment="",
               tx_dna=sprintf("%.01f%%",100*prop_nvpr)) %>%
        select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
               tx_dna,comment,starttime,submissiondate,survey_statut=!!statut,key) %>%
        arrange(town,agentname,starttime,submissiondate) %>%
        labelled::to_factor()
      
      idlist <- str_sort(unique(result[[c("value","caseid")]]))
      result[["msg"]] <- c(sprintf("Pour les questionnaires suivants, le taux de recours à la modalité << Ne veut pas répondre >> est relativement important : %s. ",
                                   paste(idlist,collapse ="; ")),
                           "Bien vouloir attirer l'attention des agents et le cas échéant documentez dans la colonne << comment >>.")
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,
  #    statut,consentment,age,idlist)
}


# Verification du nombre de competences socio-emotionnelles
# type doit avoir l'une de valeurs suivantes : "all","listening","regulation","negociation"
# Cette verification me semble de peu d'interet car il est possible qu'une personne 
# puisse n'avoir aucune competences socio-emoitionnelles...
check.numberOfEmotionalSkills <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                                          supervisorList=NULL,townList=NULL,type="all"){
  
  typeList <- c("all","listening","regulation","negociation")
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  }else{
    
    if(!(type[1] %in% typeList)){
      stop(sprintf("[PFS] Le type de competence à évaluer doit être l'une des valeurs suivantes : %s.",
                   paste(typeList,collapse = "; ")))
    }else{
      typeOfEmotionalSkill <- type[1]
      is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
      start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                      ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
      end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                    ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
      if(is.null(agentList))
        agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
      
      if(is.null(supervisorList))
        supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
      
      if(is.null(townList))
        townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
      
      consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
      dbase <- data %>% 
        filter(survey_date>=start & survey_date<=end,
               agentname %in% agentList,
               supervisorname %in% supervisorList,
               id_town %in% townList,
               !!consentment==1)
      
      # La verification ne porte que sur les personnes concernêes par la question...
      if(is_womenForm)
        dbase <- dbase %>% filter(b4 %in% c(1,2,-96), b6a==1 | b6b==2)
      
      # result <- list()
      if(nrow(dbase)){
        statut <- as.name(ifelse(is_womenForm,'n12','k12'))
        dbase$numberOfEmotionalSkills <- 0
        
        listeningSkills <- switch(ifelse(is_womenForm,"woman","man"),
                                  woman=sprintf("g1%s",letters[1:7]),
                                  man=sprintf("e1%s",letters[1:7]))
        regulationSkills <- switch(ifelse(is_womenForm,"woman","man"),
                                   woman=sprintf("g2%s",letters[1:9]),
                                   man=sprintf("e2%s",letters[1:9]))
        negociationSkills <- switch(ifelse(is_womenForm,"woman","man"),
                                    woman=sprintf("g3%s",letters[1:8]),
                                    man=sprintf("e3%s",letters[1:8]))
        
        emotionalSkills <- switch (typeOfEmotionalSkill,
                                   all=c(listeningSkills,regulationSkills,negociationSkills),
                                   listening=listeningSkills,
                                   regulation=regulationSkills,
                                   negociation=negociationSkills
        )
        
        skillNames <- switch (typeOfEmotionalSkill,
                              all = c("écoute","régulation","négociation"),
                              listening="écoute",
                              regulation="régulation",
                              negociation="négociation"
        )
        
        for(varname in emotionalSkills) 
          dbase$numberOfEmotionalSkills <- dbase$numberOfEmotionalSkills + 
          as.numeric(dbase[,varname] %in% c(4,5))
        
        result[["value"]] <- dbase %>% 
          filter(numberOfEmotionalSkills==0) %>%
          mutate(comment="") %>%
          select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
                 comment,starttime,submissiondate,survey_statut=!!statut,key) %>%
          arrange(town,agentname,starttime,submissiondate) %>%
          labelled::to_factor()
        
        idlist <- str_sort(unique(result[[c("value","caseid")]]))
        result[["msg"]] <- c(sprintf("Pour les questionnaires suivants, aucune compétence socio-émotionnelle (%s) n'a été déclarée : %s. ",
                                     paste(skillNames,collapse =","),paste(idlist,collapse ="; ")),
                             "Bien vouloir attirer l'attention des agents et le cas échéant documentez dans la colonne << comment >>.",
                             sprintf("Pour de plus amples informations, référez-vous aux questions suivantes des questionnaires : %s.",
                                     paste(str_to_title(emotionalSkills),collapse = ", "))
        )
      }
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,
  #    statut,consentment,idlist,type,typeList,skillNames,listeningSkills,regulationSkills,
  #    negociationSkills,emotionalSkills,varname,typeOfEmotionalSkill)
}

# Verification du temps passé dans les activities quotidienne
# prepareMeal,cleanHouse, babysit, work,entertain, all
check.timeInDailyActivities <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                                        supervisorList=NULL,townList=NULL,dailyActivitity="all"){
  
  activities <- c("all","prepareMeal","cleanHouse", "babysit", "work","entertain")
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  }else{
    
    if(!(dailyActivitity[1] %in% activities)){
      stop(sprintf("[PFS] L'activité à évaluer doit être l'une des valeurs suivantes : %s.",
                   paste(activities,collapse = "; ")))
    }else{
      activity <- dailyActivitity[1]
      is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
      start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                      ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
      end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                    ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
      if(is.null(agentList))
        agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
      
      if(is.null(supervisorList))
        supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
      
      if(is.null(townList))
        townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
      
      consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
      dbase <- data %>% 
        filter(survey_date>=start & survey_date<=end,
               agentname %in% agentList,
               supervisorname %in% supervisorList,
               id_town %in% townList,
               !!consentment==1)
      
      # La verification ne porte que sur les personnes concernêes par la question...
      if(is_womenForm)
        dbase <- dbase %>% filter(b4 %in% c(1,2,-96), b6a==1 | b6b==2)
      
      # result <- list()
      if(nrow(dbase)){
        statut <- as.name(ifelse(is_womenForm,'n12','k12'))
        #"prepareMeal","cleanHouse", "babysit", "work","entertain"
        time_prepareMeal <- switch(ifelse(is_womenForm,"woman","man"),woman="h12v",man="f12v")
        time_cleanHouse <- switch(ifelse(is_womenForm,"woman","man"),woman="h13v",man="f13v")
        time_babysit <- switch(ifelse(is_womenForm,"woman","man"),woman="h14v",man="f14v")
        time_work <- switch(ifelse(is_womenForm,"woman","man"),woman="h15v",man="f15v")
        time_entertain <- switch(ifelse(is_womenForm,"woman","man"),woman="h16v",man="f16v")
        
        vartime <- switch (activity,
                           all=c(time_prepareMeal,time_cleanHouse,time_babysit,time_work,time_entertain),
                           prepareMeal=time_prepareMeal,
                           cleanHouse=time_cleanHouse,
                           babysit=time_babysit,
                           work=time_work,
                           entertain=time_entertain)
        
        dailyActivitiesNames <- switch (activity,
                                        all = "préparer le repas, nettoyer la maison, garder les enfants, travailler et se divertir/s'amuser",
                                        prepareMeal="préparer le repas",
                                        regulation="nettoyer la maison",
                                        babysit="garder les enfants",
                                        work="travailler",
                                        entertain="se divertir/s'amuser")
        dbase$error <- 0
        if(activity=="all"){
          for(varname in vartime) dbase$error <- dbase$error + dbase[[varname]]
          dbase$error <- as.numeric(dbase$error>param$threshold_timeInDailyActivities)
        }else{
          dbase$error <- as.numeric(dbase[[vartime]]>param$threshold_timeInDailyActivity)
        }
        
        #conversion en minutes..
        for(varname in vartime) dbase[varname] <- sprintf("%d minutes",dbase[[varname]])
        
        result[["value"]] <- dbase %>% 
          filter(error==1) %>%
          mutate(comment="") %>%
          select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
                 dplyr::one_of(vartime),comment,starttime,submissiondate,survey_statut=!!statut,key) %>%
          arrange(town,agentname,starttime,submissiondate) %>%
          labelled::to_factor()
        
        idlist <- str_sort(unique(result[[c("value","caseid")]]))
        result[["msg"]] <- c(sprintf("Pour les questionnaires suivants, beaucoup de temps passé à %s : %s. ",
                                     dailyActivitiesNames,paste(idlist,collapse ="; ")),
                             "Bien vouloir attirer l'attention des agents et le cas échéant documentez dans la colonne << comment >>.",
                             sprintf("Pour de plus amples informations, référez-vous aux questions suivantes des questionnaires : %s.",
                                     paste(str_to_title(vartime),collapse = ", "))
        )
      }
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,
  #    statut,consentment,idlist,dailyActivitity,activities,activity,time_prepareMeal,time_cleanHouse,
  #    time_babysit,time_work,time_entertain,vartime,dailyActivitiesNames,varname)
}


#Verification de la disponibilité d'au moins un support communautaire
#ce controle nous semble de peu d'intérêt car sous-entend l'existence d'une obligation morale
# ou du loi/ordonnance contraignant les individus d'une communauté à se soutenir.
check.availabilityOfCommunitySupport <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                                                 supervisorList=NULL,townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  }else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList,
             !!consentment==1)
    
    # La verification ne porte que sur les personnes concernêes par la question...
    if(is_womenForm)
      dbase <- dbase %>% filter(b4 %in% c(1,2,-96), b6a==1 | b6b==2)
    
    # result <- list()
    if(nrow(dbase)){
      statut <- as.name(ifelse(is_womenForm,'n12','k12'))
      
      dbase$numberOfCommunitySupport <- 0
      varlist <- switch(ifelse(is_womenForm,"woman","man"),
                        woman=sprintf("m1%s",letters[1:6]),
                        man=sprintf("j1%s",letters[1:6]))
      
      
      for(varname in varlist) dbase$numberOfCommunitySupport <- dbase$numberOfCommunitySupport +
        as.numeric(dbase[[varname]] %in% c(4,5))
      
      result[["value"]] <- dbase %>% 
        filter(numberOfCommunitySupport==0) %>%
        mutate(comment="") %>%
        select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
               dplyr::one_of(varlist),comment,starttime,submissiondate,survey_statut=!!statut,key) %>%
        arrange(town,agentname,starttime,submissiondate) %>%
        labelled::to_factor()
      
      idlist <- str_sort(unique(result[[c("value","caseid")]]))
      result[["msg"]] <- c(sprintf("Pour les questionnaires suivants, aucun support communautaire n'a été déclaré: %s. ",
                                   paste(idlist,collapse ="; ")),
                           "Bien vouloir attirer l'attention des agents et le cas échéant documentez dans la colonne << comment >>.",
                           sprintf("Pour de plus amples informations, référez-vous aux questions suivantes des questionnaires : %s.",
                                   paste(str_to_title(varlist),collapse = ", "))
      )
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,
  #    statut,consentment,idlist,varname,varlist)
}

# Indentification des formulaires/questionnaires incomplets/ineligibles
# Ce controle n'est pas tres utile... mais pas soucis de conformite avec
# ce qui est fait du cote du consultant GIL/BM, nous gardons ce controle...
check.statutOfForms <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                                  supervisorList=NULL,townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  }else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList)
    
    # result <- list()
    if(nrow(dbase)){
      statut <- as.name(ifelse(is_womenForm,'n12','k12'))
      result[["value"]] <- dbase %>% 
        filter(!!statut!=2) %>%
        mutate(comment="") %>%
        select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
               !!statut,comment,starttime,submissiondate,key) %>%
        arrange(town,agentname,starttime,submissiondate) %>%
        labelled::to_factor()
      
      idlist <- str_sort(unique(result[[c("value","caseid")]]))
      result[["msg"]] <- c(sprintf("Les questionnaires suivants sont incomplets ou sont associés à des ménages inéligibles : %s. ",
                                   paste(idlist,collapse ="; ")),
                           "Bien vouloir attirer l'attention des agents et le cas échéant documentez dans la colonne << comment >>.")
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,
  #    statut,consentment,idlist)
}


# Verification du nombre d'epouse... 4 est le minimun dans la religion mulsuman
# et la majorite des beneficiaires sont des peuls..
check.numberOfSpouses <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                                          supervisorList=NULL,townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  } else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    if(!is_womenForm){
      stop("[PFS] La base de données fournie n'est pas celle des femmes.")
    } else{
      start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                      ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
      end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                    ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
      if(is.null(agentList))
        agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
      
      if(is.null(supervisorList))
        supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
      
      if(is.null(townList))
        townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
      
      consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
      dbase <- data %>% 
        filter(survey_date>=start & survey_date<=end,
               agentname %in% agentList,
               supervisorname %in% supervisorList,
               id_town %in% townList,
               b4 %in% c(1,2,-96),
               b6a==1 | b6b==2,
               !!consentment==1)
      
      # result <- list()
      if(nrow(dbase)){
        result[["value"]] <- dbase %>% 
          filter(c19==1,c20>param$threshold_numberOfSpouse) %>%
          mutate(comment="") %>%
          select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
                 numberOfSpouse=c20,comment,starttime,submissiondate,survey_statut=n12,key) %>%
          arrange(town,agentname,starttime,submissiondate) %>%
          labelled::to_factor()
        
        idlist <- str_sort(unique(result[[c("value","caseid")]]))
        result[["msg"]] <- c(sprintf("Dans les ménages suivants, le conjoint à au moins 4 epouses : %s. ",
                                     paste(idlist,collapse ="; ")),
                             "Bien vouloir documenter chaque cas dans la colonne << comment >>.")
      }
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,consentment,idlist)
}


#check.existenceOfIntimatePartnerViolence
#VPI => varlist <- sprintf("j%d",c(8:13,15,17,19,23,25,27,31,33,35,37,39,41,43,47,49,51))
#Ce HFC est de peu d'interêt car concerne la section ACASI qui est chiffree. Par ailleurs, l'auteur
#des reponses est directement la/le repondant et le questionnaire dans sa section K envisage 
#bien la possibilite de ne point avoir de violence entre partenaire intime.
# Pour toutes ces raisons dont la premiere, aucun programme de verification ne sera fait pour le 
# moment sur ce cas..
check.existenceOfIntimatePartnerViolence <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                                                     supervisorList=NULL,townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  } else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    if(!is_womenForm){
      stop("[PFS] La base de données fournie n'est pas celle des femmes.")
    } else{
      start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                      ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
      end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                    ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
      if(is.null(agentList))
        agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
      
      if(is.null(supervisorList))
        supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
      
      if(is.null(townList))
        townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
      
      consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
      dbase <- data %>% 
        filter(survey_date>=start & survey_date<=end,
               agentname %in% agentList,
               supervisorname %in% supervisorList,
               id_town %in% townList,
               b4 %in% c(1,2,-96),
               b6a==1 | b6b==2,
               !!consentment==1)
      
      if(nrow(dbase)){
        
        varlist <- sprintf("j%d",c(8:13,15,17,19,23,25,27,31,33,35,37,39,41,43,47,49,51))
        dbase$numberOfVPI <- 0
        
        for(varname in varlist){
          dbase[varname]  <- as.numeric(dbase[[varname]])
          dbase[which(is.na(dbase[,varname])),varname] <- 0
          dbase$numberOfVPI <- dbase$numberOfVPI + as.numeric(dbase[,varname]==1)
        } 
        
        # Du fait de la confidentialite des reponses, aucune valeur relatif au violence
        # ne sera affiche
        result[["value"]] <- dbase %>% 
          filter(numberOfVPI==0) %>%
          select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
                 starttime,submissiondate,survey_statut=n12,key) %>%
          arrange(town,agentname,starttime,submissiondate) %>%
          labelled::to_factor()
        
        idlist <- str_sort(unique(result[[c("value","caseid")]]))
        agentNames <-  str_sort(unique(result[[c("value","agentname")]]))
        
        result[["msg"]] <- c(sprintf("Dans les ménages suivants, la repondante n'a declaré aucune violence sexuelle entre partenaire intime : %s. ",
                                     paste(idlist,collapse ="; ")),
                             sprintf("\n\nBien vouloir attirer l'attention des agents suivants : %s. ",
                                     paste(agentNames,collapse ="; ")),
                             "\n\nDemandez leur de sensibiliser un peu plus les répondantes sur la confidentialité de leurs réponses.",
                             sprintf("Notamment en ce qui concerne les violences associées aux questions suivantes : %s",
                                     paste(str_to_title(varlist),collapse = ", ")))
      }
    }
  }
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,consentment,idlist,
  #    varlist,varname)
  invisible(result)
}

# Verifie que le menage a bien des biens, liste les menages qui n'en ont pas.
# si propertyWife = TRUE alors le programme retournera l'ensemble des menages ou la femme
# ne possede aucun bien dans le menage, etc...
# type indique le type de biens de possessions que l'on souhaite verifie les articles (goods) ou
# le cheptel (livestock) ou tout bien confondu
check.householdPossessions <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,supervisorList=NULL,
                                       townList=NULL,propertyWife=FALSE,type="all"){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  typeList <- c("all","goods","livestock")
  hh_goodsNames <- c("matelas","bicyclettes","motos","portables","radios","télévisions","moustiquaires"
                  , "seaux","boeufs","moutons_chevres","porcs","volaile","lapins","cobayes")
  names(hh_goodsNames) <- c("d1a","d1b","d1c","d1d","d1e","d1f","d1g","d1h","d3a","d3b","d3c","d3d","d3e","d3f")
  
  w_goodsNames <- sprintf("F_%s",hh_goodsNames)
  names(w_goodsNames) <- str_replace_all(str_replace_all(names(hh_goodsNames),"1","2"),"3","4")
  goodsNames <- c(hh_goodsNames,w_goodsNames)
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  } else{
    if(!is.logical(propertyWife) | any(!(type %in% typeList)) ){
      stop(sprintf("[PFS] Le parametre propertyWife doit etre un booleen et celui <type> un des parametres suivants : %s.",
                   paste(typeList,collapse = ", ")))
    }else{
      is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
      if(!is_womenForm){
        stop("[PFS] La base de données fournie n'est pas celle des femmes.")
      } else{
        start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                        ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
        end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                      ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
        if(is.null(agentList))
          agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
        
        if(is.null(supervisorList))
          supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
        
        if(is.null(townList))
          townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
        
        consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
        dbase <- data %>% 
          filter(survey_date>=start & survey_date<=end,
                 agentname %in% agentList,
                 supervisorname %in% supervisorList,
                 id_town %in% townList,
                 b4 %in% c(1,2,-96),
                 b6a==1 | b6b==2,
                 !!consentment==1)
        
        typeValue <- type[1]
        varlist <- switch(typeValue,
                          goods=switch(ifelse(propertyWife,"woman","everybody"),
                                       woman=sprintf("d2%s",letters[1:8]),
                                       everybody=sprintf("d1%s",letters[1:8])),
                          livestock=switch(ifelse(propertyWife,"woman","everybody"),
                                           woman=sprintf("d4%s",letters[1:6]),
                                           everybody=sprintf("d3%s",letters[1:6])),
                          all=switch(ifelse(propertyWife,"woman","everybody"),
                                     woman=c(sprintf("d2%s",letters[1:8]),
                                             sprintf("d4%s",letters[1:6])),
                                     everybody=c(sprintf("d1%s",letters[1:8]),
                                                 sprintf("d3%s",letters[1:6]))
                                     )
                          )
        
        
        if(propertyWife){
          sublist <- ifelse(str_detect(varlist,"d2"),
                            str_replace_all(varlist,"2","1"),
                            str_replace_all(varlist,"4","3"))
          dbase <- dbase %>% drop_na(any_of(sublist))
          # for(varname in sublist){
          #   elt <- as.name(varname)
          #   dbase <- dbase %>% filter(!!elt>0)
          # } 
        }
        
        if(nrow(dbase)){
          dbase$women_isOwner <- FALSE
          for(varname in varlist){
            check.cond <- FALSE 
            if(str_detect(varname,"d1|d3")){
              #check.cond = 0 : MENAGE NE POSSEDE PAS LE BIEN
              #check.cond = 1 : MENAGE POSSEDE LE BIEN
              check.cond <- as.numeric(dbase[,varname]>0)
              # labs <- c("Menage-non proprietaire","Menage proprietaire")
              # dbase[,sprintf("%s_check",varname)] <- factor(labs[check.cond+1],levels = labs)
            }else{
              subvarname <- ifelse(str_detect(varname,"d2"),
                                   str_replace_all(varname,"2","1"),
                                   str_replace_all(varname,"4","3"))
              #check.cond = 0 : FEMME NE POSSEDE PAS LE BIEN ALORS QU'IL EST DISPONIBLE DANS LE MENAGE
              #check.cond = 1 : FEMME POSSEDE LE BIEN DISPONIBLE DANS LE MENAGE
              #check.cond = 2 : NON CONCERNE - CAR LE MENAGE NE POSSEDE PAS LE BIEN OU QUE LE REPONDANT N'A PAS VOULU REPONDRE
              check.cond <- ifelse(is.na(dbase[,subvarname]) | dbase[,subvarname]==0 | 
                                     dbase[,subvarname]==-98 | dbase[,subvarname]==-99,
                                   2,as.numeric(dbase[,varname]>0))
              # labs <- c("Femme-non proprietaire","Femme proprietaire","Non concerne")
              # dbase[,sprintf("%s_check",varname)] <- factor(labs[check.cond+1],levels =labs )
            }
            dbase$women_isOwner<- dbase$women_isOwner | check.cond==1
            dbase[,goodsNames[varname]] <- dbase[,varname]
          }
          
          result[["value"]] <- dbase %>% 
            filter(!women_isOwner) %>%
            mutate(comment="") %>%
            select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
                   dplyr::one_of(goodsNames[varlist]),comment,starttime,submissiondate,survey_statut=n12,key) %>%
            arrange(town,agentname,starttime,submissiondate) %>%
            labelled::to_factor()
          
          idlist <- str_sort(unique(result[[c("value","caseid")]]))
          result[["msg"]] <- c(sprintf("Dans les ménages suivants, %s n'a aucun %s : %s. ",
                                       ifelse(propertyWife,"la femme","le ménage"),
                                       ifelse(type=="livesotck","bétail","bien"),
                                       paste(idlist,collapse ="; ")),
                               "Bien vouloir documenter chaque cas dans la colonne << comment >>.",
                               sprintf("Pour de plus amples détails bien vouloir vous référez aux questions suivantes : %s",
                                       paste(str_to_title(varlist),collapse = "; ")),
                               sprintf("La valeur << NA >> indique que %s n'est pas concerné%s par la possession du %s listé en colonne.",
                                       ifelse(propertyWife,"la femme","le ménage"),ifelse(propertyWife,"e",""),
                                       ifelse(type=="livesotck","bétail","bien"))
          )
        }
      }
    } 
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,consentment,idlist,
  #    varname,varlist,check.cond,typeValue,typeList,propertyWife,sublist,subvarname,type,goodsNames,
  #    hh_goodsNames,w_goodsNames)
}

# Verifie si le menage exploite des terres pour l'agriculture ou l'élevage
check.landUse <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                          supervisorList=NULL,townList=NULL){
  
  result <- list()
  result[["value"]] <- result[["msg"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  } else{
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    if(!is_womenForm){
      stop("[PFS] La base de données fournie n'est pas celle des femmes.")
    } else{
      start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                      ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
      end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                    ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
      if(is.null(agentList))
        agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
      
      if(is.null(supervisorList))
        supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
      
      if(is.null(townList))
        townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
      
      consentment <- as.name(ifelse(is_womenForm,"a9c","a11"))
      dbase <- data %>% 
        filter(survey_date>=start & survey_date<=end,
               agentname %in% agentList,
               supervisorname %in% supervisorList,
               id_town %in% townList,
               b4 %in% c(1,2,-96),
               b6a==1 | b6b==2,
               !!consentment==1)
      
      if(nrow(dbase)){
        result[["value"]] <- dbase %>% 
          filter(e1!=1) %>%
          mutate(comment="") %>%
          select(town,supervisorname,agentname,caseid,pfs_info=pfs_hoh_name,respondent_name,
                 landUse=e1,comment,starttime,submissiondate,survey_statut=n12,key) %>%
          arrange(town,agentname,starttime,submissiondate) %>%
          labelled::to_factor()
        
        idlist <- str_sort(unique(result[[c("value","caseid")]]))
        result[["msg"]] <- c(sprintf("Les ménages suivants n'exploitent pas de terres pour l'agriculture ou l'élevage : %s. ",
                                     paste(idlist,collapse ="; ")),
                             "Bien vouloir documenter chaque cas dans la colonne << comment >>.")
      }
    }
  }
  invisible(result)
  # rm(startdate,enddate,agentList,supervisorList,townList,is_womenForm,data,start,end,consentment,idlist)
}

# Obtenir la liste des identifiants des questionnaires avec de potentielles incoherences à documenter
getIDFormWithPotentialInconsistencies <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,
                                                  supervisorList=NULL,townList=NULL){ 
  result <- list()
  result[["msg"]] <- result[["value"]] <- result[["dataset"]] <- NULL 
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  } else {
    is_womenForm <- str_sub(data$caseid[1],start = 1,end = 1L)=='F'
    
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    if(is.null(agentList))
      agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
    
    if(is.null(supervisorList))
      supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
    
    if(is.null(townList))
      townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
    
    
    data <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList)
    
    statut <- as.name(ifelse(is_womenForm,'n12','k12'))
    
    #dput(grep("check",ls(),value=T))
    fn_List <- c("check.availabilityOfCommunitySupport", "check.consentment",
                 "check.debtAmont", "check.duplicatedID", "check.existenceOfIntimatePartnerViolence", 
                  "check.householdPossessions", "check.ifActivityAvailabled", "check.ifOtherActivity", 
                  "check.incomeAmont", "check.landUse", "check.numberOfEmotionalSkills", 
                  "check.numberOfSpouses", "check.numberOfTMO", "check.respondentAge", 
                  "check.respondentAgeAtMarriage", "check.savingsAmont", "check.statutOfForms", 
                  "check.timeInDailyActivities", "check.useOfDontAnswer", "check.useOfDontKnow", 
                  "check.useOfJumps", "check.useOfOther")
    
    fn_labs <- c("Potentielles incohérences sur la disponibilité d'un support communautaire", 
                  "Potentielles incohérences sur le consentement à participer à l'étude",
                  "Potentielles incohérences sur le montant de la dette",
                  "Liste de doublons parmi les identifiants",
                  "Potentielles incohérences dans le report des violences entre partenaires intimes",
                  "Potentielles incohérences dans le report des biens de possessions",
                  "Potentielles incohérences sur les activités exercées",
                  "Activités exercées non listées",
                  "Potentielles incohérences sur le montant du revenu",
                  "Potentielles incohérences dans le report de la possession de terrain",
                  "Potentielles incohérences dans le report des compétences socio-émotionnelles",
                  "Potentielles incohérences dans le report du nombre d'épouses",
                  "Potentielles incohérences dans le report du nombre de TMO perçu",
                  "Potentielles incohérences dans le report de l'âge du/de la répondant(e)",
                  "Potentielles incohérences dans le report de l'âge au mariage",
                  "Potentielles incohérences sur le montant de l'épargne",
                  "Liste des questionnaires incomplets/inéligibles", 
                  "Potentielles incohérences dans le report des temps passés dans certaines activités du quotidien",
                  "Liste des questionnaires avec un recours élevé à la modalité << Ne veut pas répondre >>",
                  "Liste des questionnaires avec un recours élevé à la modalité << Ne sait pas >>",
                  "Liste des questionnaires avec un recours élevé aux sauts/filtres",
                  "Liste des questionnaires avec un recours élevé à la modalité << Autre >>")
    names(fn_labs) <- fn_List
    
    w_fn <- c("check.existenceOfIntimatePartnerViolence","check.numberOfSpouses","check.numberOfTMO",
              "check.respondentAgeAtMarriage","check.householdPossessions","check.landUse")
    
    idlist <- ""
    
    nb_form <-nrow(data)
    result[["value"]] <- tibble_row(lab="",value="") %>% slice(-1L) #initialisation du dataframe
    
    for(fn_name in fn_List){
      
      if(fn_name %in% w_fn & !is_womenForm) next
      
      tmp <- do.call(fn_name,list(data, startdate,enddate,agentList,supervisorList,townList),
                     envir = .GlobalEnv)
        
      if(!is.null(tmp$value) && nrow(tmp$value)){
        idlist <- str_sort(unique(c(idlist,str_sort(unique(tmp[[c("value","caseid")]])))))
        result[["value"]] <- result[["value"]] %>%
          add_row(tibble_row(lab=sprintf("%s : ",fn_labs[fn_name]),
                             value=sprintf("%d questionnaires concernés par cette potentielle incohérence",
                                           length(idlist))))
      } 
        
    }
    
    # result[["msg"]] <- sprintf("%d questionnaires, dont les identifiants sont les suivants, ont de potentielles incohérences : %s. ",
    #                              length(idlist),paste(idlist,collapse ="; "))
    
    nb_inconsistencies <- length(idlist)
    result[["msg"]] <- sprintf("%d questionnaires sur %d contiennent de potentielles incohérences. Soit %.02f%% de données potentiellement incohérentes.",
                               nb_inconsistencies,nb_form,100*nb_inconsistencies/nb_form)
    
    result[["dataset"]] <- data %>% 
      filter(caseid %in% unique(idlist)) %>% 
      select(caseid,town,supervisorname,agentname,pfs_info=pfs_hoh_name,respondent_name,
             starttime,endtime,submissiondate,survey_statut=!!statut,key) %>%
      arrange(caseid,starttime,endtime,submissiondate) %>%
      labelled::to_factor()
  }
  #if(dim(result$value)[1]) print(result$msg)
  # rm(enddate,startdate,data,start,end,dbase,result,tmp,statut,agentList,supervisorList,townList,is_womenForm,
  #    fn_List,fn_name,tmp,idlist,fn)
  invisible(result)
}
