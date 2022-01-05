# Reporting statistics
# Charles M. <charles.moute@gmail.com>
# version 210831-1430


#----------------------------------------------------------------------------------------------->
# Fonctions utilitaires
#----------------------------------------------------------------------------------------------->
#vignette("ggplot2-specs")

# Obtient un listing des effectifs des questionnaires attendus,soumis, complet et des doublouns
# ainsi que le taux de couverture theorique dans une ou plusieurs communes...
getTableStatistics <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,supervisorList=NULL,
                               townList=NULL){
 
  result <- NULL
  
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
      townList <- unique(pluck(data,"id_town")) #townList <- unique(pluck(data,"town")) #unique(data[["town"]])
    
    # Nombre de questionnaire soumis
    dbase <- data %>% 
      filter(survey_date>=as_date(start) & survey_date<=as_date(end),
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList)
    
    agentList <- unique(dbase[["agentname"]])
    supervisorList <- unique(dbase[["supervisorname"]])
    townList <- unique(dbase[["id_town"]])
    start <- min(dbase[["survey_date"]])
    end <- max(dbase[["survey_date"]])
    statut <- as.name(ifelse(is_womenForm,'n12','k12'))
    
    #Nombre de questionnaires soumis
    result <- dbase %>% arrange(id_town) %>% group_by(id_town) %>% mutate(Soumis=n()) %>% 
      ungroup() %>% select(id_town,Soumis) %>% distinct() %>% arrange(id_town)
    
    #Ajout du nombre de questionnaires complet sans double compte (Conplet0) et 
    # avec double compte (Complet)
    tmp <- dbase %>% arrange(id_town) %>%
      filter(!!statut==2) %>% group_by(id_town) %>% mutate(Complet=n()) %>% ungroup() %>%
      group_by(caseid) %>% slice(1L) %>% # on s'assure de supprimer les doublons
      ungroup() %>% group_by(id_town) %>% mutate(Complet0=n()) %>% ungroup() %>% 
      select(id_town,Complet,Complet0) %>% distinct() %>% arrange(id_town)
    result <- left_join(result,tmp,by = "id_town")
    
    # Ajout du nombre de questionnaires cibles par village ....
    result <- left_join(result,
                      target %>% 
                        select(id_town,Village=town,Cible=nbhh),
                      by = "id_town")
    
    #Ajout du nombres de doublon effectues dans chaque village..
    idtmp <- str_sort(dbase[["caseid"]][which(duplicated(dbase[["caseid"]]))])
    tmp <- dbase %>% 
      filter(caseid %in% idtmp) %>%
      group_by(caseid) %>%
      slice(-1L) %>% # suppression d'un des doublons
      ungroup() %>% group_by(id_town) %>%
      summarise(Doublon=n())
    
    result <- left_join(result,tmp,by = "id_town") %>%
      replace_na(list(Doublon=0))
    
    # Ajout du nombre de questionnaire inéligible, du taux de couverture, ainsi que des
    # des noms des villages et des communes...
    result <- left_join(result,deployment %>% select(id_town,Commune=council),by="id_town") %>%
      arrange(Commune,Village) %>%
      mutate(`Inéligible/Incomplet`= Soumis - Complet,
             Couverture=sprintf("%.1f%%",100*Complet0/Cible)) %>%
      select(Commune,id_town,Village,Soumis,Doublon,`Inéligible/Incomplet`,Complet,Complet0,Cible,Couverture)
    # pour le rapport on retire id_town & Complet... Complet0 sera renome en Complet..
    
    result$Comment <- ""
    rowid <- which(result$Complet0>result$Cible)
    if(length(rowid))
      result$Comment[rowid] <- paste0(c("Possible incoherence dans le report des statuts d'enquête.",
                                      "Un ou plusieurs questionnaires incomplet(s)/inéligible(s) marqué(s) à tort complet(s)"),
                                    collapse = " ")
    rowid <- which(result$Cible==0)
    if(length(rowid))
      result$Comment[rowid] <- "Village constitué exclusivement de ménages de remplacement"
   
    # idlist <- setdiff(unique((deployment %>% filter(supervisor_name %in% supervisorList))$id_town)
    #                   ,unique(result$id_town))
    
    dtmp <- deployment %>% filter(supervisor_name %in% supervisorList)
    idlist <- dtmp$id_town[!dtmp$id_town %in% townList]
    # idlist <- dtmp$id_town[!dbtmp$id_town %in% unique(result$id_town)]
    
    for(elt in idlist){
      infos <- deployment %>% filter(id_town==elt) %>% select(Commune=council,id_town,Village=town)
      val_cible <- (target %>% filter(id_town==elt))$nbhh
      result <- result %>% 
        add_row(tibble_row(Commune=infos$Commune,id_town=elt,Village=infos$Village,Soumis=0,Doublon=0,
                           `Inéligible/Incomplet`=0,Complet=0,Complet0=0,Cible=val_cible[1],Couverture="0.0%",
                           Comment="Données inexistantes ou indisponibles sur le serveur"))
    }
     
  }
  invisible(result)
}

# Listing de quelques statistiques utiles ...
getGlobalStatistics <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,supervisorList=NULL,
                                townList=NULL,include.inconsistencies=FALSE){
  result <- NULL
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
    
  } else {
    
    target_survey <- NULL
    if(is.null(agentList) & is.null(supervisorList) & is.null(townList))
      target_survey <- target$nbhh[which(target$town=="TOTAL")]
    
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
      townList <- unique(pluck(data,"id_town")) #townList <- unique(pluck(data,"town")) #unique(data[["town"]])
    
    # Nombre de questionnaire soumis
    dbase <- data %>% 
      filter(survey_date>=start & survey_date<=end,
             agentname %in% agentList,
             supervisorname %in% supervisorList,
             id_town %in% townList)
    
    agentList <- unique(dbase[["agentname"]])
    supervisorList <- unique(dbase[["supervisorname"]])
    townList <- unique(dbase[["id_town"]])
    start <- min(dbase[["survey_date"]])
    end <- max(dbase[["survey_date"]])
    
    if(nrow(dbase)){
      nb_submittedForm <- nrow(dbase)
      number_surveydate <- as.numeric(end-start)
      if(is.null(target_survey))
        target_survey <- (target %>% filter(id_town %in% townList) %>% summarise(eff=sum(nbhh)))[["eff"]]
      
      tmp <- staff %>% 
        filter(agent %in% agentList,
               supervisor_name %in% supervisorList) %>% 
        group_by(sex) %>% 
        summarise(eff=n())
      
      tmp_man <- (tmp %>% filter(sex=="Homme"))[["eff"]]
      tmp_woman <- (tmp %>% filter(sex=="Femme"))[["eff"]]
      target_todate <- number_surveydate*param$threshold_manByDay*ifelse(length(tmp_man),tmp_man,0) +  
        number_surveydate*param$threshold_womanByDay*ifelse(length(tmp_woman),tmp_woman,0)
      
      # cat("[getGlobalStatistics] \n",
      #     sprintf("%d questionnaires soumis / %d questionnaires complets attendus (%.01f%%)",
      #             nb_submittedForm,target_survey,100*nb_submittedForm/target_survey))
      
      
      result <- tibble_row(label="Taux de couverture global :",
                           value= sprintf("%d questionnaires soumis / %d questionnaires complets attendus (%.01f%%)",
                                          nb_submittedForm,target_survey,100*nb_submittedForm/target_survey))
      
      result <- result %>% 
        add_row(tibble_row(label= sprintf("Taux de couverture du %s au %s :",as_date(start),as_date(end)),
                           value= sprintf("%d questionnaires soumis / %d questionnaires complets attendus à date (%.01f%%)",
                                          nb_submittedForm,target_todate,100*nb_submittedForm/target_todate)))
      
      statut <- as.name(ifelse(is_womenForm,'n12','k12'))
      nb_completedForm <- nrow(dbase %>% filter(!!statut==2))
      result <- result %>% 
        add_row(tibble_row(label= "Nombre de questionnaires complets : ",
                           value= sprintf("%d questionnaires complets / %d questionnaires soumis (%.01f%%)",
                                          nb_completedForm,nb_submittedForm,100*nb_completedForm/nb_submittedForm)))
      
      # ici les doublons sont inclus dans le compte pour éviter de biaiser les statistiques
      # si l'on souhaite les extraire on peut utiliser unique a ce niveau ou inclure la liste
      # des identifiants des doublons a exclure.. et utiliser plutot la variable caseid qui
      # porte sur les individus..
      nb_primaryForm <- length((dbase %>% filter(!(caseid2 %in% idlist_replacement)))[["caseid2"]])
      result <- result %>% 
        add_row(tibble_row(label= "Nombre de questionnaires primaires : ",
                           value= sprintf("%d questionnaires primaires / %d questionnaires soumis (%.01f%%)",
                                          nb_primaryForm,nb_submittedForm,100*nb_primaryForm/nb_submittedForm)))
      
      # Questionnaire primaire complet soumis...
      nb_completedPrimaryForm <- length((dbase %>% filter(!(caseid2 %in% idlist_replacement),
                                                          !!statut==2))[["caseid2"]])
      result <- result %>% 
        add_row(tibble_row(label= "Nombre de questionnaires primaires complets : ",
                           value= sprintf("%d questionnaires primaires complets / %d questionnaires complets (%.01f%%)",
                                          nb_completedPrimaryForm,nb_completedForm,
                                          100*nb_completedPrimaryForm/nb_completedForm)))
      
      if(include.inconsistencies){
        #Nombre de questionnaire avec potentielles incoherences
        tmp <- getIDFormWithPotentialInconsistencies(dbase,startdate,enddate,agentList,
                                                     supervisorList,townList)
        nb_formWithInconsensistencies <- length(str_sort(unique(tmp$dataset[["caseid"]])))
        result <- result %>% 
          add_row(tibble_row(label= "Nombre de questionnaires avec de potentielles incohérences : ",
                             value= sprintf("%d questionnaires avec de potentielles incohérences / %d questionnaires soumis (%.01f%%)",
                                            nb_formWithInconsensistencies,nb_submittedForm,
                                            100*nb_formWithInconsensistencies/nb_submittedForm)))
      }
      
      # Nombre de jour de collecte évalué
      result <- result %>% 
        add_row(tibble_row(label= "Nombre de jours de collecte : ",value= sprintf("%d jours",end-start)))
      
      # Durée moyenne d'administration d'un questionnaire sur la période évalué
      result <- result %>% 
        add_row(tibble_row(label= "Durée moyenne d'administration d'un questionnaire : ",
                           value= sprintf("%s",
                                          str_sub(hms::as_hms(mean(data$duration, na.rm = TRUE)),
                                                  start=1,end=8))))
    }else{
      result <- tibble_row(label="",value="Statistiques non disponibles")
    }
  }
  # rm(enddate,startdate,data,start,end,dbase,result,tmp,statut,agentList,supervisorList,townList,is_womenForm)
  invisible(result)
}

# Liste des villages visitees et de ceux encore a visiter 
getVisitedTowns <- function(data,startdate=NULL,enddate=NULL,agentList=NULL,supervisorList=NULL){
  
  result <- NULL
  
  if(is.null(data)){
    stop("[PFS] La base de données fournie est vide")
  } else {
    start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                    ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
    end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                  ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
    
    if(is.null(supervisorList)){
      if(!is.null(agentList)){
        supervisorList <- unique(as.character(staff$supervisor_name[which(staff$agent %in% agentList)]))
      }else{
        supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
        agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
      }
    }else{
      if(!is.null(agentList)){
        agentList <- unique(as.character((staff %>% filter(supervisor_name %in% supervisorList,
                                                           agent %in% agentList))$agent))
      }else{
        agentList <- unique(as.character(staff$agent[which(staff$supervisor_name %in% supervisorList)]))
      }
    }
    
    # Questionnaires effectuées ...
    tmp <- data %>% 
      filter(supervisorname %in% supervisorList,
             agentname %in% agentList) %>%
      mutate(town2=sprintf("%s : %s",council,town)) %>% 
      select(id_town=a8,town=town2) %>%
      distinct()
    
    # Cas/questionnaires attendues ...
    tmp2 <- cases %>% 
      filter(supervisor_name %in% supervisorList) %>%
      mutate(town2=sprintf("%s : %s",council,town)) %>% 
      select(id_town,town=town2) %>% 
      distinct()
    
    visitedTown <- as.character(tmp[["town"]])
    novisitedTown <- setdiff(as.character(tmp2[["town"]]),visitedTown)
    numberTown <- nrow(cases %>% select(id_town,town) %>% distinct())
    
    result <- tibble_row(label= "Nombre de villages visités/entamés",
                         value= sprintf("%d / %d villages attendus (%.01f%%)",
                                        length(visitedTown),numberTown,
                                        100*length(visitedTown)/numberTown))
    
    result <- result %>% 
      add_row(tibble_row(label= "Liste des villages visités/entamés",
                         value= ifelse(length(visitedTown),
                                       paste(visitedTown,collapse = ", "),
                                       "- AUCUN VILLAGE -")))
    
    result <- result %>% 
      add_row(tibble_row(label= "Liste des villages non entamés",
                         value= ifelse(length(novisitedTown),
                                       paste(novisitedTown,collapse = ", "),
                                       "- AUCUN VILLAGE -")))
    
  }
  # rm(enddate,startdate,data,start,end,dbase,result,tmp,statut,agentList,supervisorList,townList,is_womenForm,
  #    evaluationName,evaluationLevel,evaluationLevel_list)
  invisible(result)
}


agent.drawTrendbyTown <- function(agentName,startdate=NULL,enddate=NULL,completeForm=FALSE){
  
  data  <- NULL
  iswoman <- as.character(staff$sex[which(staff$agent==agentName)])=="Femme"
  if(iswoman){
    data <- df
  }else{
    data <- dh
  }

  statut <- as.name(ifelse(iswoman,'n12','k12'))
  start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                  ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
  end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
  
  dbase <- data %>% 
    # mutate(id_town=as.numeric(a8)) %>%
    filter(survey_date>=start & survey_date<=end,agentname %in% agentName)
  if(completeForm) dbase <- dbase %>% filter(!!statut==2)
  
  dbase <- dbase  %>% group_by(id_town,town) %>% summarise(Effectif=n(),.groups = 'drop') %>% arrange(id_town,town)
  # dbase$town <- factor(dbase$town,levels=str_sort(unique(dbase$town)))
  
  
  # dbase <- left_join(dbase,target %>% filter(id_town %in% dbase$id_town),by="id_town")
  dbase <- left_join(dbase,target %>% select(-town),by="id_town")
  varname <- ifelse(iswoman,"nbhh_byWomen","nbhh")
  dbase$labs <- sprintf("%d / %d attendus",dbase[["Effectif"]],dbase[[varname]])
  
  agentList <- agentName
  startdate <- start
  enddate <- end
  include.inconsistencies <- TRUE
  
  stat <- getGlobalStatistics(data,startdate,enddate,agentList,NULL,NULL,include.inconsistencies)
  
  labsub <- sprintf("du %s : %s",
                    str_sub(stat$label[2],start=23,end=46),
                    str_trim(str_split(stat$value[2],"/")[[1]])[2])
  
  labcap <- sprintf("%s & %s",
                    str_trim(str_split(stat$value[5],"/")[[1]])[1],
                    str_trim(str_split(stat$value[6],"/")[[1]])[1])
  
  graph <- dbase %>%
    ggplot( aes(x=town, y=Effectif)) +
    geom_bar(aes(fill=Effectif), stat="identity", width=1) +
    labs(y=sprintf("Nombre de questionnaires %s",ifelse(completeForm,"complets","soumis")),
         title=stat$value[3],
         subtitle=labsub,
         caption=labcap)+
    coord_flip() + theme_linedraw() + xlab("") + 
    geom_text(aes(label=labs),hjust=1.5,colour="white", fontface="bold",size=3.5)
  
  # rm(agentName,agentList,startdate,enddate,data,iswoman,statut,start,end,dbase,completeForm,labsub,labcap,
  #    graph,dbase,varname,stat)
  invisible(graph)
}


survey.drawTrendbyTown <- function(supervisor=NULL,townList=NULL,startdate=NULL,enddate=NULL,
                                   completeForm=FALSE,auto_yaxis=TRUE,label_inner=TRUE){
  
  start <- ifelse(is.null(startdate),max(c(min(df[["survey_date"]]),min(dh[["survey_date"]]))),
                  ifelse(is.Date(startdate),startdate,
                         ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
  end <- ifelse(is.null(enddate),min(c(max(df[["survey_date"]]),max(dh[["survey_date"]]))),
                ifelse(is.Date(enddate),enddate,
                       ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
  
  supervisorList <- switch (ifelse(is.null(supervisor),"listOfSupervisors","oneSupervisor"),
    listOfSupervisors = base::intersect(unique(pluck(df,"supervisorname")),unique(pluck(dh,"supervisorname"))),
    oneSupervisor = supervisor[1]
  )
  
  
  fdb <- df %>% 
    # mutate(id_town=as.numeric(a8)) %>%
    filter(survey_date>=start & survey_date<=end,
           supervisorname %in% supervisorList) %>%
    mutate(town=sprintf("%d_%s",id_town,town))
  
  hdb <- dh %>% 
    # mutate(id_town=as.numeric(a8)) %>%
    filter(survey_date>=start & survey_date<=end,
           supervisorname %in% supervisorList) %>%
    mutate(town=sprintf("%d_%s",id_town,town))
  
  if(completeForm){
    fdb <- fdb %>% filter(n12==2)
    hdb <- hdb %>% filter(k12==2)
  } 
  
  if(length(townList)){
    fdb <- fdb %>% filter(id_town %in% townList)
    hdb <- hdb %>% filter(id_town %in% townList)
  }
  
  tmp.fdb <- fdb %>%
    group_by(id_town,town) %>%
    summarise(Effectif=n(),.groups = 'drop') %>%
    mutate(sex="Femme")
  
  tmp.hdb <- hdb %>%
    group_by(id_town,town) %>%
    summarise(Effectif=n(),.groups = 'drop') %>%
    mutate(sex="Homme")
  
  dbase <- bind_rows(tmp.fdb,tmp.hdb) %>%
    arrange(id_town)
  
  dbase$sex <- factor(dbase$sex, levels = c("Femme","Homme"))
  # dbase$town <- factor(dbase$town,levels=str_sort(unique(dbase$town)))
  
  # dbase <- left_join(dbase,target %>% filter(town %in% dbase$town),by="id_town")
  dbase <- left_join(dbase,target %>% select(-town),by="id_town")
  dbase$labs <- sprintf("%d/%d attendus..",dbase[["Effectif"]],dbase[["nbhh"]])
  
  startdate <- start
  enddate <- end
  # townList <- unique(as.character(dbase$town))
  townList <- unique(as.character(dbase$id_town))
  include.inconsistencies <- TRUE
  
  statF <- getGlobalStatistics(fdb,startdate,enddate,NULL,supervisorList,townList,include.inconsistencies)
  statH <- getGlobalStatistics(hdb,startdate,enddate,NULL,supervisorList,townList,include.inconsistencies)
  
  tval01 <- strtoi(str_extract(str_trim(str_split(statF$value[3],"/")[[1]])[1],"[0-9]+")) +
           strtoi(str_extract(str_trim(str_split(statH$value[3],"/")[[1]])[1],"[0-9]+"))
  
  tval02 <- strtoi(str_extract(str_trim(str_split(statF$value[3],"/")[[1]])[2],"[0-9]+")) +
           strtoi(str_extract(str_trim(str_split(statH$value[3],"/")[[1]])[2],"[0-9]+"))
  labTitle <- sprintf("%d questionnaires complets / %d soumis (%.01f%%)",
                      tval01,tval02,100*tval01/tval02)
  if(completeForm)
    labTitle <- sprintf("%d questionnaires complets",tval01)
  
  sval01 <- strtoi(str_extract(str_trim(str_split(statF$value[2],"/")[[1]])[2],"[0-9]+"))
  sval02 <- strtoi(str_extract(str_trim(str_split(statH$value[2],"/")[[1]])[2],"[0-9]+"))
  labsub <- sprintf("du %s : %d questionnaires complets attendus",
                    str_sub(statF$label[2],start=23,end=46),sval01+sval02) #,100*(sval01+sval02)/tval02
  
  cval01 <- strtoi(str_extract(str_trim(str_split(statF$value[5],"/")[[1]])[1],"[0-9]+")) +
            strtoi(str_extract(str_trim(str_split(statH$value[5],"/")[[1]])[1],"[0-9]+"))
  cval02 <- strtoi(str_extract(str_trim(str_split(statF$value[6],"/")[[1]])[1],"[0-9]+")) +
            strtoi(str_extract(str_trim(str_split(statH$value[6],"/")[[1]])[1],"[0-9]+"))
  
  labcap <- sprintf("%d questionnaires primaires complets & %d questionnaires avec de potentielles incohérences",
                    cval01,cval02)
  
  incr <- max(dbase$Effectif) %% 10
  maxvalue = max(dbase$Effectif)+ ifelse(incr,10-incr,0)
  pas <- maxvalue/20
  graph <- dbase %>%
    ggplot( aes(x=town, y=Effectif,fill=sex)) +
    geom_bar(stat="identity", width=1, position = "dodge2") +
    scale_fill_manual(values = c("palevioletred", "skyblue") ) +
    labs(y=sprintf("Nombre de questionnaires %s",ifelse(completeForm,"complets","soumis")),
         title=labTitle,
         subtitle=labsub,
         caption=labcap)+
    coord_flip() + theme_linedraw() + xlab("")
    
    if(length(townList)<=15){
      graph <- graph + 
        geom_label(aes(label=labs),hjust = ifelse(label_inner,0.5,-0.05),position = position_dodge(.9))
      if(!auto_yaxis)
        graph <- graph + 
          scale_y_continuous(limits=c(0,maxvalue),breaks=seq(from=0,to=maxvalue,by=1))
    }else{
      if(!auto_yaxis){
        graph <- graph +
          scale_y_continuous(limits=c(0,maxvalue),breaks=seq(from=0,to=maxvalue,by=pas)) # +
        # geom_text(aes(label=Effectif),vjust=0.5,hjust=0,colour="gray40", fontface="bold",size=2)
      }
    }
  invisible(graph)
}


agent.drawDataCollectEvolution <- function(agentName,startdate=NULL,enddate=NULL,
                                           completeForm=TRUE,auto_xaxis=FALSE,
                                           auto_yaxis=TRUE){
  
  data  <- NULL
  iswoman <- as.character(staff$sex[which(staff$agent==agentName)])=="Femme"
  if(iswoman){
    data <- df
  }else{
    data <- dh
  }
  
  threshold <- ifelse(iswoman,param$threshold_womanByDay,param$threshold_manByDay)
  statut <- as.name(ifelse(iswoman,'n12','k12'))
  start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                  ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
  end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
  
  dbase <- data %>% 
    filter(endtime>=as_date(start) & endtime<=as_date(end),
           agentname %in% agentName)
  if(completeForm) dbase <- dbase %>% filter(!!statut==2)
  
  dbase <- dbase  %>% 
    group_by(endtime=as_date(endtime)) %>% 
    summarise(Effectif=n())
  
  incr <- max(dbase$Effectif) %% 10
  maxvalue = max(dbase$Effectif)+ ifelse(incr,10-incr,0)
  
  graph <- dbase %>% 
    ggplot(aes(x = endtime, y = Effectif)) +
    geom_line(color="steelblue") + 
    geom_hline(aes(yintercept = threshold,colour="red"),
               linetype="twodash",show.legend= FALSE)+
    # geom_point() + geom_label(aes(label=Effectif)) +
    geom_label(aes(label=Effectif))
    
  if(!auto_xaxis)
    graph <- graph + scale_x_date(date_labels = "%d %b",date_minor_breaks = "1 day",date_breaks = "1 day")
  
  if(!auto_yaxis)
    graph <- graph + scale_y_continuous(limits=c(0,maxvalue),breaks=seq(from=0,to=maxvalue,by=1))
  
  graph <- graph + 
    xlab("") + theme_ipsum() +
    theme(axis.text.x=element_text(angle=60, hjust=1)) +
    # geom_text(aes(label="Seuil"),y=threshold) +
    labs(y=sprintf("Nombre de questionnaires %s",ifelse(completeForm,"complets","soumis")),
         title=sprintf("%s",paste(agentName,collapse = ", ")),
         subtitle=sprintf("du %s au %s : %d questionnaire(s) %s au total.",
                          as_date(start), as_date(end),sum(dbase$Effectif),
                          ifelse(completeForm,"complet(s)","soumi(s)")),
         caption="© PFS Cameroun 2021")
  invisible(graph)
}

town.drawDataCollectEvolution <- function(town=NULL,startdate=NULL,enddate=NULL,completeForm=TRUE,
                                          auto_xaxis=FALSE){
  
  start <- ifelse(is.null(startdate),max(c(min(df[["survey_date"]]),min(dh[["survey_date"]]))),
                  ifelse(is.Date(startdate),startdate,
                         ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
  end <- ifelse(is.null(enddate),min(c(max(df[["survey_date"]]),max(dh[["survey_date"]]))),
                ifelse(is.Date(enddate),enddate,
                       ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
  
  townList <- switch (ifelse(is.null(town),"listOfTowns","oneTown"),
                      listOfTowns = base::intersect(unique(pluck(df,"id_town")),
                                                    unique(pluck(dh,"id_town"))),
                      oneTown = town[1])
  
  fdb <- df %>% 
    filter(endtime>=as_date(start) & endtime<=as_date(end),
           id_town %in% townList)
  hdb <- dh %>% 
    filter(endtime>=as_date(start) & endtime<=as_date(end),
           id_town %in% townList)
  
  if(completeForm){
    fdb <- fdb %>% filter(n12==2)
    hdb <- hdb %>% filter(k12==2)
  } 
  
  threshold_man <- length(unique(hdb[["agentname"]]))*param$threshold_manByDay
  threshold_woman <- length(unique(fdb[["agentname"]]))*param$threshold_womanByDay
  
  tmp.fdb <- fdb %>%
    group_by(endtime=as_date(endtime)) %>% 
    summarise(Effectif=n()) %>% 
    mutate(sex="Femme")
  
  tmp.hdb <- hdb %>%
    group_by(endtime=as_date(endtime)) %>% 
    summarise(Effectif=n()) %>%
    mutate(sex="Homme")
  
  dbase <- bind_rows(tmp.fdb,tmp.hdb)
  dbase$sex <- factor(dbase$sex, levels = c("Femme","Homme"))
  
  incr <- max(dbase$Effectif) %% 10
  maxvalue = max(dbase$Effectif)+ ifelse(incr,10-incr,0)
  graph <- dbase %>% 
    ggplot(aes(x = endtime, y = Effectif)) +
    geom_line(aes(color=sex)) +
    scale_color_manual(values = c("palevioletred", "skyblue"))
  
  if(threshold_woman==threshold_man){
    graph <- graph +
      geom_hline(aes(yintercept = threshold_woman),linetype="dotted",show.legend= FALSE) +
      annotate("text",y=threshold_woman,x=as_date(start),
               color="black",fontface="bold.italic",label=sprintf("%d",threshold_woman))
  }else{
    graph <- graph + 
      geom_hline(aes(yintercept = threshold_woman),color="palevioletred",
                 linetype="dotted",show.legend= FALSE) +
      annotate("text",y=threshold_woman,x=as_date(start),
               color="palevioletred",fontface="bold.italic",
               label=sprintf("%d",threshold_woman)) +
      geom_hline(aes(yintercept = threshold_man),color="skyblue",
                 linetype="dotted",show.legend= FALSE)  +
      annotate("text",y=threshold_man,x=as_date(start),
               color="skyblue",fontface="bold.italic",
               label=sprintf("%d",threshold_man))
  }
  
  graph <- graph + geom_label(aes(label=Effectif))
  
  if(!auto_xaxis)
    graph <- graph +
    scale_x_date(date_labels = "%d %b",date_minor_breaks = "1 day",date_breaks = "1 day")
    #+ scale_y_continuous(limits=c(0,maxvalue),breaks=seq(from=0,to=maxvalue,by=1))
  
  graph <- graph +
    xlab("") + theme_ipsum() +
    theme(axis.text.x=element_text(angle=60, hjust=1)) +
    labs(y=sprintf("Nombre de questionnaires %s",ifelse(completeForm,"complets","soumis")),
         title=sprintf("%s",ifelse(length(townList)==1,
                                   as.character(towns$town[towns$id_town==townList]),
                                   "Projet Filets sociaux")),
         subtitle=sprintf("du %s au %s : %d questionnaire(s) %s au total",
                          as_date(start), as_date(end),sum(dbase$Effectif),
                          ifelse(completeForm,"complet(s)","soumi(s)")),
         caption="© PFS Cameroun 2021")
  
  invisible(graph)
}


survey.drawDataCollectEvolution <- function(supervisor=NULL,startdate=NULL,enddate=NULL,
                                            completeForm=TRUE,auto_xaxis=FALSE){
  
  start <- ifelse(is.null(startdate),max(c(min(df[["survey_date"]]),min(dh[["survey_date"]]))),
                  ifelse(is.Date(startdate),startdate,
                         ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
  end <- ifelse(is.null(enddate),min(c(max(df[["survey_date"]]),max(dh[["survey_date"]]))),
                ifelse(is.Date(enddate),enddate,
                       ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
  
  supervisorList <- switch (ifelse(is.null(supervisor),"listOfSupervisors","oneSupervisor"),
                            listOfSupervisors = base::intersect(unique(pluck(df,"supervisorname")),unique(pluck(dh,"supervisorname"))),
                            oneSupervisor = supervisor[1])
  
  fdb <- df %>% 
    filter(endtime>=as_date(start) & endtime<=as_date(end),
           supervisorname %in% supervisorList)
  hdb <- dh %>% 
    filter(endtime>=as_date(start) & endtime<=as_date(end),
           supervisorname %in% supervisorList)
  
  if(completeForm){
    fdb <- fdb %>% filter(n12==2)
    hdb <- hdb %>% filter(k12==2)
  } 
  
  threshold_man <- length(unique(hdb[["agentname"]]))*param$threshold_manByDay
  threshold_woman <- length(unique(fdb[["agentname"]]))*param$threshold_womanByDay
  
  tmp.fdb <- fdb %>%
    group_by(endtime=as_date(endtime)) %>% 
    summarise(Effectif=n()) %>% 
    mutate(sex="Femme")
  
  tmp.hdb <- hdb %>%
    group_by(endtime=as_date(endtime)) %>% 
    summarise(Effectif=n()) %>%
    mutate(sex="Homme")
  
  dbase <- bind_rows(tmp.fdb,tmp.hdb)
  dbase$sex <- factor(dbase$sex, levels = c("Femme","Homme"))
  
  incr <- max(dbase$Effectif) %% 10
  maxvalue = max(dbase$Effectif)+ ifelse(incr,10-incr,0)
  graph <- dbase %>% 
    ggplot(aes(x = endtime, y = Effectif)) +
    geom_line(aes(color=sex)) +
    scale_color_manual(values = c("palevioletred", "skyblue"))
  
  if(threshold_woman==threshold_man){
    graph <- graph +
      geom_hline(aes(yintercept = threshold_woman),linetype="dotted",show.legend= FALSE) +
      annotate("text",y=threshold_woman,x=as_date(start),
               color="black",fontface="bold.italic",label=sprintf("%d",threshold_woman))
  }else{
    graph <- graph + 
      geom_hline(aes(yintercept = threshold_woman),color="palevioletred",
                 linetype="dotted",show.legend= FALSE) +
      annotate("text",y=threshold_woman,x=as_date(start),
               color="palevioletred",fontface="bold.italic",
               label=sprintf("%d",threshold_woman)) +
      geom_hline(aes(yintercept = threshold_man),color="skyblue",
                 linetype="dotted",show.legend= FALSE)  +
      annotate("text",y=threshold_man,x=as_date(start),
               color="skyblue",fontface="bold.italic",
               label=sprintf("%d",threshold_man))
  }
  
  graph <- graph +
    geom_label(aes(label=Effectif))
  
  if(!auto_xaxis)
    graph <- graph + scale_x_date(date_labels = "%d %b",date_minor_breaks = "1 day",date_breaks = "1 day")
    # scale_y_continuous(limits=c(0,maxvalue),breaks=seq(from=0,to=maxvalue,by=1)) +
  
  graph <- graph +
    xlab("") + theme_ipsum() +
    theme(axis.text.x=element_text(angle=60, hjust=1)) +
    labs(y=sprintf("Nombre de questionnaires %s",ifelse(completeForm,"complets","soumis")),
         title=sprintf("%s",ifelse(length(supervisorList)==1,supervisorList,"Projet Filets sociaux")),
         #subtitle=sprintf("Collecte de donnée du %s au %s : %d questionnaire(s) %s au total",
         subtitle=sprintf("du %s au %s : %d questionnaire(s) %s au total",
                          as_date(start), as_date(end),sum(dbase$Effectif),
                          ifelse(completeForm,"complet(s)","soumi(s)")),
         caption="© PFS Cameroun 2021")
  
  invisible(graph)
}

