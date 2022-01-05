# Build reports by agent, town and for survey
# Charles M. <charlesmoute@gmail.com>
# version 210827-0630

#----------------------------------------------------------------------------------------------->
# Fonctions utilitaires
#----------------------------------------------------------------------------------------------->

# Ecriture de la section identification des donnees evaluees dans le rapport
#type = c("survey","agent","supervisor","town")
#type=survey => agentList=NULL, supervisorLis=t=NULL & townList==NULL
#type=agent => agentList!=NULL & length(agentList)==1, supervisorList==NULL & townList==NULL
#type=supervisor => agentList==NULL, supervisorList!=NULL & length(agentList)==1 & townList==NULL
#type=town => agentList==NULL, supervisorList==NULL & townList!=NULL & length(agentList)==1
write.documentHeader <- function (type=c("survey","agent","supervisor","town"),startdate=NULL,
                                  enddate=NULL,agentList=NULL,supervisorList=NULL,
                                  townList=NULL,evaluationDate=param$evaluationDate){
  
  type <- match.arg(type)
  agentList <- switch(type=="agent",
                      ifelse(is.null(agentList),staff$agent[1],agentList[1]),
                      agentList)
  supervisorList <- switch(type=="supervisor",
                           ifelse(is.null(supervisorList),staff$supervisor_name[1],supervisorList[1]),
                           supervisorList)
  data <- switch(type,
                 survey=bind_rows(df %>% 
                                    select(caseid,supervisorname,agentname,region,
                                           division,council,id_town,town,survey_date) %>%
                                    mutate(typeOfStaff="Enquêtrice"),
                                  dh %>% 
                                    select(caseid,supervisorname,agentname,region,
                                           division,council,id_town,town,survey_date) %>%
                                    mutate(typeOfStaff="Enquêteur")),
                 agent = switch(ifelse(agentList[1] %in% unique(df[["agentname"]]),"woman","man"),
                                woman=df %>% 
                                  select(caseid,supervisorname,agentname,region,
                                         division,council,id_town,town,survey_date) %>%
                                  mutate(typeOfStaff="Enquêtrice"),
                                man=dh %>% 
                                  select(caseid,supervisorname,agentname,region,
                                         division,council,id_town,town,survey_date) %>%
                                  mutate(typeOfStaff="Enquêteur")),
                 supervisor = bind_rows(df %>% 
                                          select(caseid,supervisorname,agentname,region,
                                                 division,council,id_town,town,survey_date) %>%
                                          mutate(typeOfStaff="Enquêtrice"),
                                        dh %>% 
                                          select(caseid,supervisorname,agentname,region,
                                                 division,council,id_town,town,survey_date) %>%
                                          mutate(typeOfStaff="Enquêteur")),
                 town = bind_rows(df %>% 
                                    select(caseid,supervisorname,agentname,region,
                                           division,council,id_town,town,survey_date) %>%
                                    mutate(typeOfStaff="Enquêtrice"),
                                  dh %>% 
                                    select(caseid,supervisorname,agentname,region,
                                           division,council,id_town,town,survey_date) %>%
                                    mutate(typeOfStaff="Enquêteur"))
                 )
  
  start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                  ifelse(is.Date(startdate),startdate,
                         ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
  end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                ifelse(is.Date(enddate),enddate,
                       ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
  
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
  
  agentList <- unique(data[["agentname"]])
  supervisorList <- unique(data[["supervisorname"]])
  townList <- unique(data[["id_town"]])
  
  result <- tibble_row(lab="Date rapport : ",value=sprintf("%s",as_date(evaluationDate)))
  result <- result %>%
    add_row(tibble_row(lab="Période de monitoring : ",
                       value=sprintf("du %s au %s",as_date(start),as_date(end))))
  
  regions <- unique(data[["region"]])
  result <- result %>%
    add_row(tibble_row(lab=sprintf("Région%s : ",ifelse(length(regions)>1,"s","")),
                       value=sprintf("%s",paste(regions,collapse = ", "))))
  
  divisions <- unique(data[["division"]])
  result <- result %>%
    add_row(tibble_row(lab=sprintf("Département%s : ",ifelse(length(divisions)>1,"s","")),
                       value=sprintf("%s",paste(divisions,collapse = ", "))))
  
  councils <- unique(data[["council"]])
  result <- result %>%
    add_row(tibble_row(lab=sprintf("Commune%s : ",ifelse(length(councils)>1,"s","")),
                       value=sprintf("%s",paste(councils,collapse = ", "))))
  
  villages <- unique(data[["id_town"]])
  # nomVillages <- unique(sprintf("%s : %s",data[["council"]],data[["town"]]))
  nomVillages <- unique(data[["town"]])
  result <- result %>%
    add_row(tibble_row(lab=sprintf("Village%s : ",ifelse(length(villages)>1,
                                                         sprintf("s (%d)",length(villages)),
                                                         "")),
                       value=sprintf("%s",paste(nomVillages,collapse = ", "))))
  
  supervisors <- unique(data[["supervisorname"]])
  result <- result %>%
    add_row(tibble_row(lab=sprintf("Superviseur%s : ",ifelse(length(supervisors)>1,
                                                         sprintf("s (%d)",length(supervisors)),
                                                         "")),
                       value=sprintf("%s",paste(supervisors,collapse = ", "))))
  
  investigators <- unique((data %>% filter(typeOfStaff=="Enquêteur"))[["agentname"]])
  if(length(investigators))
    result <- result %>%
    add_row(tibble_row(lab=sprintf("Enquêteur%s : ",
                                   ifelse(length(investigators)>1,
                                          sprintf("s (%d)",length(investigators)),
                                          "")),
                       value=sprintf("%s",paste(investigators,collapse = ", "))))
  
  investigators <- unique((data %>% filter(typeOfStaff=="Enquêtrice"))[["agentname"]])
  if(length(investigators))
    result <- result %>%
    add_row(tibble_row(lab=sprintf("Enquêtrice%s : ",
                                   ifelse(length(investigators)>1,
                                          sprintf("s (%d)",length(investigators)),
                                          "")),
                       value=sprintf("%s",paste(investigators,collapse = ", "))))
  
  ft <- flextable(result,col_keys = c("lab","value")) %>%
    delete_part(part="header") %>% # suppression de l'entête
    border_inner_h(border = fp_border(color = "black", width=1.5)) %>%
    bold(j=1,part = "body") %>%
    width(j=1,width=1.87) %>%
    width(j=2,width=4.43) %>%
    hline_bottom(border = fp_border(color = "white", width=1.5))
  
  if(length(unique(data$typeOfStaff))==2){
    ft <- ft %>%
      hline(i=nrow(result)-1,border = fp_border(color = "white", width=1.5))
  }
  
  # print(ft, preview = "docx")
  # read_docx() %>% body_add_flextable(ft) # pour l'ajout dans un document produit avec officer
  # legerement different dans le cas d'un rmarkdown où on peut directement utiliser ft
  invisible(ft)
}

# Ecriture de la section relatif aux statistiques de collecte des donnees
# Exemple pour agent :
# doctmp <- write.dataStatistics(type="agent",agentList = staff$agent[1])
# doctmp <- write.dataStatistics(type="supervisor",supervisorList = staff$supervisor_name[60])
  
write.dataStatistics <- function(type=c("agent","supervisor","town"),startdate=NULL,
                                 enddate=NULL,agentList=NULL,supervisorList=NULL,
                                 townList=NULL,evaluationDate=param$evaluationDate,
                                 graph_onescreen=TRUE){
  
  type_value <- match.arg(type)
  agentList <- switch(type_value=="agent",
                      ifelse(is.null(agentList),staff$agent[1],agentList[1]),
                      agentList)
  supervisorList <- switch(type_value=="supervisor",
                           ifelse(is.null(supervisorList),staff$supervisor_name[1],supervisorList[1]),
                           supervisorList)
  data <- switch(type_value,
                 agent = switch(ifelse(agentList[1] %in% unique(df[["agentname"]]),"woman","man"),
                                woman=df %>%
                                  select(caseid,supervisorname,agentname,region,
                                         division,council,id_town,town,survey_date) %>%
                                  mutate(typeOfStaff="Enquêtrice"),
                                man=dh %>%
                                  select(caseid,supervisorname,agentname,region,
                                         division,council,id_town,town,survey_date) %>%
                                  mutate(typeOfStaff="Enquêteur")),
                 bind_rows(df %>% 
                             select(caseid,supervisorname,agentname,region,
                                    division,council,id_town,town,survey_date) %>%
                             mutate(typeOfStaff="Enquêtrice"),
                           dh %>% 
                             select(caseid,supervisorname,agentname,region,
                                    division,council,id_town,town,survey_date) %>%
                             mutate(typeOfStaff="Enquêteur"))
                 )
  
  data_type <- ifelse(length(unique(data[["typeOfStaff"]]))==2,"both",
                      ifelse(unique(data[["typeOfStaff"]])=="Enquêtrice",
                             "woman","man"))

  startdate <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                  ifelse(is.Date(startdate),startdate,
                         ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
  enddate <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                ifelse(is.Date(enddate),enddate,
                       ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
  
  if(is.null(agentList))
    agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
  
  if(is.null(supervisorList))
    supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
  
  if(is.null(townList))
    townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
  
  data <- data %>%
    filter(survey_date>=startdate & survey_date<=enddate,
           agentname %in% agentList,
           supervisorname %in% supervisorList,
           id_town %in% townList)
  
  agentList <- unique(data[["agentname"]])
  supervisorList <- unique(data[["supervisorname"]])
  townList <- unique(data[["id_town"]])
  
  portrait <- block_section(
    prop_section(
      page_size = page_size(orient = "portrait"),
      type = "continuous"
    ))
  
  landscape <- block_section(
    prop_section(
      page_size = page_size(orient = "landscape"),
      type = "nextPage"# type = "nextPage"
    ))
  
  
  document <- read_docx() %>%
    body_add_par(value="",style="Normal") %>%
    body_add_par(value = "Résumé statistique", style = "heading 2")
  

  document <- switch (data_type,
    woman = document %>%
      body_add_par(value="",style="Normal") %>%
      body_add_flextable(flextable(getGlobalStatistics(data=df,startdate=startdate,enddate = enddate,
                                                       agentList = agentList,supervisorList = supervisorList,
                                                       townList = townList,include.inconsistencies = TRUE),
                                   col_keys = c("label","value")) %>%
                           delete_part(part="header") %>% # suppression de l'entête
                           border_inner_h(border = fp_border(color = "black", width=1.5)) %>%
                           bold(j=1,part = "body") %>%
                           width(j=1,width=1.87) %>%
                           width(j=2,width=4.43) %>%
                           hline_bottom(border = fp_border(color = "white", width=1.5))),
    man  = document %>%
      body_add_par(value="",style="Normal") %>%
      body_add_flextable(flextable(getGlobalStatistics(data=dh,startdate=startdate,enddate = enddate,
                                                       agentList = agentList,supervisorList = supervisorList,
                                                       townList = townList,include.inconsistencies = TRUE),
                                   col_keys = c("label","value")) %>%
                           delete_part(part="header") %>% # suppression de l'entête
                           border_inner_h(border = fp_border(color = "black", width=1.5)) %>%
                           bold(j=1,part = "body") %>%
                           width(j=1,width=1.87) %>%
                           width(j=2,width=4.43) %>%
                           hline_bottom(border = fp_border(color = "white", width=1.5))),
    both = document %>%
      body_add_par(value="",style="Normal") %>%
      body_add_par(value = "Enquêteur(s)", style = "heading 3") %>%
      body_add_par(value="",style="Normal") %>%
      body_add_flextable(flextable(getGlobalStatistics(data=dh,startdate=startdate,enddate = enddate,
                                                       agentList = agentList,supervisorList = supervisorList,
                                                       townList = townList,include.inconsistencies = TRUE),
                                   col_keys = c("label","value")) %>%
                           delete_part(part="header") %>% # suppression de l'entête
                           border_inner_h(border = fp_border(color = "black", width=1.5)) %>%
                           bold(j=1,part = "body") %>%
                           width(j=1,width=1.87) %>%
                           width(j=2,width=4.43) %>%
                           hline_bottom(border = fp_border(color = "white", width=1.5))) %>%
      body_add_par(value="",style="Normal") %>%
      body_add_par(value = "Enquêtrice(s)", style = "heading 3") %>%
      body_add_par(value="",style="Normal") %>%
      body_add_flextable(flextable(getGlobalStatistics(data=df,startdate=startdate,enddate = enddate,
                                                       agentList = agentList,supervisorList = supervisorList,
                                                       townList = townList,include.inconsistencies = TRUE),
                                   col_keys = c("label","value")) %>%
                           delete_part(part="header") %>% # suppression de l'entête
                           border_inner_h(border = fp_border(color = "black", width=1.5)) %>%
                           bold(j=1,part = "body") %>%
                           width(j=1,width=1.87) %>%
                           width(j=2,width=4.43) %>%
                           hline_bottom(border = fp_border(color = "white", width=1.5)))
  )
  
  document <- document %>% 
    body_add_par(value="",style="Normal") %>%
    body_end_block_section(value = portrait) %>%
    body_add_par(value = "Evolution de la collecte de données", style = "heading 2")
    
  g1 <- g2 <- NULL
  # if(type_value=="agent" & length(agentList)==1){ #ceci est une precaution inutile deja verifie plus haut..
  if(type_value=="agent"){
    g1 <- agent.drawDataCollectEvolution(agentName=agentList,startdate = startdate,
                                         enddate = enddate, completeForm = FALSE)
    g2 <- agent.drawDataCollectEvolution(agentName=agentList,startdate = startdate,enddate = enddate)
  }else{
    if(type_value=="town"){
      g1 <- town.drawDataCollectEvolution(town=townList,startdate = startdate,
                                          enddate = enddate, completeForm = FALSE)
      g2 <- town.drawDataCollectEvolution(town=townList,startdate = startdate,enddate = enddate)
    }else{
      g1 <- survey.drawDataCollectEvolution(supervisor=supervisorList, startdate = startdate,
                                            enddate = enddate, completeForm = FALSE)
      g2 <- survey.drawDataCollectEvolution(supervisor=supervisorList,startdate = startdate,
                                            enddate = enddate)
    }
  }
  
  #Sauvegarde de l'image produite dans un unique fichier tampon
  fileout <- tempfile(fileext = ".png")
  if(graph_onescreen){
    
    ggsave(fileout,plot=g1+g2,device = "png",width = 14,height = 7)
    
    # Insertion de l'image en mode paysage dans notre document..
    document <- document %>%
      body_add_par(value="",style="Normal") %>%
      body_add_img(fileout,height = 4.7, width = 9.5, style = "centered") %>%
      body_add_par(value="",style="Normal") #%>%
    # body_end_block_section(value = landscape)
    
    # # TEST: Insertion de l'image en mode portrait dans notre document..
    # fileout <- tempfile(fileext = ".png")
    # ggsave(fileout,plot=g1,device = "png",width = 14,height = 7)
    # document <- document %>%
    #   body_add_par(value="",style="Normal") %>%
    #   body_add_img(fileout,height = 4.7, width = 9.5, style = "centered") %>%
    #   body_add_par(value="",style="Normal") %>%
    #   body_add_gg(value = g2,style = "centered") %>%
    #   body_add_par(value="",style="Normal")
  }else{
    
    # Creation d'un fichier word vierge..
    doctmp <- read_docx()
   
    # Insertion de la premiere image (questionnaires soumis)
    fileout <- tempfile(fileext = ".png")
    ggsave(fileout,plot=g1,device = "png",width = 14,height = 7)
    doctmp <- doctmp %>%
      body_add_par(value="",style="Normal") %>%
      body_add_img(fileout,height = 4.7, width = 9.5, style = "centered")
    
    # Insertion de la deuxieme image (questionnaires complets)
    fileout <- tempfile(fileext = ".png")
    ggsave(fileout,plot=g2,device = "png",width = 14,height = 7)
    doctmp <- doctmp %>%
      body_add_par(value="",style="Normal") %>%
      body_add_img(fileout,height = 4.7, width = 9.5, style = "centered")
    
    # Sauvevarge du fichier temporaire word..
    fileout <- tempfile(fileext = ".docx")
    print(doctmp,target = fileout)
    
    # Insertion du contenu dans le document en cours de creation...
    document <- document %>% 
      body_add_docx(src=fileout) 
  }
  
  if(type_value=="agent"){
    g1 <- agent.drawTrendbyTown(agentName=agentList,startdate = startdate,enddate = enddate)
    g2 <- agent.drawTrendbyTown(agentName=agentList,startdate = startdate,enddate = enddate,
                                completeForm=TRUE)
    
    fileout <- tempfile(fileext = ".png")
    ggsave(fileout,plot=g1+g2,device = "png",width = 14,height = 7)
    
    document <- document %>%
      # body_end_block_section(value = portrait) %>%
      body_add_par(value = "Listing par village", style = "heading 2") %>%
      body_add_par(value="",style="Normal") %>%
      body_add_img(fileout,height = 4.7, width = 9.5, style = "centered") %>%
      body_add_par(value="",style="Normal") #%>%
      # body_end_block_section(value = landscape)
  }else{
    villages <- unique(data[["id_town"]])
    total <- ceiling(length(villages)/10)
    g1 <- g2 <- NULL
    doctmp <- read_docx()
    for(i in 0:total){
      numList <- c(1:10)+(i*10)
      numList <- numList[which(numList<=length(villages))]
      if(length(numList)){
        g1 <- survey.drawTrendbyTown(supervisor=supervisorList,townList = villages[numList])
        g2 <- survey.drawTrendbyTown(supervisor=supervisorList,townList = villages[numList],
                                     completeForm=TRUE)
        fileout <- tempfile(fileext = ".png")
        ggsave(fileout,plot=g1+g2,device = "png",width = 14,height = 7)
        doctmp <- doctmp %>%
          body_add_par(value="",style="Normal") %>%
          body_add_img(fileout,height = 4.7, width = 9.5, style = "centered")
      }
    }
    fileout <- tempfile(fileext = ".docx")
    print(doctmp,target = fileout)
    if(!is.null(g1)){
      document <- document %>%
        # body_end_block_section(value = portrait) %>%
        body_add_par(value = "Listing par village", style = "heading 2") %>%
        body_add_par(value="",style="Normal") %>%
        body_add_docx(src=fileout) %>%
        body_add_par(value="",style="Normal") #%>%
        # body_end_block_section(value = landscape)
    }
  }
  invisible(document)
}

# Ecriture de la section relatif à l'identification des potentielles incoherences dans les
# donnees
# Lorsque vous souhaitez ecrire des erreurs pour les agents > assurez-vous que le nom de l'agent
# correspond bien au type en parametre.. par defaut le programme corrige en selectionnant le bon
# champ...
# Exemple : doctmp <- write.dataInconsistencies(type=ifelse(staff$sex[1]=="Femme","woman","man"),
# evaluationLevel = "agent",agentList = staff$agent[1])
# Dans le cas des resport des erreurs au niveau supervisor & town ... pour survey on a une fonction
# qui s'en occupe mais pour l'instant on laisse cette option... on appelera deux fois
# la fontion pour les incohenrences au niveau des femmes + celles au niveau des hommes
# Exemple : doctmp <- write.dataInconsistencies(type="woman",evaluationLevel = "supervisor",
#                                               supervisorList = staff$supervisor_name[1])
# 
write.dataInconsistencies <- function(type=c("woman","man"),
                                      evaluationLevel=c("survey","agent","supervisor","town"),
                                      listOfFunctions=c("check.respondentAge","check.numberOfTMO",
                                                        "check.incomeAmont","check.savingsAmont",
                                                        "check.debtAmont","check.useOfDontAnswer", 
                                                        "check.useOfDontKnow","check.useOfOther"),
                                      startdate=NULL,enddate=NULL,agentList=NULL,supervisorList=NULL,
                                      townList=NULL,evaluationDate=param$evaluationDate,title=NULL,subtitle=NULL){
  
  fn_List <- c("all","check.availabilityOfCommunitySupport", "check.consentment", 
              "check.debtAmont", "check.duplicatedID", "check.existenceOfIntimatePartnerViolence", 
              "check.householdPossessions", "check.ifActivityAvailabled", "check.ifOtherActivity", 
              "check.incomeAmont", "check.landUse", "check.numberOfEmotionalSkills", 
              "check.numberOfSpouses", "check.numberOfTMO", "check.respondentAge", 
              "check.respondentAgeAtMarriage", "check.savingsAmont", "check.statutOfForms", 
              "check.timeInDailyActivities", "check.useOfDontAnswer", "check.useOfDontKnow", 
              "check.useOfJumps", "check.useOfOther")
  fn_names <- c("Potentielles incohérences sur la disponibilité d'un support communautaire", 
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
  names(fn_names) <- fn_List[-1]
  
  w_fn <- c("check.existenceOfIntimatePartnerViolence","check.numberOfSpouses","check.numberOfTMO",
            "check.respondentAgeAtMarriage","check.householdPossessions","check.landUse")
  
  type_value <- match.arg(type)
  evaluation_value <- match.arg(evaluationLevel)
  data <- switch(type_value,woman = df,man=dh)
  
  fn_values <- match.arg(listOfFunctions,fn_List,several.ok = TRUE)
  if("all" %in% fn_values){
    fn_values <- fn_values[which(!(fn_values %in% "all"))]
    if(!length(fn_values)) fn_values <- fn_List[-1]
  }
  
  fn_values <- switch(type_value,
                      woman = fn_values,
                      man=fn_values[which(!(fn_values %in% w_fn))])
  
  startdate <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                      ifelse(is.Date(startdate),startdate,
                             ifelse(is.numeric(startdate),
                                    as_date(startdate),
                                    ymd(startdate))))
  
  enddate <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                    ifelse(is.Date(enddate),enddate,
                           ifelse(is.numeric(enddate),
                                  as_date(enddate),
                                  ymd(enddate))))
  if(is.null(agentList))
    agentList <- unique(pluck(data,"agentname")) #unique(data[["agentname"]])
  else{
    agentList <- agentList[agentList %in% unique(data[["agentname"]])]
    if(!length(agentList)) agentList <- unique(data[["agentname"]])
  }
  
  if(is.null(supervisorList))
    supervisorList <- unique(pluck(data,"supervisorname")) #unique(data[["supervisorname"]])
  
  if(is.null(townList))
    townList <- unique(pluck(data,"id_town")) #unique(data[["town"]])
  
  data <- data %>% 
    filter(survey_date>=startdate & survey_date<=enddate,
           agentname %in% agentList,
           supervisorname %in% supervisorList,
           id_town %in% townList)
  
  # portrait <- block_section(
  #   prop_section(
  #     page_size = page_size(orient = "portrait"),
  #     type = "continuous"
  #   ))
  # 
  # landscape <- block_section(
  #   prop_section(
  #     page_size = page_size(orient = "landscape"),
  #     type = "nextPage"
  #   ))

  document <- read_docx() #%>%
    # body_add_par(value="",style="Normal") %>%
    # body_end_block_section(value = portrait)
  
  
  style_name0 <- switch(evaluation_value,agent="heading 1","heading 2")
  style_name <- switch(evaluation_value,agent="heading 2","heading 3")
  
  #Insertion des incoherences dans une boucle
  if(!is.null(title)){
    document <- document %>%
      body_add_par(value = title, style = "heading 1") %>%
      body_add_par(value="",style="Normal")
    style_name <- "heading 2"
  }
    
  if(!is.null(subtitle)){
    document <- document %>%
      body_add_par(value = subtitle, style = "heading 2") %>%
      body_add_par(value="",style="Normal")
    style_name <- "heading 3"
  }
  
  if(nrow(data)){
    
    keysName_todelete <- c("pfs_info","respondent_name","starttime", "endtime","survey_statut")
    if(evaluation_value=="supervisor") keysName_todelete <- c(keysName_todelete,"supervisorname")
    if(evaluation_value=="agent") keysName_todelete <- c(keysName_todelete,"agentname","supervisorname")
    if(evaluation_value=="town") keysName_todelete <- c(keysName_todelete,"town")
    pgwidth <- 10
    
    for(fn in fn_values){
      result <- do.call(fn,list(data=data,startdate=startdate,enddate=enddate,
                                agentList=agentList,supervisorList=supervisorList,
                                townList=townList),
                        envir = .GlobalEnv)
      if(!is.null(result$value) && nrow(result$value)){
        
        key_names <- setdiff(names(result$value),keysName_todelete)
        
        # automatiquement adjusté à la taille du contenu
        # ft <- theme_zebra(autofit(flextable(result$value,col_keys = key_names)))
        
        # Adjustement du tableau a la taille du "document"
        ft <- autofit(flextable(result$value,col_keys = key_names))
        ft <- width(ft,width = dim(ft)$widths*pgwidth/(flextable_dim(ft))$widths)
        ft <- theme_zebra(ft)
        
        
        # ft <- theme_zebra(fit_to_width(flextable(result$value,col_keys = key_names),
        #                                max_width = 14/length(key_names)))
        
        document <- document %>%
          body_add_par(value = fn_names[fn], style = style_name) %>%
          body_add_par(value="",style="Normal") %>%
          body_add_par(value=paste(result$msg,collapse = "\n"),style="Normal") %>%
          body_add_par(value="",style="Normal") %>%
          body_add_flextable(ft) %>%
          body_add_par(value="",style="Normal")
      }
    }
    
  }else{
    document <- document %>%
      body_add_par(value="Aucune potentielle incohérence n'a été identifiée",style="Normal")
  }
  
  # document <- document %>%
  #   body_add_par(value="",style="Normal") %>%
  #   body_end_block_section(value = landscape)
  
  invisible(document)
}

# Production des rapports d'evaluations par agent et sur une periode indique
# write.agentInfos(staff[["agent"]],startdate=param$startdate,enddate=param$enddate)
write.agentInfos <- function(agentList,startdate=NULL,enddate=NULL,graph_onescreen=TRUE){
 
  if(!any(agentList %in% staff[["agent"]])){
    tmp <- agentList[!agentList %in% staff$agent]
    cat(sprintf("[PFS] Les noms suivants ne figurent pas dans la liste des agents de PFS :  %s.",
                paste(tmp,collapse = ", ")))
  }else{
    
    portrait <- block_section(
      prop_section(
        page_size = page_size(orient = "portrait"),
        type = "continuous"
      ))
    
    landscape <- block_section(
      prop_section(
        page_size = page_size(orient = "landscape"),
        type = "nextPage"
      ))
    
    for(agent in agentList){
      filename <- sprintf("%s/%s.docx",dir_agent,agent)
      if(file.exists(filename)) invisible(file.remove(filename))
      
      cat(sprintf("\nPFS > Evaluation du travail de l'agent %s.",agent))
      doc_header  <- write.documentHeader(type="agent",startdate=startdate,enddate=enddate,
                                          agentList =agent)
      doc_statistics <- write.dataStatistics(type="agent",startdate=startdate,enddate=enddate,
                                             agentList = agent,graph_onescreen=graph_onescreen)
      data_type <- ifelse(as.character(staff[["sex"]][which(staff[["agent"]]==agent)])=="Femme",
                          "woman","man")
      doc_inconsistencies <- write.dataInconsistencies(type = data_type,evaluationLevel = "agent",
                                                       startdate = startdate,enddate = enddate,
                                                       agentList = agent,
                                                       title="Liste des potentielles incohérences")
      document <- read_docx(path=template_report) %>%
        body_add_flextable(doc_header) %>%
        body_add_par(value="",style="Normal") %>%
        body_add_par(value = "Statistiques", style = "heading 1")
      
      fileout <- tempfile(fileext = ".docx")
      print(doc_statistics,target = fileout)
      
      document <- document %>%
        body_add_docx(src=fileout) %>%
        body_add_par(value="",style="Normal") # %>%
        # body_end_block_section(value = portrait) %>%
        
      
      fileout <- tempfile(fileext = ".docx")
      print(doc_inconsistencies,target = fileout)
      
      document <- document %>%
        body_add_docx(src=fileout) %>%
        body_add_par(value="",style="Normal") %>%
        body_end_block_section(value = landscape)
      
      print(document,target=filename)
      cat(sprintf("\nPFS > Sauvegarde du rapport %s",filename))
    }
  }
}

# Production des rapports d'evaluations par superviseur et sur une periode indique
# write.supervisorInfos(unique(staff[["supervisor_name"]]),startdate=param$startdate,enddate=param$enddate)
write.supervisorInfos <- function(supervisorList,startdate=NULL,enddate=NULL,graph_onescreen=TRUE) {
  
  supervisors <- unique(staff[["supervisor_name"]])
  
  if(!any(supervisorList %in% supervisors)){
    tmp <- supervisorList[!supervisorList %in% supervisors]
    cat(sprintf("[PFS] Les noms suivants ne figurent pas dans la liste des superviseurs de PFS :  %s.",
                paste(tmp,collapse = ", ")))
  }else{
    
    portrait <- block_section(
      prop_section(
        page_size = page_size(orient = "portrait"),
        type = "continuous"
      ))
    
    landscape <- block_section(
      prop_section(
        page_size = page_size(orient = "landscape"),
        type = "nextPage"
      ))
    
    for(supervisor in supervisorList){
      filename <- sprintf("%s/%s.docx",dir_supervisor,supervisor)
      if(file.exists(filename)) invisible(file.remove(filename))
      
      cat(sprintf("\nPFS > Evaluation du travail du superviseur %s.",supervisor))
      doc_header  <- write.documentHeader(type="supervisor",startdate=startdate,enddate=enddate,
                                          supervisorList=supervisor)
      doc_statistics <- write.dataStatistics(type="supervisor",startdate=startdate,enddate=enddate,
                                             supervisorList = supervisor,graph_onescreen=graph_onescreen)
      doc_inconsistenciesF <- write.dataInconsistencies(type = "woman",evaluationLevel = "supervisor",
                                                        startdate = startdate,enddate = enddate,
                                                        supervisorList = supervisor,
                                                        title="Liste des potentielles incohérences",
                                                        subtitle="Enquêtrices")
      doc_inconsistenciesH <- write.dataInconsistencies(type = "man",evaluationLevel = "supervisor",
                                                        startdate = startdate,enddate = enddate,
                                                        supervisorList = supervisor,
                                                        subtitle="Enquêteurs")
      document <- read_docx(path=template_report) %>%
        body_add_flextable(doc_header) %>%
        body_add_par(value="",style="Normal") %>%
        body_add_par(value = "Statistiques", style = "heading 1")
      
      fileout <- tempfile(fileext = ".docx")
      print(doc_statistics,target = fileout)
      document <- document %>%
        body_add_docx(src=fileout) %>%
        body_add_par(value="",style="Normal") # %>%
      # body_end_block_section(value = portrait) %>%
      
      fileout <- tempfile(fileext = ".docx")
      print(doc_inconsistenciesF,target = fileout)
      document <- document %>%
        body_add_docx(src=fileout) %>%
        body_add_par(value="",style="Normal")
        
      fileout <- tempfile(fileext = ".docx")
      print(doc_inconsistenciesH,target = fileout)
      document <- document %>%
        body_add_docx(src=fileout) %>%
        body_add_par(value="",style="Normal") %>%
        body_end_block_section(value = landscape)
      
      print(document,target=filename)
      cat(sprintf("\nPFS > Sauvegarde du rapport %s",filename))
    }
  }
}

# Production des rapports d'evaluations par village et sur une periode indique
# write.townInfos(townList= unique(cases[["id_town"]]),startdate=param$startdate,enddate=param$enddate)
# PB avec => BARDE NDOKAYO
write.townInfos <- function(townList,startdate=NULL,enddate=NULL,graph_onescreen=TRUE) {
  
  villages <- unique(c(df[["id_town"]],dh[["id_town"]]))
  # nomVillages <- unique(c(sprintf("%d_%s",df[["id_town"]],df[["town"]]),
  #                         sprintf("%d_%s",dh[["id_town"]],dh[["town"]])))
  # nomVillages <- str_sort(nomVillages)
  # names(nomVillages) <- sprintf("ID%03d",map(str_split(nomVillages,pattern = "_"),pluck,1) %>% unlist() %>% strtoi())
  
  if(!any(townList %in% villages)){
    tmp <- unique(towns$town[towns$id_town %in% townList[!townList %in% villages]])
    cat(sprintf("[PFS] Les noms suivants ne figurent pas dans la liste des villages du PFS :  %s.",
                paste(tmp,collapse = ", ")))
  }else{
    
    portrait <- block_section(
      prop_section(
        page_size = page_size(orient = "portrait"),
        type = "continuous"
      ))
    
    landscape <- block_section(
      prop_section(
        page_size = page_size(orient = "landscape"),
        type = "nextPage"
      ))
    
    for(village in townList){
      village_name <- sprintf("%s",towns$town[towns$id_town==village])
      filename <- sprintf("%s/%s_%s.docx",dir_tow,village,village_name)
      if(file.exists(filename)) invisible(file.remove(filename))
      
      cat(sprintf("\nPFS > Evaluation du travail effectué dans le village %s (Code %s).",village_name,village))
      doc_header  <- write.documentHeader(type="town",startdate=startdate,enddate=enddate,
                                          townList=village)
      doc_statistics <- write.dataStatistics(type="town",startdate=startdate,enddate=enddate,
                                             townList = village,graph_onescreen=graph_onescreen)
      doc_inconsistenciesF <- write.dataInconsistencies(type = "woman",evaluationLevel = "town",
                                                        startdate = startdate,enddate = enddate,
                                                        townList = village,
                                                        title="Liste des potentielles incohérences",
                                                        subtitle="Enquêtrices")
      doc_inconsistenciesH <- write.dataInconsistencies(type = "man",evaluationLevel = "town",
                                                        startdate = startdate,enddate = enddate,
                                                        townList = village,
                                                        subtitle="Enquêteurs")
      document <- read_docx(path=template_report) %>%
        body_add_flextable(doc_header) %>%
        body_add_par(value="",style="Normal") %>%
        body_add_par(value = "Statistiques", style = "heading 1")
      
      fileout <- tempfile(fileext = ".docx")
      print(doc_statistics,target = fileout)
      document <- document %>%
        body_add_docx(src=fileout) %>%
        body_add_par(value="",style="Normal") # %>%
      # body_end_block_section(value = portrait) %>%
      
      fileout <- tempfile(fileext = ".docx")
      print(doc_inconsistenciesF,target = fileout)
      document <- document %>%
        body_add_docx(src=fileout) %>%
        body_add_par(value="",style="Normal")
      
      fileout <- tempfile(fileext = ".docx")
      print(doc_inconsistenciesH,target = fileout)
      document <- document %>%
        body_add_docx(src=fileout) %>%
        body_add_par(value="",style="Normal") %>%
        body_end_block_section(value = landscape)
      
      print(document,target=filename)
      cat(sprintf("\nPFS > Sauvegarde du rapport %s",filename))
    }
  }
}

# Production du rapport d'activite d'ensemble
write.pfsInfos <- function(startdate=NULL,enddate=NULL){
  
  # Nom du fichier contenant le rapport d'activite
  filename <- sprintf("%s/PFS.docx",dir_reporting)
  if(file.exists(filename)) invisible(file.remove(filename))
  cat("\n Writing ",filename," ....")
  
  portrait <- block_section(
    prop_section(
      page_size = page_size(orient = "portrait"),
      type = "continuous"
    ))
  
  landscape <- block_section(
    prop_section(
      page_size = page_size(orient = "landscape"),
      type = "nextPage"
  ))
  
  document <- read_docx(path=template_report) %>%
    body_add_flextable(write.documentHeader(type="survey",startdate=startdate,enddate=enddate)) %>%
    body_add_par(value="",style="Normal") %>%
    body_add_par(value = "Statistiques", style = "heading 1") %>%
    body_add_par(value="",style="Normal") %>%
    body_add_par(value = "Enquête Homme", style = "heading 2")
  
  ft <- flextable(getGlobalStatistics(dh,startdate = startdate,enddate = enddate,
                                      include.inconsistencies = TRUE),
                  col_keys = c("label","value")) %>%
    delete_part(part="header") %>% # suppression de l'entête
    border_inner_h(border = fp_border(color = "black", width=1.5)) %>%
    bold(j=1,part = "body") %>%
    width(j=1,width=1.87) %>%
    width(j=2,width=4.43) %>%
    hline_bottom(border = fp_border(color = "white", width=1.5))
  document <- document %>%
    body_add_par(value="",style="Normal") %>%
    body_add_flextable(ft)
  
  ft <- flextable(getGlobalStatistics(df,startdate = startdate, enddate = enddate,
                                      include.inconsistencies = TRUE),
                  col_keys = c("label","value")) %>%
    delete_part(part="header") %>% # suppression de l'entête
    border_inner_h(border = fp_border(color = "black", width=1.5)) %>%
    bold(j=1,part = "body") %>%
    width(j=1,width=1.87) %>%
    width(j=2,width=4.43) %>%
    hline_bottom(border = fp_border(color = "white", width=1.5))
  document <- document %>% 
    body_add_par(value="",style="Normal") %>%
    body_add_par(value = "Enquête Femme", style = "heading 2") %>%
    body_add_par(value="",style="Normal") %>%
    body_add_flextable(ft)
  
  document <- document %>% 
    body_add_par(value="",style="Normal") %>%
    body_end_block_section(value = portrait) %>%
    body_add_par(value = "Evolution de la collecte de données", style = "heading 2")
  
  doctmp <- read_docx()
  g1 <- survey.drawDataCollectEvolution(startdate = startdate,enddate = enddate, completeForm = FALSE)
  
  fileout <- tempfile(fileext = ".png")
  ggsave(fileout,plot=g1,device = "png",width = 14,height = 7)
  doctmp <- doctmp %>%
    body_add_par(value="",style="Normal") %>%
    body_add_img(fileout,height = 4.7, width = 9.5, style = "centered")
  
  g2 <- survey.drawDataCollectEvolution(startdate = startdate,enddate = enddate, completeForm = TRUE)
  fileout <- tempfile(fileext = ".png")
  ggsave(fileout,plot=g2,device = "png",width = 14,height = 7)
  doctmp <- doctmp %>%
    body_add_par(value="",style="Normal") %>%
    body_add_img(fileout,height = 4.7, width = 9.5, style = "centered")
  
  
  fileout <- tempfile(fileext = ".docx")
  print(doctmp,target = fileout)
  
  document <- document %>% 
    body_add_docx(src=fileout) #%>%
    # body_add_par(value="",style="Normal") %>%
    # body_end_block_section(value = landscape)
  
  villages <- unique(c(dh[["id_town"]],df[["id_town"]]))
  total <- ceiling(length(villages)/10)
  g1 <- g2 <- NULL
  doctmp <- read_docx()
  # incr <- 0
  for(i in 0:total){
    numList <- c(1:10)+(i*10)
    numList <- numList[which(numList<=length(villages))]
    if(length(numList)){
      if(i%%2==0){
        g1 <- survey.drawTrendbyTown(townList = villages[numList],completeForm=TRUE)
      }else{
        g2 <- survey.drawTrendbyTown(townList = villages[numList],completeForm=TRUE)
        
        fileout <- tempfile(fileext = ".png")
        ggsave(fileout,plot=g1+g2,device = "png",width = 14,height = 7)
        doctmp <- doctmp %>%
          body_add_par(value="",style="Normal") %>%
          body_add_img(fileout,height = 4.7, width = 9.5, style = "centered")
        # body_add_gg(value=g1+g2,width = 14,height = 7,style = "centered")
      }
    }
  }
  if(length(numList) & i%%2==0){
    fileout <- tempfile(fileext = ".png")
    ggsave(fileout,plot=g1,device = "png",width = 14,height = 7)
    doctmp <- doctmp %>%
      body_add_par(value="",style="Normal") %>%
      body_add_img(fileout,height = 4.7, width = 9.5, style = "centered")
      # body_add_gg(value=g1,width = 14,height = 7,style="centered")
  }
  
  fileout <- tempfile(fileext = ".docx")
  print(doctmp,target = fileout)
  
  if(!is.null(g1)){
    document <- document  %>%
      # body_add_par(value="",style="Normal") %>%
      # body_end_block_section(value = portrait) %>%
      # body_add_par(value="",style="Normal") %>%
      body_add_par(value = "Listing par village", style = "heading 2") %>%
      body_add_docx(src=fileout) %>%
      body_add_par(value="",style="Normal") %>%
      body_end_block_section(value = landscape) 
  }
  
  document <- document %>% 
    body_add_par(value = "Potentielles incohérences", style = "heading 1") %>%
    body_add_par(value="",style="Normal") %>%
    body_add_par(value = "Enquête Homme", style = "heading 2")
  
  #Enquete homme...
  result <- getIDFormWithPotentialInconsistencies(dh,startdate=startdate,enddate=enddate)
  ft <- flextable(result$value,col_keys = c("lab","value")) %>%
    delete_part(part="header") %>% # suppression de l'entête
    border_inner_h(border = fp_border(color = "black", width=1.5)) %>%
    bold(j=1,part = "body") %>%
    width(j=1,width=1.87) %>%
    width(j=2,width=4.43) %>%
    hline_bottom(border = fp_border(color = "white", width=1.5)) %>%
    theme_zebra()
  
  document <- document %>%
    body_add_par(value="",style="Normal") %>% 
    body_add_par(value=result$msg,style="Normal") %>%
    body_add_par(value="",style="Normal") %>% 
    body_add_flextable(ft)
  
  #Enquete femme...
  document <- document %>% 
    body_add_par(value="",style="Normal") %>%
    body_add_par(value = "Enquête Femme", style = "heading 2")
  
  result <- getIDFormWithPotentialInconsistencies(df,startdate=startdate,enddate=enddate)
  ft <- flextable(result$value,col_keys = c("lab","value")) %>%
    delete_part(part="header") %>% # suppression de l'entête
    border_inner_h(border = fp_border(color = "black", width=1.5)) %>%
    bold(j=1,part = "body") %>%
    width(j=1,width=1.87) %>%
    width(j=2,width=4.43) %>%
    hline_bottom(border = fp_border(color = "white", width=1.5)) %>%
    theme_zebra()
  
  document <- document %>%
    body_add_par(value="",style="Normal") %>% 
    body_add_par(value=result$msg,style="Normal") %>%
    body_add_par(value="",style="Normal") %>% 
    body_add_flextable(ft)
  
  # fileout <- tempfile(fileext = ".docx")
  # print(document, target = fileout)
  print(document,target = filename)
  cat("\n",filename," written...\n")
}

#Dans > write.dataInconsistencies completes pour chaque type d'incoherences
#le nombre de questionnaire avant cette incohenrece.

# Ecriture dans un fichier word distinct des cas en double
write.duplicateCase <- function(startdate=NULL,enddate=NULL,agentList=NULL,supervisorList=NULL, 
                                townList=NULL){

  #Complement du nom...
  prefix <- ""
  
  if(length(townList)==1 && townList %in% towns$id_town){
    towname <- towns$town[towns$id_town==townList]
    prefix <- sprintf("_%s",towname)
  }
  
  if(length(supervisorList)==1 && supervisorList %in% unique(staff$supervisor_name))
    prefix <- sprintf("%s_%s",ifelse(length(prefix),prefix,""),supervisorList)
  
  if(length(agentList)==1 && agentList %in% unique(staff$agent))
    prefix <- sprintf("%s_%s",ifelse(length(prefix),prefix,""),agentList)
  
  # Nom du fichier contenant le rapport d'activite
  filename <- sprintf("%s/Statistical summaries%s.docx",dir_reporting,prefix)
  
  # Nom du fichier contenant le rapport d'activite
  filename <- sprintf("%s/DuplicateCases%s.docx",dir_reporting,prefix)
  if(file.exists(filename)) invisible(file.remove(filename))
  
  result.dh <- check.duplicatedID(dh,startdate,enddate,agentList,supervisorList,townList)
  result.df <- check.duplicatedID(df,startdate,enddate,agentList,supervisorList,townList)
  
  document <- read_docx(path=template_report2) %>%
    body_add_par(value = "Listing des questionnaires avec de potentielles incohérences", 
                 style = "heading 1") %>%
    body_add_par(value="",style="Normal")
  
  pgwidth <- 10
  
  if(nrow(result.dh$value)){
    
    tmp <- result.dh$value %>%
      select(caseid,town,supervisorname,agentname,starttime,key,todelete)
    
    ft <- flextable(tmp,col_keys = c("caseid","town","supervisorname","agentname","starttime","key","todelete")) %>%
      border_inner_h(border = fp_border(color = "black", width=1.5)) %>%
      bold(j=1,part = "body") %>%
      width(j=1,width=1.87) %>%
      width(j=2,width=4.43) %>%
      hline_bottom(border = fp_border(color = "white", width=1.5))
    
    ft <- autofit(ft)
    ft <- width(ft,width = dim(ft)$widths*pgwidth/(flextable_dim(ft))$widths)
    ft <- theme_zebra(ft)
    
    document <- document %>%
      body_add_par(value = "Enquête homme", style = "heading 2") %>%
      body_add_par(value="",style="Normal") %>%
      body_add_par(value=paste0(c("Bien vouloir inscrire dans la colonne << todelete >> la valeur << Oui >> ou << Non >>. ",
                                  "Utilisez la valeur se trouvant dans la colonne << key >> pour ouvrir le questionnaire. ",
                                  "Pour ce faire : "),
                                collapse ="" ),
                   style="Normal") %>%
      body_add_par(value="",style = "Normal") %>%
      body_add(block_list(
        fpar(ftext("1. Rendez-vous sur la plateforme")),
        fpar(ftext("2. Allez sur l'onglet << Monitor>>")),
        fpar(ftext("3. Cliquez sur << Look up by key >> sur << Questionnaire Homme>>")),
        fpar(ftext("4. Insérez les << key >> des questionnaires que vous voulez visualiser en les séparant par des virgules")),
        fpar(ftext("5. Cliquez sur << View submissions >>")),
        fpar(ftext("6. Parcourez les questionnaires et indiquez dans la colonne << todelete>> quel questionnaire doit être supprimé et lequel doit être conservé."))
      )) %>%
      body_add_par(value="",style="Normal") %>%
      body_add_flextable(ft)
  }
  
  if(nrow(result.df$value)){
    
    tmp <- result.df$value %>%
      select(caseid,town,supervisorname,agentname,starttime,key,todelete)
    
    ft <- flextable(tmp,col_keys = c("caseid","town","supervisorname","agentname","starttime","key","todelete")) %>%
      border_inner_h(border = fp_border(color = "black", width=1.5)) %>%
      bold(j=1,part = "body") %>%
      width(j=1,width=1.87) %>%
      width(j=2,width=4.43) %>%
      hline_bottom(border = fp_border(color = "white", width=1.5))
    
    ft <- autofit(ft)
    ft <- width(ft,width = dim(ft)$widths*pgwidth/(flextable_dim(ft))$widths)
    ft <- theme_zebra(ft)
    
    document <- document %>%
      body_add_par(value = "Enquête femme", style = "heading 2") %>%
      body_add_par(value="",style="Normal") %>%
      body_add_par(value=paste0(c("Bien vouloir inscrire dans la colonne << todelete >> la valeur << Oui >> ou << Non >>. ",
                                  "Utilisez la valeur se trouvant dans la colonne << key >> pour ouvrir le questionnaire. ",
                                  "Pour ce faire : "),
                                collapse ="" ),
                   style="Normal") %>%
      body_add_par(value="",style = "Normal") %>%
      body_add(block_list(
        fpar(ftext("1. Rendez-vous sur la plateforme")),
        fpar(ftext("2. Allez sur l'onglet << Monitor>>")),
        fpar(ftext("3. Cliquez sur << Look up by key >> sur << Questionnaire Femme>>")),
        fpar(ftext("4. Insérez les << key >> des questionnaires que vous voulez visualiser en les séparant par des virgules")),
        fpar(ftext("5. Cliquez sur << View submissions >>")),
        fpar(ftext("6. Parcourez les questionnaires et indiquez dans la colonne << todelete>> quel questionnaire doit être supprimé et lequel doit être conservé."))
      )) %>%
      body_add_par(value="",style="Normal") %>%
      body_add_flextable(ft)
  }
  
  print(document,target = filename)
  cat(filename," written...\n")
}

# Ecriture dans un tableaux des effectifs par commune sur une periode
# Commune - Village  - Cible - Soumis - Complet
# Pour les hommes et les femmes...
write.statDocs <- function(startdate=NULL,enddate=NULL,agentList=NULL,supervisorList=NULL,
                           townList=NULL){
  #Complement du nom...
  prefix <- ""
  
  if(length(townList)==1 && townList %in% towns$id_town){
    towname <- towns$town[towns$id_town==townList]
    prefix <- sprintf("_%s",towname)
  }
  
  if(length(supervisorList)==1 && supervisorList %in% unique(staff$supervisor_name))
    prefix <- sprintf("%s_%s",ifelse(length(prefix),prefix,""),supervisorList)
  
  if(length(agentList)==1 && agentList %in% unique(staff$agent))
    prefix <- sprintf("%s_%s",ifelse(length(prefix),prefix,""),agentList)
  
  # Nom du fichier contenant le rapport d'activite
  filename <- sprintf("%s/Statistical summaries%s.docx",dir_reporting,prefix)
  if(file.exists(filename)) invisible(file.remove(filename))
  
  homme <- getTableStatistics(dh,startdate,enddate,agentList,supervisorList,townList) %>%
    select(-Complet) %>% rename(Complet=Complet0)
  
  femme <- getTableStatistics(df,startdate,enddate,agentList,supervisorList,townList) %>%
    select(-Complet) %>% rename(Complet=Complet0)
  
  document <- read_docx(path=template_report2) %>%
    body_add_par(value = "Résumés statistiques", 
                 style = "heading 1") %>%
    body_add_par(value="",style="Normal")
  
  pgwidth <- 10
  varlist <- c("Commune","Village","Soumis","Doublon","Inéligible/Incomplet","Complet","Cible","Couverture","Comment")
  
  if(nrow(homme)){
    
    tmp <- homme %>% select(one_of(varlist))
    ft <- flextable(tmp,col_keys = varlist) %>%
      border_inner_h(border = fp_border(color = "black", width=1.5)) %>%
      bold(j=1,part = "body") %>%
      width(j=1,width=1.87) %>%
      width(j=2,width=4.43) %>%
      hline_bottom(border = fp_border(color = "white", width=1.5))
    
    ft <- autofit(ft)
    ft <- width(ft,width = dim(ft)$widths*pgwidth/(flextable_dim(ft))$widths)
    ft <- theme_zebra(ft)
    
    document <- document %>%
      body_add_par(value = "Enquête homme", style = "heading 2") %>%
      body_add_par(value="",style="Normal") %>%
      body_add_flextable(ft)
  }
  
  if(nrow(femme)){
    
    tmp <- femme %>% select(one_of(varlist))
    ft <- flextable(tmp,col_keys = varlist) %>%
      border_inner_h(border = fp_border(color = "black", width=1.5)) %>%
      bold(j=1,part = "body") %>%
      width(j=1,width=1.87) %>%
      width(j=2,width=4.43) %>%
      hline_bottom(border = fp_border(color = "white", width=1.5))
    
    ft <- autofit(ft)
    ft <- width(ft,width = dim(ft)$widths*pgwidth/(flextable_dim(ft))$widths)
    ft <- theme_zebra(ft)
    
    document <- document %>%
      body_add_par(value = "Enquête femme", style = "heading 2") %>%
      body_add_par(value="",style="Normal") %>%
      body_add_flextable(ft)
  }
  print(document,target = filename)
  cat(filename," written...\n")
}

# Extraction des donnees du serveur afin de faciliter le travail 
# d'apurement. L'extension du fichier à produire doit etre avec .xlsx
extract.cases <- function(data,filename,
                          duplicateCases=TRUE,
                          startdate=NULL,enddate=NULL,agentlist=NULL,supervisorlist=NULL,townlist=NULL,
                          sheetname=NULL,keylist=NULL,idlist=NULL,varlist=NULL){
  
  varnames <- c("key","caseid2","caseid","starttime","duration_lab")
  
  start <- ifelse(is.null(startdate),min(data[["survey_date"]]),
                  ifelse(is.Date(startdate),startdate,ifelse(is.numeric(startdate),as_date(startdate),ymd(startdate))))
  end <- ifelse(is.null(enddate),max(data[["survey_date"]]),
                ifelse(is.Date(enddate),enddate,ifelse(is.numeric(enddate),as_date(enddate),ymd(enddate))))
  
  if(is.null(agentlist)) agentlist <- unique(data[["agentname"]])
  if(is.null(supervisorlist)) supervisorlist <- unique(data[["supervisorname"]])
  if(is.null(townlist)) townlist <- unique(data[["id_town"]])
  
  if(is.null(idlist)) idlist <- unique(data[["caseid2"]])
  if(is.null(keylist)) keylist <- unique(data[["key"]])
  if(is.null(varlist)){
    varlist <- c(varnames,setdiff(names(data),varnames))
  }else{
    varlist <- c(varnames,setdiff(varlist,varnames))
  }
  
  if(duplicateCases){
    result <- check.duplicatedID(data,startdate=start,enddate=end,agentList=agentlist,
                                 supervisorList = supervisorlist,townList = townlist)
    idlist <- NULL
    if(nrow(result$value)) idlist <- unique(str_replace_all(result$value$caseid,"F-|H-",""))
  }
  
  filename <- sprintf("%s/%s",dir_reporting,filename)
  wb <- createWorkbook()
  if(file.exists(filename)) wb <- loadWorkbook(filename)
  
  sheetname <- ifelse(is.null(sheetname),
                      strftime(today(),format="%y%m%d%H%M%S"),
                      sheetname)
  if(!sheetname %in% names(wb)) addWorksheet(wb,sheetName=sheetname)
  
  
  dbase <- data %>% 
    filter(caseid2 %in% idlist,
           key %in% keylist,
           survey_date>=start & survey_date<=end,
           agentname %in% agentlist,
           supervisorname %in% supervisorlist,
           id_town %in% townlist) %>%
    select(one_of(varnames),everything()) %>% 
    to_factor() # %>% as.data.frame()
  
  deleteData(wb,sheetname,cols=1:nrow(data),rows = 1:nrow(data))
  writeData(wb,sheetname,dbase)
  freezePane(wb,sheetname,firstActiveRow=2,firstActiveCol=3)
  saveWorkbook(wb,file=filename,overwrite = TRUE)
  cat("PFS > ",filename," written...")
}

# Report des cas des doublons disponibles dans la base de donnees afin de
# faciliter le travail d'identification des doublons des superviseurs..
# extract.duplicateCases(supervisorlist = c("ASSOMO MONEMOTO ARSÈNE"),filename="listeDoublons_ASSOMO)
extract.duplicateCases <- function(filename="listeDoublons.xlsx",agentlist=NULL,
                                   supervisorlist=NULL,townlist=NULL){
  extract.cases(df,filename=filename,duplicateCases=TRUE, agentlist=agentlist,
                supervisorlist = supervisorlist,townlist=townlist,
                sheetname = "femme")
  cat("\n")
  extract.cases(dh,filename=filename,duplicateCases=TRUE,agentlist=agentlist,
                supervisorlist = supervisorlist,townlist=townlist,
                sheetname = "homme")
}

#Extraction des observations pour les bases de donnees hommes et femmes respectant certains criteres
extract.observations <- function(filename=sprintf("extract_%s.xlsx",strftime(Sys.time(),format="%y%m%d%H%M%S")),
                                 startdate=NULL,enddate=NULL,agentlist=NULL,supervisorlist=NULL,townlist=NULL,
                                 keylist=NULL,idlist=NULL){
  extract.cases(df,filename=filename,duplicateCases=FALSE,startdate=startdate,enddate=enddate,agentlist=agentlist,
                supervisorlist = supervisorlist,townlist=townlist,keylist=keylist,idlist=idlist,sheetname = "femme")
  cat("\n")
  extract.cases(dh,filename=filename,duplicateCases=FALSE,startdate=startdate, enddate=enddate,agentlist=agentlist,
                supervisorlist = supervisorlist,townlist=townlist,keylist=keylist,idlist=idlist,sheetname = "homme")
}


# Fusion des 3 fichiers excel de monitoring dans un seul fichier excel avec 3 onglets distincts
# Report, Main Survey & Backcheck
compile.monitoringFiles <- function(filename = sprintf("Monitoring_%s.xlsx",strftime(Sys.time(),format="%y%m%d%H%M%S"))){
  
  filename <- sprintf("%s/%s",dir_monitoring,filename)
  wb <- createWorkbook()
  if(file.exists(filename)) wb <- loadWorkbook(filename)
  
  # Creation des onglets
  sheetlist <- c("Report","Main survey","Backcheck")
  for(sheetname in sheetlist){
    if(sheetname %in% names(wb)) removeWorksheet(wb, sheetname) #suppression si il existe deja..
    addWorksheet(wb,sheetName=sheetname)
  }
    
  filelist <- c("RapportTerrain.xlsx","Monitoring.xlsx","MonitoringBackcheck.xlsx")
  names(filelist) <- sheetlist #Nom des onglets
  
  # Variable utilitaire
  data <- list(NULL,NULL,NULL)
  names(data) <- sheetlist
  
  dir_list <- list.files(path_monitoring,full.names = TRUE)
  for(dir_name in dir_list){
    if(str_detect(basename(dir_name),"_")){
      cat("\nPFS > Le dossier du superviseur ", str_sub(basename(dir_name),2L), " n'a pas été traité car incomplet")
      next
    }
    cat("\nPFS > Traitement du dossier du superviseur ",basename(dir_name))
    for(sheetname in sheetlist){
      cat("\n      * Traitement du fichier ",filelist[sheetname])
      fichier <- sprintf("%s/%s",dir_name,filelist[sheetname])
      sub_wb <- loadWorkbook(fichier)
      sub_sheetnames <- "Monitoring"
      if(sheetname=="Report") sub_sheetnames <- setdiff(names(sub_wb),"Feuil2")
      for(subname in sub_sheetnames){
        cat("\n      **** Ajout des donnees de l'onglet ",subname," a la base [ ",sheetname," ]")
        dbtmp <- read.xlsx(sub_wb,sheet = subname)
        if("ID-MENAGE.DE.REMPLACEMENT" %in% names(dbtmp)){
          dbtmp$`ID-MENAGE.DE.REMPLACEMENT` <- as.character(dbtmp$`ID-MENAGE.DE.REMPLACEMENT`)
          dbtmp$`NEW.-.ID.MENAGE.REMPLACEMENT` <- as.character(dbtmp$`NEW.-.ID.MENAGE.REMPLACEMENT`)
          dbtmp$`DOUBLON.-.ID.REMPLACEMENT` <- as.character(dbtmp$`DOUBLON.-.ID.REMPLACEMENT`)
        } 
        
        if("CASEID_REMPLACANT" %in% names(dbtmp)){
          dbtmp$`CASEID_REMPLACANT` <- as.double(dbtmp$`CASEID_REMPLACANT`)
        }
        
        data[[sheetname]] <- bind_rows(data[[sheetname]],dbtmp)
      }
    }
  }
  
  for(sheetname in sheetlist){
    
    names(data[[sheetname]]) <- str_to_lower(names(data[[sheetname]]))
    names(data[[sheetname]]) <- str_replace_all(names(data[[sheetname]]),"\\-|\\.|\\?","")
    
    # Ajout des colonnes HH_ID
    # utiliser la base de donnees cases pour la fusion
    cat("\nPFS > Ajout dans la base ",sheetname," des colonnes en lien avec l'identification des menages")
    if(sheetname!="Report"){
      data[[sheetname]] <- data[[sheetname]] %>% 
        select(-newidmenageprimaire,-doublonidprimaire,-newidmenageremplacement,-doublonidremplacement) %>%
        rename(idmenage_primaire=idmenageprimaire,idmenage_remplacement=idmenagederemplacement) %>%
        drop_na(idmenage_primaire) %>%
        mutate(caseid=as.numeric(if_else(str_sub(idmenage_primaire,start = 1,end = 1)=='M',
                                         str_sub(idmenage_primaire,start = 10,end = 19),
                                         str_sub(idmenage_primaire,start = 12,end = 21))),
               caseid_remplacant=str_sub(idmenage_remplacement,start = 12,end = 21),
               caseid2 = sprintf("%s-%.0f",str_sub(cible,1,1),caseid)) %>%
        left_join(cases %>% 
                    select(caseid,HH_ID=hhid,id_region,id_division,id_council,id_town,sample)) %>%
        left_join(deployment %>% select(id_town,TOWN_ID=townID)) 
      
      if(sheetname=="Main survey") 
        data[[sheetname]] <- data[[sheetname]] %>%
          left_join(staff %>% select(agent,id_supervisor,supervisor_name),
                    by=c("agent"="agent"))
      
      if(sheetname=="Backcheck") 
        data[[sheetname]] <- data[[sheetname]] %>%
          left_join(staff %>% select(id_supervisor,supervisor_name) %>% distinct(),
                    by=c("superviseur"="supervisor_name"))
      
      
      
    }else{
      data[[sheetname]] <- data[[sheetname]] %>% 
        rename(backcheck_effectue=back_check_effectue) %>%
        mutate(F_caseid=sprintf("F-%.0f",caseid),H_caseid=sprintf("H-%.0f",caseid)) %>%
        left_join(cases %>% rename(HH_ID=hhid) %>% 
                    select(-rowid,-label,-respondent_name,-respondent_sex,-town,-id_supervisor,-supervisor_name)) %>%
        left_join(deployment %>% select(id_town,TOWN_ID=townID))
    }
    
    cat("\nPFS > Insertion de la base ",sheetname," dans le fichier de monitoring compile")
    data[[sheetname]] <- data[[sheetname]] %>% arrange(caseid)
    writeDataTable(wb,sheetname,data[[sheetname]],tableStyle = "TableStyleLight9")
    freezePane(wb,sheetname,firstActiveRow=2,firstActiveCol=1) # Fige la premiere colonne
    if("observations" %in% names(data[[sheetname]])){
      addStyle(wb, sheetname, style = createStyle(wrapText = TRUE), rows = 2:nrow(data[[sheetname]]), 
               cols = grep("observations",names(data[[sheetname]]))[1])  
    }
    # addFilter(wb, sheetname, row = 1, cols = 1:ncol(data[[sheetname]])) # Ajout des filtres pour faciliter le travail
    
    setColWidths(wb,sheetname, cols = 1:ncol(data[[sheetname]]), widths = "auto") # Largeur des colonnes automatiques
  }
  
  # Sauvegarde du fichier de monitoring..
  saveWorkbook(wb,file=filename,overwrite = TRUE)
  cat("PFS > ",filename," written...")
}
