
# Script de gestion des données du volet Screening
# HandiSSR
# Novembre 2017
# @version : 20171129_0955

#----------------------------------------------------------------------------------------------->
# Creation de variables utilitaires
#----------------------------------------------------------------------------------------------->

# Creation de l'identifiant equipes dans la base du personnel de collecte
staff$team.id <- strtoi(sprintf("%d%02d",staff$phase.code,staff$equipe))

## Nombre d'incoherence corriges par le superviseur
nb.incoherenceBySupervisor <- length(which(as.numeric(enum.controle$e_commune)==4)) +
  length(which(as.numeric(household.habitat$commune)==4))

# Nettoyage de la base enumeration des menages identifies par le superviseur comme
# incoherents
enum.household <- enum.household %>% filter(as.numeric(e_commune)!=4)

# Ajout de l'identifiant des menages dans la base de controle du denombrement
enum.controle$structid_lab <- sprintf("%s-%s-%s-%s-%s",
                                      str_sub(enum.controle$structid,1,1),
                                      str_sub(enum.controle$structid,2,3),
                                      str_sub(enum.controle$structid,4,5),
                                      str_sub(enum.controle$structid,6,8),
                                      str_sub(enum.controle$structid,9,11))
enum.controle$e_hhid <- sprintf("%s%03d",enum.controle$structid,enum.controle$id_controle)
enum.controle$e_hhid_lab <- sprintf("%s-%03d",enum.controle$structid_lab,
                                    enum.controle$id_controle)
enum.controle <- enum.controle %>% 
  filter(e_hhid %in% enum.household$e_hhid & as.numeric(e_commune)!=4)

# Ajout de l'identifiant des menages dans la base de supervision du denombrement
enum.supervision$structid_lab <- sprintf("%s-%s-%s-%s-%s",
                                      str_sub(enum.supervision$structid,1,1),
                                      str_sub(enum.supervision$structid,2,3),
                                      str_sub(enum.supervision$structid,4,5),
                                      str_sub(enum.supervision$structid,6,8),
                                      str_sub(enum.supervision$structid,9,11))
enum.supervision$e_hhid <- sprintf("%s%03d",enum.supervision$structid,
                                   enum.supervision$id_supervision)
enum.supervision$e_hhid_lab <- sprintf("%s-%03d",enum.supervision$structid_lab,
                                       enum.supervision$id_supervision)
enum.supervision <- enum.supervision %>% 
  filter(e_hhid %in% enum.household$e_hhid & as.numeric(e_commune)!=4)

## Ajout de l'identifiant menage dans la base membre de menage
household.membres$hhid <- str_sub(household.membres$hhmid,end = 14)

## Ajout des identifants utiles pour le superviseur a fin d'identifier
## facilement les menages
household.habitat$hhid_lab <- sprintf("%s-%s-%s-%s-%s-%s",
                                      str_sub(household.habitat$hhid,1,1),
                                      str_sub(household.habitat$hhid,2,3),
                                      str_sub(household.habitat$hhid,4,5),
                                      str_sub(household.habitat$hhid,6,8),
                                      str_sub(household.habitat$hhid,9,11),
                                      str_sub(household.habitat$hhid,12,14))
household.membres$hhid_lab <- sprintf("%s-%s-%s-%s-%s-%s",
                                      str_sub(household.membres$hhid,1,1),
                                      str_sub(household.membres$hhid,2,3),
                                      str_sub(household.membres$hhid,4,5),
                                      str_sub(household.membres$hhid,6,8),
                                      str_sub(household.membres$hhid,9,11),
                                      str_sub(household.membres$hhid,12,14))
household.membres$hhmid_lab <- sprintf("%s-%s-%s-%s-%s-%s-%s",
                                       str_sub(household.membres$hhmid,1,1),
                                       str_sub(household.membres$hhmid,2,3),
                                       str_sub(household.membres$hhmid,4,5),
                                       str_sub(household.membres$hhmid,6,8),
                                       str_sub(household.membres$hhid,9,11),
                                       str_sub(household.membres$hhmid,12,14),
                                       str_sub(household.membres$hhmid,15,16))

## Filtre des données menages déclarées comme incoherentes par le superviseur
household.habitat <- household.habitat %>% filter(as.numeric(commune)!=4)
household.membres <- household.membres %>% filter(as.numeric(commune)!=4)

## Suppression des membres de ménages donc les informations sur la tailles du ménages ne sont
## pas fournies...
idlist <- (household.habitat %>% filter(is.na(taille_menage)) %>% select(hhid) %>% distinct())$hhid
idlist <- idlist[which(idlist %in% household.membres$hhid)]
household.membres <- household.membres %>% filter(!(hhid %in% idlist))
rm(idlist)

## Ajustement de l'erreur au niveau des identifiants 
rowid <- which(!(household.habitat$hhid %in% enum.household$e_hhid))
if(length(rowid)>0){
  household.habitat$erreurs_signalees_1[rowid] <- household.habitat$erreurs_signalees_1[rowid] + 1
  household.habitat$erreurs_detectees_1[rowid] <- 1
  household.habitat$erreurs_justifiees_1[rowid] <- 0
}
rm(rowid)

## Ajustement des données pour les menages donc le statut n'est pas defini
rowid <- which(is.na(household.habitat$h_statut))
if(length(rowid)>0){
    household.habitat$h_statut[rowid] <- "Partiellement enquêté"
}
rm(rowid)

# Suppression des espaces  pour certaines variables
household.habitat <- household.habitat %>%
  mutate(commentaire=str_trim(commentaire))

household.membres <- household.membres %>%
  mutate(q129=str_trim(q129))

# Détection des doublons dans la base de données
if(length(which(duplicated(household.habitat$hhid)))>0){
  msg <- sprintf("Liste des hhid-doublons : %s",
                 paste(unique(household.habitat$hhid[duplicated(household.habitat$hhid)]),collapse=", "))
  stop(msg)
  rm(msg)
}

#nom_cm=paste("%d : %s",c(1:length(which(as.numeric(q113)==1))),str_trim(q110[which(as.numeric(q113)==1)]),collapse = "; ")

# Ajout de quelques variables
# Creation des variables nom_cm, phone_cm, phones_household
household.membres <- household.membres %>% 
  left_join(y=household.habitat %>% select(hhid,commentaire),
            by=c("hhid"="hhid")) %>%
  group_by(hhid) %>%
  mutate(nom_cm=str_trim(q110[which(as.numeric(q113)==1)]),
         phone_cm=q130[which(as.numeric(q113)==1)],
         phones_household=paste(sprintf("%s : %d",str_trim(q110),q130),
                                collapse = "; "))

phones_household <- str_trim(str_replace_all(
  str_replace_all(household.membres$phones_household,"99999999",replacement = "NA"),
  "( ?)([:alpha:]|[:blank:])+: NA;?",""))
household.membres$phones_household <- phones_household
rm(phones_household)

# Creation d'une variable AUTRE_DIFFICULTE pour simplifier l'affichage de
# la variable q129
q129.lab <- c("Non","Physiques ou motrices","Auditives","Visuel",
              "A communiquer","A prendre soin de soi","Intellectuelles",
              "Psychique","Autre")
# paste(q129.lab[strtoi(unlist(str_split(c("249"),"")))],collapse=", ")
household.membres <- household.membres %>% group_by(hhmid) %>%
  mutate(AUTRE_DIFFICULTE=if_else(is.na(q129),"",
                                  paste(q129.lab[strtoi(unlist(str_split(str_trim(q129),"")))],
                                        collapse = ", ")))

# Ajout de la variable avenue dans la base houosehold.membres
household.membres <- household.membres %>%
  left_join(y=enum.household[,c("e_hhid","e_commentaire")],by=c("hhid"="e_hhid")) %>% 
  rename(avenue=e_commentaire) %>%
  mutate(avenue=str_trim(avenue))

# Ajout du nom de l'agent screening ayant collecté les données
household.membres <- household.membres %>%
  left_join(y= household.habitat[,c("hhid","agent")] %>%
              left_join(y=staff[,c("id","noms")],by = c("agent"="id")) %>%
              rename(nomAgent=noms,numAgent=agent) %>% 
              mutate(nomAgent=str_trim(nomAgent)) %>% 
              select(hhid,numAgent,nomAgent),
            by=c("hhid"="hhid"))

#----------------------------------------------------------------------------------------------->
# Fonctions utilitaires
#----------------------------------------------------------------------------------------------->

# Production des erreur produits lors du denombre :: Cette fonction ne sert a rien
build.enumErrorList <- function(zdList=NULL,agentList=NULL){
  if(is.null(zdList))
    zdList <- as.character(sort(strtoi(unique(enum.household$e_zd))))
  if(is.null(agentList)|length(agentList)==0)
    agentList <- sort(unique(enum.household$e_agent[which(enum.household$e_zd %in% zdList)]))
  result <- enum.controle %>% 
    filter( (erreur_detectee==1 &erreur_justifiee!=1) & (e_zd %in% zdList & e_agent %in% agentList)) %>%
    group_by(e_agent,e_zd) %>% 
    select(e_agent,e_hhid,e_hhid_lab,structid,structid_lab,e_commune,e_zone,e_colline,
           e_zd,e_structure,id_controle) %>%
    mutate(error_list=enum.errorList[1]) %>%
    inner_join(y=enum.household[,c("e_hhid","e_nomcm","e_phonecm")],by=c("e_hhid"="e_hhid")) %>%
    arrange(e_zd,e_agent)
  invisible(result)
}

# Production des statistiques de la base de denobrement
build.enumStatistics <- function(zdList=NULL,agentList=NULL,clusterList=NULL){
  if(is.null(zdList))
    zdList <- as.character(sort(strtoi(unique(enum.household$e_zd))))
  if(is.null(agentList)|length(agentList)==0)
    agentList <- sort(unique(enum.household$e_agent[which(enum.household$e_zd %in% zdList)]))
  if(is.null(clusterList))
    clusterList <- sort(unique(staff$cluster.id[which(!is.na(staff$cluster.id) &
                                                        staff$id %in% agentList)]))
  result <- enum.household %>%
    filter((e_zd %in% zdList) & (e_agent %in% agentList)) %>%
    group_by(e_zd,e_agent) %>%
    mutate(nb_household=n(),
           startDate=min(e_startdate,na.rm = F),
           endDate=max(e_date,na.rm=F),
           surveyduration_day=endDate-startDate,
           surveyduration_min=sum(e_surveyduration,na.rm = TRUE)) %>%
    select(e_zd,e_agent,e_structure,nb_household,startDate,endDate,
           surveyduration_day,surveyduration_min) %>% distinct() %>%
    mutate(nb_struct=n()) %>%
    select(e_zd,e_agent,nb_struct,nb_household,startDate,endDate,
           surveyduration_day,surveyduration_min) %>% distinct() %>%
    left_join(y=staff,by=c("e_agent"="id")) %>% ungroup() %>%
    select(e_zd,cluster.id,e_agent,noms,nb_struct,nb_household,
           startDate,endDate,surveyduration_day,surveyduration_min) %>% 
    arrange(e_zd,cluster.id,noms) %>% filter((e_agent %in% agentList) & (cluster.id %in% clusterList))
  invisible(result)
}

# Production de la liste des menages à screener pour une ou plusieurs ZD et pour un ou plusieurs agents
build.screeningList <- function(zdList=NULL,agentList=NULL){
  if(is.null(zdList))
    zdList <- as.character(sort(strtoi(unique(enum.household$e_zd))))
  if(is.null(agentList)|length(agentList)==0)
    agentList <- sort(unique(enum.household$e_agent[which(enum.household$e_zd %in% zdList)]))
  
  id.errorList <- unique(build.enumErrorList(zdList,agentList)$e_hhid)
  result <- enum.household %>% 
    filter(!(e_hhid %in% id.errorList) & (e_zd %in% zdList) & (e_agent %in% agentList)) %>%
    arrange(e_commune,e_zone,e_colline,e_zd,e_agent,e_structure,e_nummenage) %>%
    select(e_commune,e_zone,e_colline,e_zd,e_agent,e_structure,e_nummenage,
           e_nomcm,e_phonecm,e_taillemenage,observations)
  invisible(result)
}

# Ecriture des données screening dans un fichier Excel
write.screeningList <- function(zdList=NULL,agentList=NULL){
  
  if(is.null(zdList))
    zdList <- as.character(sort(strtoi(unique(enum.household$e_zd))))
  if(is.null(agentList)|length(agentList)==0)
    agentList <- sort(unique(enum.household$e_agent[which(enum.household$e_zd %in% zdList)]))
  
  filename <- "./outputs/screeningListing.xlsx"
  dbase <- build.screeningList(zdList,agentList)
  result <- dbase %>% select(e_zd:observations)
  result$e_zd <- strtoi(result$e_zd)
  result$e_phonecm[which(result$e_phonecm==99999999)] <- NA
  
  wb <- loadWorkbook(template.screening)
  addWorksheet(wb,sheet=1,
               footer=c("FICHE DE SUPERVISION SCREENING",
                        format(Sys.time(),"%d %B %Y, %H:%M"),
                        "Page &[Page]"))
  
  value <- "COMMUNE : " 
  if(length(unique(dbase$e_commune))==1) 
    value = sprintf("COMMUNE : %s",as.character(unique((dbase$e_commune))))
  writeData(wb,sheet=1,x=value,startCol = 3,startRow = 4,rowNames=FALSE,colNames = FALSE)
  value="ZONE : "
  if(length(unique(dbase$e_zone))==1) 
    value = sprintf("ZONE : %s",as.character(unique((dbase$e_zone))))
  writeData(wb,sheet=1,x=value,startCol = 6,startRow = 4,rowNames=FALSE,colNames = FALSE)
  value="COLLINE : "
  if(length(unique(dbase$e_colline))==1) 
    value = sprintf("COLLINE : %s",as.character(unique((dbase$e_colline))))
  writeData(wb,sheet=1,x=value,startCol = 11,startRow = 4,rowNames=FALSE,colNames = FALSE)
  writeData(wb,sheet=1,x=result,startCol = 1,startRow = 7,rowNames=FALSE,colNames = FALSE)
  saveWorkbook(wb,filename,TRUE)
  cat(filename," recorded")
}

# Informations clés sur un ou plusieurs menages
screening.infosHousehold <- function(zdList=NULL,agentList=NULL,clusterList=NULL,hhidList=NULL){
  
  if(is.null(zdList))
    zdList <- as.character(sort(strtoi(unique(household.habitat$zd))))
  if(is.null(agentList)|length(agentList)==0)
    agentList <- sort(unique(household.habitat$agent[which(household.habitat$zd %in% zdList)]))
  if(is.null(clusterList))
    clusterList <- sort(unique(staff$cluster.id[which(!is.na(staff$cluster.id) &
                                                        staff$id %in% agentList)]))
  if(is.null(hhidList)){
    hhidList <- household.habitat %>% 
      filter((zd %in% zdList) & ((agent %in% agentList))) %>% #|(agent02 %in% agentList)
      left_join(y=staff,by=c("agent"="id")) %>%
      filter((cluster.id %in% clusterList) & ((agent %in% agentList))) %>% #|(agent02 %in% agentList)
      select(hhid) %>% arrange(hhid)
    hhidList <- str_sort(unique(as.character(hhidList$hhid)),numeric = TRUE)
  }
  result <- household.habitat %>%
    filter((hhid %in% hhidList) & (zd %in% zdList) & 
             (agent %in% agentList)) %>% #| agent02 %in% agentList
    left_join(y=staff,by=c("agent"="id")) %>%
    filter((cluster.id %in% clusterList) & ((agent %in% agentList))) %>% #|(agent02 %in% agentList)
    rename(nb_residentP=q010_3,nb_residentA=q011_3,nb_visiteurs=q012_3,nb_ph=q014_3) %>% 
    mutate(erreurs_signalees=erreurs_signalees_1+erreurs_signalees_2+erreurs_signalees_3+
             erreurs_signalees_4+erreurs_signalees_5+erreurs_signalees_6+erreurs_signalees_7+
             erreurs_signalees_8+erreurs_signalees_9,
           commentaire = str_trim(commentaire),
           error01= (erreurs_detectees_1==1 & erreurs_justifiees_1==0),
           error01_lab = if_else(error01,household.errorList[1],NULL),
           error02= (erreurs_detectees_2==1 & erreurs_justifiees_2==0),
           error02_lab = if_else(error02,household.errorList[2],NULL),
           error03= (erreurs_detectees_3==1 & erreurs_justifiees_3==0),
           error03_lab = if_else(error03,household.errorList[3],NULL),
           error04= (erreurs_detectees_4==1 & erreurs_justifiees_4==0),
           error04_lab = if_else(error04,household.errorList[4],NULL),
           error05= (erreurs_detectees_5==1 & erreurs_justifiees_5==0),
           error05_lab = if_else(error05,household.errorList[5],NULL),
           error06= (erreurs_detectees_6==1 & erreurs_justifiees_6==0),
           error06_lab = if_else(error06,household.errorList[6],NULL),
           error07= (erreurs_detectees_7==1 & erreurs_justifiees_7==0),
           error07_lab = if_else(error07,household.errorList[7],NULL),
           error08= (erreurs_detectees_8==1 & erreurs_justifiees_8==0),
           error08_lab = if_else(error08,household.errorList[8],NULL),
           error09= (erreurs_detectees_9==1 & erreurs_justifiees_9==0),
           error09_lab = if_else(error09,household.errorList[9],NULL),
           nb_error= error01+error02+error03+error04+error05+error06+error07+error08+error09,
           tx_incoherence = round(100*nb_error/length(household.errorList),1),
           error = (nb_error>=1) | (as.numeric(isunlocked)==1 & as.numeric(ischecked)==2),
           error_lab=if_else(error,"A corriger","Correct"),
           error_list=str_c(str_replace_na(error01_lab),str_replace_na(error02_lab),
                            str_replace_na(error03_lab),str_replace_na(error04_lab),
                            str_replace_na(error05_lab),str_replace_na(error06_lab),
                            str_replace_na(error07_lab),str_replace_na(error08_lab),
                            str_replace_na(error09_lab),sep=", "),
           error_list=str_replace_all(error_list,", NA",""),
           error_list=str_replace_all(error_list,"NA, ",""),
           error_list=str_replace_all(error_list,"NA",""),
           error_list=if_else(error,error_list,"")) %>%
    select(hhid,hhid_lab,commune:start_date,date,start_time,end_time,survey_duration,
           nb_residentP,nb_residentA,nb_visiteurs,nb_ph,taille_menage,h_statut,
           commentaire,isclosed,error,error_lab,error_list,erreurs_signalees,tx_incoherence,
           noms,team.id,cluster.id)
    
  invisible(result)  
}

# paste(c(error01_lab,error02_lab,error03_lab,error04_lab,
#         error05_lab,error06_lab,error07_lab,error08_lab,
#         error09_lab)[c(error01_lab,error02_lab,error03_lab,error04_lab,
#                        error05_lab,error06_lab,error07_lab,error08_lab,
#                        error09_lab)!=""],collapse = ", ")

#Informations sur ZD
screening.infosZD <- function(zdList=NULL,by.agent=FALSE){
  # on utiliser la base produite par infos.household pour calculer les différentes statistiques
  # propre à une ZD que l'on pourra regrouper par agent ou un cluster donnée
  if(is.null(zdList))
    zdList <- as.character(sort(strtoi(unique(household.habitat$zd))))
  
  dbtmp <- screening.infosHousehold(zdList = zdList)
  if(by.agent){
    dbtmp <- dbtmp %>% group_by(zd,agent)
  }else{
    dbtmp <- dbtmp %>% group_by(zd)
  }
  result <- dbtmp %>%
    summarise(zd_agent= str_c(unique(noms),collapse =", "),
              zd_team=str_c(unique(team.id),collapse =", "),
              zd_cluster=str_c(unique(cluster.id),collapse =", "),
              nb_household=n(),
              zd_startDate=min(start_date,na.rm = TRUE),
              zd_endDate=max(date,na.rm = TRUE),
              zd_duration_day=(zd_endDate - zd_startDate)+1, 
              zd_duration_min=sum(survey_duration,na.rm = TRUE),
              zd_nbResidentPresent=sum(nb_residentP,na.rm = TRUE),
              zd_nbResidentAbsent=sum(nb_residentA,na.rm = TRUE),
              zd_nbVisiteur=sum(nb_visiteurs,na.rm = TRUE),
              zd_nbPH_Eligible=sum(nb_ph,na.rm = TRUE),
              zd_populationTotale=sum(taille_menage,na.rm = TRUE),
              formCompleted=sum(as.numeric(h_statut)==3,na.rm = TRUE),
              formPartial=sum(as.numeric(h_statut)==2,na.rm = TRUE),
              formNotCompleted=sum(as.numeric(h_statut)==4,na.rm = TRUE),
              formClosed=sum(as.numeric(isclosed)==1,na.rm = TRUE),
              formNotClosed=sum(as.numeric(isclosed)==2,na.rm = TRUE),
              errorSignaled=sum(erreurs_signalees,na.rm=TRUE),
              formWithIncoherence=sum(error,na.rm = TRUE),
              prop.formWithIncoherence= round(100*formWithIncoherence/nb_household,1)) #%>%
    # select(zd:prop.formWithIncoherence)
  
  if(by.agent){
    result <- result %>% arrange(zd,zd_agent)
  }else{
    result <- result %>% arrange(zd)
  }  
  invisible(result)
}

#Informations sur agents
screening.infosAgent <- function(agentList=NULL,by.zd=FALSE){
  # on utilisera la base produite par infos.household pour calculer les différentes statistiques
  # propre à un agent que l'on pourra grouper par ZD
  
  if(is.null(agentList)|length(agentList)==0)
    agentList <- sort(unique(household.habitat$agent))
  
  dbtmp <- screening.infosHousehold(agentList = agentList)
  
  if(by.zd){
    dbtmp <- dbtmp %>% group_by(agent,zd)
  }else{
    dbtmp <- dbtmp %>% group_by(agent)
  }
  
  result <- dbtmp %>%
    summarise(nom_agent= str_c(unique(noms),collapse =", "),
              agent_team=str_c(unique(team.id),collapse =", "),
              agent_cluster=str_c(unique(cluster.id),collapse =", "),
              nb_household=n(),
              agent_startDate=min(start_date,na.rm = TRUE),
              agent_endDate=max(date,na.rm = TRUE),
              agent_duration_day=(agent_endDate - agent_startDate)+1, 
              agent_duration_min=sum(survey_duration,na.rm = TRUE),
              agent_nbResidentPresent=sum(nb_residentP,na.rm = TRUE),
              agent_nbResidentAbsent=sum(nb_residentA,na.rm = TRUE),
              agent_nbVisiteur=sum(nb_visiteurs,na.rm = TRUE),
              agent_nbPH_Eligible=sum(nb_ph,na.rm = TRUE),
              agent_populationTotale=sum(taille_menage,na.rm = TRUE),
              formCompleted=sum(as.numeric(h_statut)==3,na.rm = TRUE),
              formPartial=sum(as.numeric(h_statut)==2,na.rm = TRUE),
              formNotCompleted=sum(as.numeric(h_statut)==4,na.rm = TRUE),
              formClosed=sum(as.numeric(isclosed)==1,na.rm = TRUE),
              formNotClosed=sum(as.numeric(isclosed)==2,na.rm = TRUE),
              errorSignaled=sum(erreurs_signalees,na.rm=TRUE),
              formWithIncoherence=sum(error,na.rm = TRUE),
              prop.formWithIncoherence= round(100*formWithIncoherence/nb_household,1)) #%>%
    # select(agent:prop.formWithIncoherence)
  
  if(by.zd){
    result <- result %>% arrange(agent,zd)
  }else{
    result <- result %>% arrange(agent)
  }
  
  invisible(result)
}

#Informations par binome
screening.infosCluster <- function(clusterList=NULL,by.zd=TRUE){
  
  if(is.null(clusterList))
    clusterList <- sort(unique(staff$cluster.id))
  
  dbtmp <- screening.infosHousehold(clusterList = clusterList)
  
  if(by.zd){
    dbtmp <- dbtmp %>% group_by(cluster.id,zd) # commune,zone,colline,
  }else{
    dbtmp <- dbtmp %>% group_by(cluster.id)
  }
  
  result <- dbtmp %>%
    summarise(cluster_nomAgent= str_c(unique(noms),collapse =", "),
              cluster_team=str_c(unique(team.id),collapse =", "),
              nb_household=n(),
              cluster_startDate=min(start_date,na.rm = TRUE),
              cluster_endDate=max(date,na.rm = TRUE),
              cluster_duration_day=(cluster_endDate - cluster_startDate)+1, 
              cluster_duration_min=sum(survey_duration,na.rm = TRUE),
              cluster_nbResidentPresent=sum(nb_residentP,na.rm = TRUE),
              cluster_nbResidentAbsent=sum(nb_residentA,na.rm = TRUE),
              cluster_nbVisiteur=sum(nb_visiteurs,na.rm = TRUE),
              cluster_nbPH_Eligible=sum(nb_ph,na.rm = TRUE),
              cluster_populationTotale=sum(taille_menage,na.rm = TRUE),
              formCompleted=sum(as.numeric(h_statut)==3,na.rm = TRUE),
              formPartial=sum(as.numeric(h_statut)==2,na.rm = TRUE),
              formNotCompleted=sum(as.numeric(h_statut)==4,na.rm = TRUE),
              formClosed=sum(as.numeric(isclosed)==1,na.rm = TRUE),
              formNotClosed=sum(as.numeric(isclosed)==2,na.rm = TRUE),
              errorSignaled=sum(erreurs_signalees,na.rm=TRUE),
              formWithIncoherence=sum(error,na.rm = TRUE),
              prop.formWithIncoherence= round(100*formWithIncoherence/nb_household,1)) #%>%
    # select(cluster.id:prop.formWithIncoherence)
  if(by.zd){
    result <- result %>% arrange(cluster.id,zd)
  }else{
    result <- result %>% arrange(cluster.id)
  }
  invisible(result)
}

# Informations sur equipe d'agent
screening.infosTeam<- function(teamList=NULL,by.zd=TRUE){
  if(is.null(teamList))
    teamList <- sort(unique(staff$team.id))
  clusterList <- sort(unique((staff %>% filter(team.id %in% teamList))$cluster.id))
  
  dbtmp <- screening.infosHousehold(clusterList = clusterList)
  
  if(by.zd){
    dbtmp <- dbtmp %>% group_by(team.id,zd) # commune,zone,colline,
  }else{
    dbtmp <- dbtmp %>% group_by(team.id)
  }
  
  result <- dbtmp %>%
    summarise(team_nomAgent= str_c(unique(noms),collapse =", "),
              nb_household=n(),
              team_startDate=min(start_date,na.rm = TRUE),
              team_endDate=max(date,na.rm = TRUE),
              team_duration_day=(team_endDate - team_startDate)+1, 
              team_duration_min=sum(survey_duration,na.rm = TRUE),
              team_nbResidentPresent=sum(nb_residentP,na.rm = TRUE),
              team_nbResidentAbsent=sum(nb_residentA,na.rm = TRUE),
              team_nbVisiteur=sum(nb_visiteurs,na.rm = TRUE),
              team_nbPH_Eligible=sum(nb_ph,na.rm = TRUE),
              team_populationTotale=sum(taille_menage,na.rm = TRUE),
              formCompleted=sum(as.numeric(h_statut)==3,na.rm = TRUE),
              formPartial=sum(as.numeric(h_statut)==2,na.rm = TRUE),
              formNotCompleted=sum(as.numeric(h_statut)==4,na.rm = TRUE),
              formClosed=sum(as.numeric(isclosed)==1,na.rm = TRUE),
              formNotClosed=sum(as.numeric(isclosed)==2,na.rm = TRUE),
              errorSignaled=sum(erreurs_signalees,na.rm=TRUE),
              formWithIncoherence=sum(error,na.rm = TRUE),
              prop.formWithIncoherence= round(100*formWithIncoherence/nb_household,1)) #%>%
  # select(cluster.id:prop.formWithIncoherence)
  if(by.zd){
    result <- result %>% arrange(team.id,zd)
  }else{
    result <- result %>% arrange(team.id)
  }
  invisible(result)
}

# Informations sur le screening produit dans un fichier Excel
write.screeningExcel <- function(zdList=NULL) {
  
  filename <- "./outputs/Screening.xlsx"
  
  if(is.null(zdList))
    zdList <- as.character(sort(strtoi(unique(household.habitat$zd))))
  
  ## Creation d'un nouveau classeur
  wb <- createWorkbook("Screening")
  
  addWorksheet(wb,"Denombrement")
  dbtmp <- build.enumStatistics(zdList = zdList)
  writeDataTable(wb,1,dbtmp)
  
  addWorksheet(wb,"Incoherences-Denombrement")
  dbtmp <- build.enumErrorList(zdList = zdList)
  writeDataTable(wb,2,dbtmp)
  
  addWorksheet(wb,"Menages")
  dbtmp <- screening.infosHousehold(zdList = zdList)
  writeDataTable(wb,3,dbtmp)
  
  addWorksheet(wb,"Incoherences-Screening")
  writeDataTable(wb,4,(dbtmp %>% filter(error)))
  
  addWorksheet(wb,"Equipes")
  agentList <- (household.habitat %>% 
                  filter(zd %in% zdList) %>%
                  inner_join(y=staff,by=c("agent"="id")) %>% 
                  arrange(noms) %>% select(agent,noms) %>% distinct())$agent
  
  teamList <- sort(unique(staff$team.id[which(staff$id %in% agentList)]))
  dbtmp <- screening.infosTeam(teamList)
  writeDataTable(wb,5,dbtmp)
  
  addWorksheet(wb,"Binomes")
  clusterList <- sort(unique(staff$cluster.id[which(staff$id %in% agentList)]))
  dbtmp <- screening.infosCluster(clusterList)
  writeDataTable(wb,6,dbtmp)
  
  # Ajout pour chaque agent
  sheet <- 7
  for(codeagent in agentList){
    nameAgent <- as.character((staff %>% filter(id==codeagent))$noms)
    addWorksheet(wb,nameAgent)
    dbtmp <- screening.infosAgent(codeagent,TRUE)
    writeDataTable(wb,sheet,dbtmp)
    sheet <- sheet + 1
  }
  saveWorkbook(wb,filename,TRUE)
  cat(filename," recorded \n")
}

# Informations surle screening produit dans un document word
write.screeningWord <- function(zdList=NULL,filename=NULL) {
  
  if(is.null(zdList))
    zdList <- as.character(sort(strtoi(unique(household.habitat$zd))))
  if(is.null(filename)) 
    filename <- "./outputs/handiSSR.docx"
  
  dbtmp <- screening.infosHousehold(zdList)
  
  nb.household <- dim(dbtmp)[1]
  nb.hhEnumerated <- dim(enum.household)[1]
  nb.hhPartial <- length(which(as.numeric(household.habitat$h_statut)==2))
  nb.hhRefusal <- length(which(as.numeric(household.habitat$h_statut)==4))
  pop.totale <- dim(household.membres)[1]
  nb.popEnumerated <- sum(enum.household$e_taillemenage,na.rm = TRUE)
  nb.ph <- sum(dbtmp$nb_ph,na.rm = TRUE)
  nb.ph1524 <- dim(household.membres %>% 
                  filter(as.numeric(eligible)==4 & between(q116,15,24) & 
                           (zd %in% zdList)))[1]
  nb.ph2549 <-dim(household.membres %>% 
                 filter(as.numeric(eligible)==4 & between(q116,25,49) & 
                          (zd %in% zdList)))[1]
  nb.possiblePH <- dim(household.membres %>% 
                      filter(as.numeric(eligible)==3 & 
                               !(q129 %in% c("","1","9",NA)) &
                               (zd %in% zdList)))[1]
  nb.handicapmajeur <- length(which(household.membres$handicap_majeur>0 &
                                      as.numeric(household.membres$eligible) %in% c(3,4)  &
                                      household.membres$zd %in% zdList))
  nb.handicapmineur <- length(which(household.membres$handicap_mineur>1 &
                                      household.membres$handicap_majeur==0 &
                                      as.numeric(household.membres$eligible) %in% c(3,4)  &
                                      household.membres$zd %in% zdList))
  nb.formWithIncoherence <- sum(dbtmp$error,na.rm = TRUE)
  
  tmp.01 <- enum.controle[which(enum.controle$e_zd %in% zdList),]
  tmp.02 <- household.habitat[which(household.habitat$zd %in% zdList),]
  if(dim(tmp.01)[1]==0){ # ceci est du a l'effacement des donnes côté dénombrement et pas côté screening
    day.max <- max(tmp.02$date,na.rm = TRUE)
    day.min <- min(tmp.02$start_date,na.rm = TRUE)
  }else{
    day.max <- max(max(tmp.01$e_date,na.rm = TRUE),max(tmp.02$date,na.rm = TRUE))
    day.min <- min(min(tmp.01$e_startdate,na.rm = TRUE),min(tmp.02$start_date,na.rm = TRUE))
  }
  nb.days <- strtoi(ymd(day.max) - ymd(day.min))
  
  # nb.days <- unique(c(tmp.01$e_startdate,tmp.01$e_date,
  #                     tmp.02$start_date,tmp.02$date))
  # nb.days <- length(which(!is.na(nb.days)))
  
  tmp.01 <- enum.household[which(enum.household$e_zd %in% zdList),]
  surveyduration <- round((sum(tmp.01$e_surveyduration,na.rm = TRUE) + 
                          sum(tmp.02$surveyduration,na.rm = TRUE))/60,1)
  rm(tmp.01,tmp.02)
  
  # Calcul des indicateurs sur l'ensemble
  ## Effectif des ménages par ZD
  db <- household.habitat %>% filter(zd %in% zdList) %>%
    group_by(zd) %>%
    summarise(Effectif=n(),ZD=strtoi(unique(zd))) %>%
    arrange(desc(Effectif))
  if(dim(db)[1]==0){
    tmp <- data.frame(x=seq(1,10,1),y=seq(1,10,1),valeur=NA)
    g1 <- tmp %>% ggplot(aes(x,y)) + 
      annotate(geom ="text",x=5,y=5,label="Pas de données disponible") +
      theme_void() + 
      theme(text=element_text(face="bold"),
            panel.border=element_rect(linetype="dashed",fill = NA))
    rm(tmp)
  }else{
    g1 <- db %>% ggplot(mapping = aes(x=ZD,y=Effectif)) + 
      geom_bar(aes(fill=Effectif),stat="identity") +
      scale_x_discrete("ZD",limits=db$ZD) + 
      labs(y="Nombre de ménage",
           title=sprintf("Effectif des ménages : %d pour %d dénombrés",nb.household,nb.hhEnumerated),
           subtitle=sprintf("Population totale : %d pour %d dénombrées",pop.totale,nb.popEnumerated),
           caption=sprintf("%d ménages incomplets; %d refus",nb.hhPartial,nb.hhRefusal))+
      coord_flip() + theme_linedraw() +
      geom_text(aes(label=Effectif),hjust=1.5,colour="white", fontface="bold",size=3.5) +
      theme(axis.title=element_text(face="bold"),
            plot.title=element_text(face="bold"))
  }
  
  ## Courbe d'integration des PH par jour de collecte
  db <- household.membres %>% filter(zd %in% zdList) %>%
    left_join(y=household.habitat,by = c("hhid"="hhid")) %>%
    group_by(date) %>%
    summarise(axis.x=ymd(as.character(unique(date))),
              PH_Washington.Group=sum(as.numeric(eligible)==4,na.rm = TRUE),
              PH_Q129=sum(as.numeric(eligible)==3 & !(q129 %in% c("","1","9",NA)),na.rm = TRUE)) %>% 
    gather(PH_Washington.Group,PH_Q129,key="statut",value="valeur") %>%
    arrange(desc(statut)) %>% 
    mutate(statut=str_replace(str_replace(statut,"_","-"),"\\."," "))
  db$statut <- factor(db$statut,levels=unique(db$statut),ordered = FALSE)
  if(dim(db)[1]==0){
    tmp <- data.frame(x=seq(1,10,1),y=seq(1,10,1),valeur=NA)
    g2 <- tmp %>% ggplot(aes(x,y)) + 
      annotate(geom ="text",x=5,y=5,label="Pas de données disponible")+
      theme_void() + 
      theme(text=element_text(face="bold"),
            panel.border=element_rect(linetype="dashed",fill = NA))
    rm(tmp)
  }else{
    g2 <- db %>% 
      ggplot(mapping = aes(x=axis.x,y=valeur,linetype=statut)) + 
      geom_line() + #geom_point() +
      labs(x="Date de collecte", y="Nombre de PH",
           title=sprintf("PH-Washington = %02d vs PH-Q129=%02d",nb.ph,nb.possiblePH),
           subtitle=sprintf("PH-Majeur = %02d ; PH-Mineur éligible = %02d",nb.handicapmajeur,nb.handicapmineur),
           caption=sprintf("PH-Washington : 15-24ans = %02d ; 25-49ans = %02d",nb.ph1524,nb.ph2549),
           linetype="") + theme_minimal() + 
      theme(axis.title=element_text(face="bold"),
            plot.title=element_text(face="bold"))
  }

  ## Effectif des ménages par Agent
  db <- household.habitat %>%  filter(zd %in% zdList) %>%
    left_join(y=staff,by=c("agent"="id")) %>%
    group_by(agent,noms) %>% summarise(Effectif=n()) %>% 
    arrange(desc(Effectif))
  if(dim(db)[1]==0){
    tmp <- data.frame(x=seq(1,10,1),y=seq(1,10,1),valeur=NA)
    g3 <- tmp %>% ggplot(aes(x,y)) + 
      annotate(geom ="text",x=5,y=5,label="Pas de données disponible")+
      theme_void() + 
      theme(text=element_text(face="bold"),
            panel.border=element_rect(linetype="dashed",fill = NA))
    rm(tmp)
  }else{
    g3 <- db %>% ggplot(mapping = aes(x=noms,y=Effectif)) + 
      geom_bar(aes(fill=Effectif),stat="identity") +
      labs(x="",
           y="Nombre de ménage",
           title=sprintf("Effectif des ménages par agent"),
           subtitle=sprintf("Nombre de jour de collecte = %d",nb.days),
           caption=sprintf("Temps effectif de collecte = %.01f heures",surveyduration)) +
      geom_text(aes(label=Effectif),hjust=1.5,colour="white",fontface="bold",size=3.5) +
      coord_flip() + theme_minimal() +
      theme(axis.title=element_text(face="bold"),
            plot.title=element_text(face="bold"),
            axis.line.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  }
  
  ## Variable listant les contributions des agents au nombre d'incohérence dans l'enquête
  errorString <- fpar(ftext("",prop = fp_text()))
  
  ## Camembert : de ceux ayant le plus d'erreur dans les données à une date donnée
  db <- dbtmp %>% filter(zd %in% zdList) %>%
    mutate(Total=sum(error,na.rm=TRUE)) %>% 
    group_by(agent) %>% 
    mutate(val.absolute=sum(error,na.rm=TRUE),
           val.percent=round(100*val.absolute/Total,1)) %>% 
    select(agent,val.absolute,val.percent) %>% distinct() %>%
    left_join(y=staff,by=c("agent"="id")) %>% 
    arrange(desc(val.percent),noms)  %>%
    filter(val.absolute>0 & !is.nan(val.percent))
    
  if(dim(db)[1]==0){
    tmp <- data.frame(x=seq(1,10,1),y=seq(1,10,1),valeur=NA)
    g4 <- tmp %>% ggplot(aes(x,y)) + 
      annotate(geom ="text",x=5,y=5,label="Pas d'incohérences détectées dans les données")+
      theme_void() + 
      theme(text=element_text(face="bold"),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA),
            panel.border=element_rect(linetype="dashed",fill = NA))
    rm(tmp)
  }else{
    # db <- household.membres %>% left_join(y=household.habitat,by=c("hhid"="hhid")) %>%
    #   mutate(Total=n()) %>% group_by(agent) %>%
    #   mutate(val.abs=n(),
    #          val.percent=round(100*val.abs/Total,1)) %>%
    #   select(agent,val.percent) %>% distinct() %>%
    #   left_join(y=staff,by=c("agent"="id")) %>% arrange(noms) %>%
    #   select(agent,noms,val.percent) %>% arrange(desc(val.percent))
    g4 <- db %>% ggplot(aes(x="",y=val.percent,fill=noms)) +
      geom_bar(width=1,stat = "identity") + 
      coord_polar("y",start = 0) +
      labs(subtitle=sprintf("Questionnaire incohérents = %d",nb.formWithIncoherence)) +
      theme_minimal() + 
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA)) #+
      # geom_text(aes(y = val.percent/3+c(0, cumsum(val.percent)[-length(val.percent)])),
      #           label=sprintf("%.01f%%",db$val.percent),size=2,fontface="bold")
    
    errorLabs <- paste(str_c(db$noms,sprintf(" (%.01f%%)",db$val.percent)),collapse = ", ")
    errorString <-fpar(ftext(sprintf("Contribution de chaque agent à l'ensemble des %d incohérences détectées : ",
                                     nb.formWithIncoherence),
                             prop=fp_text(bold=TRUE,underlined = TRUE)),
                       ftext(errorLabs,fp_text()))
  }
  
  ## Ecriture des données dans le document word
  # graph <- arrangeGrob(g1,g2,g3,g4,nrow=2,ncol=2)
  # gfilename <- "./outputs/graphes_HandiSSR.png"
  # save_plot(gfilename,grid.arrange(graph),ncol=2,nrow=2,
  #           base_aspect_ratio = 1.3)
  
  gfilename.1 <- "./outputs/graphes_01.png"
  ggsave(gfilename.1,plot=g1,device = "png",width = 8,height = 8)
  gfilename.2 <- "./outputs/graphes_02.png"
  ggsave(gfilename.2,plot=g2,device = "png",width = 8,height = 8)
  
  graph <- arrangeGrob(g3,g4,nrow=1,ncol=2)
  gfilename <- "./outputs/graphes_03.png"
  save_plot(gfilename,grid.arrange(graph),ncol=2,nrow=2,base_aspect_ratio = 1.3)
  
  # gfilename.3 <- "./outputs/graphes_03.png"
  # ggsave(gfilename.3,plot=g3,device = "png",width = 8,height = 8)
  # gfilename.4 <- "./outputs/graphes_04.png"
  # ggsave(gfilename.4,plot=g4,device = "png",width = 8,height = 8)
  
  dateLab <- ftext(as.character(format(Sys.time(),"%d %B %Y - %H:%M")),
                   prop=fp_text(bold = TRUE,font.size = 12))
  read_docx() %>% 
    body_add_img(src = logo, width = 2, height = 1,style = "centered",pos="on") %>% 
    body_add_fpar(fpar(dateLab),style = "centered") %>%
    body_add_par(value = "Statistiques volet screening", 
                 style = "heading 1") %>%
    body_add_par(value="",style="Normal") %>%
    body_add_img(src = gfilename.1, width = 5, height = 7,style = "centered") %>% 
    body_add_par(value="",style="Normal") %>%
    body_add_img(src = gfilename.2, width = 5, height = 7,style = "centered") %>% 
    body_add_par(value="",style="Normal") %>%
    body_add_img(src = gfilename, width = 5, height = 7,style = "centered") %>%
    body_add_par(value="",style="Normal") %>%
    body_add_fpar(errorString) %>%
    body_add_par(value="",style="Normal") %>%body_add_par(value="",style="Normal") %>%
    body_add_par(value="",style="Normal") %>%body_add_par(value="",style="Normal") %>%
    body_add_par(value="",style="Normal") %>%body_add_par(value="",style="Normal") %>%
    body_add_par(value="",style="Normal") %>%body_add_par(value="",style="Normal") %>%
    body_add_break() %>% 
    body_add_img(src = logo, width = 2, height = 1,style = "centered",pos="on") %>% 
    body_add_fpar(fpar(dateLab),style = "centered") %>%
    body_add_par(value = "Statistiques volet handicap", 
                 style = "heading 1") %>%
    print(target = filename) %>% 
    invisible()
  if(file.exists(gfilename.1)) file.remove(gfilename.1)
  if(file.exists(gfilename.2)) file.remove(gfilename.2)
  if(file.exists(gfilename)) file.remove(gfilename)
  cat(filename," recorded \n")
  invisible(graph)
}

# Rapport d'erreurs pour chaque Agent
write.screeningErrorByAgent <- function(agentList=NULL,zdList=NULL){
  if(is.null(agentList))
    agentList <- (household.habitat %>%
                    inner_join(y=staff,by=c("agent"="id")) %>% 
                    arrange(noms) %>% select(agent,noms) %>% distinct())$agent
  if(is.null(zdList))
    zdList <-(household.habitat %>% filter(agent %in% agentList) %>%
                select(zd) %>% arrange(zd) %>% distinct())$zd
  
  filepath <- "./outputs/screening"; graphes <- list()
  db.all <- screening.infosHousehold()
  dbase <- screening.infosAgent(agentList,by.zd = TRUE) %>%
    filter((agent %in% agentList) & (zd %in% zdList))
 
  for(codeAgent in agentList){
    nom_agent <- (staff %>% filter(id==codeAgent))$noms
    filename <- sprintf("%s/%s.docx",filepath,nom_agent)
    cat(sprintf("[>] %s (codeAgent=%d)\n",nom_agent,codeAgent))
    
    dbtmp <- db.all %>% filter(zd %in% zdList & agent==codeAgent)
    
    nb.household <- dim(dbtmp)[1]
    
    df <- enum.household %>% filter(e_zd %in% zdList & e_agent==codeAgent)
    nb.hhEnumerated <- dim(df)[1]
    nb.popEnumerated <- sum(df$e_taillemenage,na.rm = TRUE)
    
    df <- (household.membres %>% 
      left_join(y=household.habitat,by=c("hhid"="hhid")) %>%
      rename(zd=zd.x)) %>% filter(zd %in% zdList && agent==codeAgent)
    
    pop.totale <- dim(df)[1]
    nb.ph <- sum(dbtmp$nb_ph,na.rm = TRUE)
    nb.ph1524 <- dim(df %>%
                    filter(as.numeric(eligible)==4 & between(q116,15,24) &
                             (zd %in% zdList) & agent==codeAgent))[1]
    nb.ph2549 <- dim(df %>%
                   filter(as.numeric(eligible)==4 & between(q116,25,49) &
                            (zd %in% zdList) & agent==codeAgent))[1]
    nb.possiblePH <- dim(df %>%
                        filter(as.numeric(eligible)==3 & 
                                 !(q129 %in% c("","1","9",NA)) &
                                 (zd %in% zdList) & 
                                 agent==codeAgent))[1]
    nb.handicapmajeur <- length(which(df$handicap_majeur>0 &
                                        as.numeric(df$eligible) %in% c(3,4) &
                                        df$zd %in% zdList &
                                        df$agent==codeAgent))
    nb.handicapmineur <- length(which(df$handicap_mineur>1 &
                                        df$handicap_majeur==0 &
                                        as.numeric(df$eligible) %in% c(3,4) &
                                        df$zd %in% zdList &
                                        df$agent==codeAgent))
    nb.hhPartial <- length(which(as.numeric(df$h_statut)==2))
    nb.hhRefusal <- length(which(as.numeric(df$h_statut)==4))
    
    nb.formWithIncoherence <- sum(dbtmp$error,na.rm = TRUE)
    
    tmp.01 <- enum.controle[which(enum.controle$e_zd %in% zdList & 
                                    enum.controle$e_agent==codeAgent),]
    tmp.02 <- household.habitat[which(household.habitat$zd %in% zdList & 
                                        household.habitat$agent==codeAgent),]
    if(dim(tmp.01)[1]==0){ # ceci est du a l'effacement des donnes côté dénombrement et pas côté screening
      day.max <- max(tmp.02$date,na.rm = TRUE)
      day.min <- min(tmp.02$start_date,na.rm = TRUE)
    }else{
      day.max <- max(max(tmp.01$e_date,na.rm = TRUE),max(tmp.02$date,na.rm = TRUE))
      day.min <- min(min(tmp.01$e_startdate,na.rm = TRUE),min(tmp.02$start_date,na.rm = TRUE))
    }
    nb.days <- strtoi(ymd(day.max) - ymd(day.min))
    
    # nb.days <- unique(c(tmp.01$e_startdate,tmp.01$e_date,
    #                     tmp.02$start_date,tmp.02$date))
    # nb.days <- length(which(!is.na(nb.days)))
    
    tmp.01 <- enum.household[which(enum.household$e_zd %in% zdList & 
                                     enum.household$e_agent==codeAgent),]
    surveyduration <- round((sum(tmp.01$e_surveyduration,na.rm = TRUE) + 
                            sum(tmp.02$surveyduration,na.rm = TRUE))/60,1)
    rm(tmp.01,tmp.02)
    
    ## Preparation des graphiques d'évaluation de la performance d'agent
    ### Effectif des ménages par ZD
    db <- dbtmp %>% filter(zd %in% zdList & agent==codeAgent) %>% # Ce filtre est inutile mais bon au cas où
      group_by(zd) %>%
      summarise(Effectif=n(),ZD=strtoi(unique(zd))) %>%
      arrange(desc(Effectif))
    if(dim(db)[1]==0){
      tmp <- data.frame(x=seq(1,10,1),y=seq(1,10,1),valeur=NA)
      g1 <- tmp %>% ggplot(aes(x,y)) + 
        annotate(geom ="text",x=5,y=5,label="Pas de données disponible")+
        theme_void() + 
        theme(text=element_text(face="bold"),
              panel.border=element_rect(linetype="dashed",fill = NA))
      rm(tmp)
    }else{
      g1 <- db %>% ggplot(mapping = aes(x=ZD,y=Effectif)) + 
        geom_bar(aes(fill=Effectif),stat="identity") +
        scale_x_discrete("ZD",limits=db$ZD) + 
        labs(y="Nombre de ménage",
             title=sprintf("Effectif des ménages : %d pour %d dénombrés",nb.household,nb.hhEnumerated),
             subtitle=sprintf("Population totale : %d pour %d dénombrées",pop.totale,nb.popEnumerated),
             caption=sprintf("%d ménages incomplets; %d refus",nb.hhPartial,nb.hhRefusal))+
        coord_flip() + theme_linedraw() +
        geom_text(aes(label=Effectif),hjust=1.5,colour="white", fontface="bold",size=3.5)
    }
    
    ## Courbe d'administration de questionnaire par agent par jour de collecte
    #   vs l'a moyenne l'effectif de l'ensemble
    db <- db.all %>% 
      group_by(date,agent) %>% mutate(total01=n()) %>% ungroup() %>%
      group_by(date) %>%
      mutate(total02=round(mean(total01,na.rm = TRUE),1),
             total03=round(median(total01,na.rm = TRUE),1)) %>% 
      # group_by(date) %>%
      filter(zd %in% zdList & agent==codeAgent) %>%
      summarise(axis.x=ymd(as.character(unique(date))),
                Agent=n(),
                Moyenne=unique(total02),
                Ensemble=sum(total01,na.rm = TRUE),
                Mediane=unique(total03)) %>% 
      gather(Agent,Moyenne,key="statut",value="valeur") 
    db$statut <- factor(db$statut,levels=unique(db$statut),ordered = FALSE)
    if(dim(db)[1]==0){
      tmp <- data.frame(x=seq(1,10,1),y=seq(1,10,1),valeur=NA)
      g2 <- tmp %>% ggplot(aes(x,y)) + 
        annotate(geom ="text",x=5,y=5,label="Pas de données disponible")+
        theme_void() + 
        theme(text=element_text(face="bold"),
              panel.border=element_rect(linetype="dashed",fill = NA))
      rm(tmp)
    }else{
      g2 <- db %>% 
        ggplot(mapping = aes(x=axis.x,y=valeur,colour=statut)) + 
        geom_line() + geom_point() +
        # geom_hline(aes(yintercept = Mediane),color="red") +
        labs(x="Date de collecte", y="Nombre de questionnaire",
             title="Courbe de tendance de collecte",
             colour="") + theme_classic() + 
        theme(axis.title=element_text(face="bold"),
              plot.title=element_text(face="bold"))
    }
    
    ## Courbe de tendance du temps moyen journalier d'administation de questionnaires 
    #  par agent par jour de collecte vs la moyenne de l'ensemble
    db <- db.all %>% 
      group_by(date) %>% 
      mutate(total01=sum(survey_duration,na.rm = TRUE),
             total02=round(mean(survey_duration,na.rm = TRUE),2),
             total03=round(median(survey_duration,na.rm = TRUE),2)) %>% 
      filter(zd %in% zdList & agent==codeAgent) %>%
      summarise(axis.x=ymd(as.character(unique(date))),
                Agent=round(mean(survey_duration,na.rm = TRUE),2),
                Moyenne=unique(total02),
                Ensemble=unique(total01),
                Mediane=unique(total03)) %>% 
      gather(Agent,Moyenne,key="statut",value="valeur") 
    db$statut <- factor(db$statut,levels=unique(db$statut),ordered = FALSE)
    if(dim(db)[1]==0){
      tmp <- data.frame(x=seq(1,10,1),y=seq(1,10,1),valeur=NA)
      g3 <- tmp %>% ggplot(aes(x,y)) + 
        annotate(geom ="text",x=5,y=5,label="Pas de données disponible")+
        theme_void() + 
        theme(text=element_text(face="bold"),
              panel.border=element_rect(linetype="dashed",fill = NA))
      rm(tmp)
    }else{
      g3 <- db %>% 
        ggplot(mapping = aes(x=axis.x,y=valeur,colour=statut)) + 
        geom_line() + geom_point() +
        labs(x="Date de collecte", y="Durée moyenne de collecte en minutes",
             title="Durée moyenne journaliére de collecte",
             subtitle="Relatif au temps d'administration d'un questionnaire",
             colour="") + theme_classic() + 
        theme(axis.title=element_text(face="bold"),
              plot.title=element_text(face="bold"))
    }
    
    # Pour graphique g4
    db <- data.frame(x=2,y=seq(5,1.5,-0.5),lab=NA)
    db$lab[1] <- sprintf("Effectif des ménages : %d pour %d dénombrés",nb.household,nb.hhEnumerated)
    db$lab[2] <- sprintf("Population totale : %d sur %d dénombrées",pop.totale,nb.popEnumerated)
    db$lab[3] <- sprintf("PH-Majeur = %d ; PH-Mineur éligible = %d",nb.handicapmajeur,nb.handicapmineur)
    db$lab[4] <- sprintf("PH-Washington = %d ; PH-Q129 = %d",nb.ph,nb.possiblePH)
    db$lab[5] <- sprintf("PH-Washington : 15-24ans = %d ; 25-49ans = %d",nb.ph1524,nb.ph2549)
    db$lab[6] <- sprintf("Questionnaires incohérents = %d",nb.formWithIncoherence)
    db$lab[7] <- sprintf("Nombre de jours de travail (dénombrement+screening) = %d",nb.days)
    db$lab[8] <- sprintf("Temps effectif de collecte (dénombrement+screening) = %.01f heures",surveyduration)
    
    g4 <- db %>% ggplot(aes(x,y,label=lab)) + xlim(1,50) + ylim(0,5) +
      geom_text(hjust=0,vjust=.5,fontface="bold") + theme_void() + 
      theme(panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA),
            panel.border=element_rect(linetype="dashed",fill = NA))
    
    ## Sauvegarde du graphe pour une utilisation future
    # graph <- arrangeGrob(g1,g2,g3,g4,nrow=2,ncol=2)
    # graphes[[sprintf("A%02d",codeAgent)]] <- graph
    # gfilename <- sprintf("%s/graphes_A%02d.png",filepath,codeAgent)
    # save_plot(gfilename,grid.arrange(graph),ncol=2,nrow=2,
    #           base_aspect_ratio = 1.3)
    
    gfilename.1 <- "./outputs/graphes_01.png"
    ggsave(gfilename.1,plot=g1,device = "png",width = 8,height = 8)
    gfilename.2 <- "./outputs/graphes_02.png"
    ggsave(gfilename.2,plot=g2,device = "png",width = 8,height = 8)
    
    graph <- arrangeGrob(g3,g4,nrow=1,ncol=2)
    gfilename <- "./outputs/graphes_03.png"
    save_plot(gfilename,grid.arrange(graph),ncol=2,nrow=2,base_aspect_ratio = 1.3)
    
    dateLab <- ftext(as.character(sprintf("Agent%02d : %s - %s",codeAgent,nom_agent,
                                          format(Sys.time(),"le, %d %B %Y à %H:%M"))),
                     prop=fp_text(bold = TRUE,font.size = 12))
    
    ## Preparation et écriture du document word
    docAgent <- read_docx() %>% 
      body_add_img(src = logo, width = 2, height = 1,style = "centered",pos="on") %>%
      # body_add_par(value = nom_agent, style = "centered") %>%
      body_add_fpar(fpar(dateLab),style = "centered") %>%
      body_add_par(value = "Statistiques agent", 
                   style = "heading 1") %>%
      body_add_par(value="",style="Normal") %>%
      # body_add_img(src = gfilename, width = 6, height = 7,
      #              style = "centered")
      body_add_img(src = gfilename.1, width = 5, height = 7,style = "centered") %>% 
      body_add_par(value="",style="Normal") %>%
      body_add_img(src = gfilename.2, width = 5, height = 7,style = "centered") %>% 
      body_add_par(value="",style="Normal") %>%
      body_add_img(src = gfilename, width = 5, height = 7,style = "centered")
    
    
    nb.error <- sum(dbase$formWithIncoherence[which(dbase$agent==codeAgent)],
                    na.rm = TRUE)
    if(nb.error>0){
      docAgent <- docAgent %>%
        body_add_break() %>% 
        body_add_img(src = logo, width = 2, height = 1,style = "centered",pos="on") %>% 
        body_add_fpar(fpar(dateLab),style = "centered") %>%
        body_add_par(value = "Listing des incohérences", 
                     style = "heading 1")
      dbError <- screening.infosHousehold(zdList,codeAgent) %>% filter(error)
      for(hhid in 1:dim(dbError)[1]){
        commune <- dbError$commune[hhid]
        zone <- dbError$zone[hhid]
        colline <- dbError$colline[hhid]
        zd <- dbError$zd[hhid]
        structure <- dbError$structure[hhid]
        num_menage <- dbError$num_menage[hhid]
        hhid_lab <- dbError$hhid_lab[hhid]
        nb_ph <- dbError$nb_ph[hhid]
        taille_menage <- dbError$taille_menage[hhid]
        start_date <- dbError$start_date[hhid]
        date <- dbError$date[hhid]
        survey_duration <- dbError$survey_duration[hhid]
        commentaire <- dbError$commentaire[hhid]
        isclosed <- dbError$isclosed[hhid]
        tx_incoherence <- dbError$tx_incoherence[hhid]
        error_list <- dbError$error_list[hhid]
        
        docAgent <- docAgent %>%
          body_add_par(value = "",style="Normal")  %>%
          body_add_fpar(fpar(ftext(sprintf("%s, %s, %s, ZD %s, Structure %d, numéro ménage %d",
                                           commune,zone,colline,zd,structure,num_menage),
                                   prop = shortcuts$fp_bold()))) %>%
          body_add_fpar(fpar(ftext(sprintf("[ %d PH éligible sur %d personnes ]",
                                           nb_ph,taille_menage),
                                   prop = shortcuts$fp_bold())))  %>%
          body_add_par(value = "",style="Normal")  %>%
          body_add_fpar(fpar(ftext(sprintf("Identifiant ménage = %s (%s)",
                                           dbError$hhid[hhid],hhid_lab),
                                   prop = fp_text())))  %>%
          body_add_fpar(fpar(ftext(sprintf("Date de début de la collecte = %s",
                                           ymd(start_date)),
                                   prop = fp_text())))  %>%
          body_add_fpar(fpar(ftext(sprintf("Date de la dernière visite = %s",
                                           ymd(date)),
                                   prop = fp_text())))  %>%
          body_add_fpar(fpar(ftext(sprintf("Durée de collecte en minute depuis le debut = %d",
                                           survey_duration),
                                   prop = fp_text())))  %>%
          body_add_fpar(fpar(ftext(sprintf("Commentaire agent : %s",commentaire),
                                   prop = fp_text())))  %>%
          body_add_fpar(fpar(ftext(sprintf("Etat du questionnaire : %s",isclosed),
                                   prop = fp_text())))  %>%
          body_add_par(value = "",style="Normal")  %>%
          body_add_fpar(fpar(ftext(sprintf("Incohérences : %s",error_list),
                                   prop = fp_text())))  %>%
          body_add_fpar(fpar(ftext(sprintf("Taux d'incoherence intra questionnaire : %.01f%%",
                                           tx_incoherence),
                                   prop = fp_text())))  %>%
          body_add_par(value = ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>",style="Normal")
      }
    }
    
    if(nb.hhPartial>0){
      
      docAgent <- docAgent %>%
        body_add_break() %>% 
        body_add_img(src = logo, width = 2, height = 1,style = "centered",pos="on") %>% 
        body_add_fpar(fpar(dateLab),style = "centered") %>%
        body_add_par(value = "Listing des ménages incomplets", 
                     style = "heading 1")
      
      dbPartial <- household.habitat %>% 
        filter(as.numeric(h_statut)==2 & zd %in% zdList & agent == codeAgent)
      
      for(i in 1:dim(dbPartial)[1]){
        commune <- dbPartial$commune[i]
        zone <- dbPartial$zone[i]
        colline <- dbPartial$colline[i]
        zd <- dbPartial$zd[i]
        structure <- dbPartial$structure[i]
        num_menage <- dbPartial$num_menage[i]
        hhid <- dbPartial$hhid[i]; hhid_lab <- dbPartial$hhid_lab[i]
        docAgent <- docAgent %>%
          body_add_par(value = "",style="Normal")  %>%
          body_add_fpar(fpar(ftext(sprintf("%s, %s, %s, ZD %s, Structure %d, numéro ménage %d",
                                           commune,zone,colline,zd,structure,num_menage),
                                   prop = shortcuts$fp_bold()))) %>%
          body_add_fpar(fpar(ftext(sprintf("Identifiant ménage = %s (%s)",hhid,hhid_lab),
                                   prop = fp_text())))  %>%
          body_add_par(value = ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>",style="Normal")
      }
    }
    
    docAgent %>%
      print(target = filename) %>% 
      invisible()
    if(file.exists(gfilename.1)) file.remove(gfilename.1)
    if(file.exists(gfilename.2)) file.remove(gfilename.2)
    if(file.exists(gfilename)) file.remove(gfilename)
    cat(filename," recorded\n")
  }  
  invisible(graphes) 
}

# Informations sur le screening
write.screeningInfos <- function(zdList=NULL,output=c("all","xlsx","docx","agent")){
  
  if(is.null(zdList))
    zdList <- as.character(sort(strtoi(unique(household.habitat$zd))))
  
  if(is.null(output)|length(output)==0){
    output <- "all"
  }else{
    output <- output[1]
  }
  
  if(output %in% c("all","xlsx")) write.screeningExcel(zdList)
  if(output %in% c("all","docx")) write.screeningWord(zdList)
  if(output %in% c("all","agent")){
    agentList <- (household.habitat %>% filter(zd %in% zdList) %>%
                    inner_join(y=staff,by=c("agent"="id")) %>% 
                    arrange(noms) %>% select(agent,noms) %>% distinct())$agent
    write.screeningErrorByAgent(agentList,zdList)
  }
  invisible()
}

save.image("HandiSSR.RData"); save.image()
