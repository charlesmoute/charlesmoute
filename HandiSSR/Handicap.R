
# Script de gestion des données du volet Handicap
# HandiSSR
# Juillet 2018
# @version : 20180609_1635

#----------------------------------------------------------------------------------------------->
# Creation de variables utilitaires
#----------------------------------------------------------------------------------------------->

# nombre de questionnaire enregistrée partiellement (categoire==NA)
# En réalité que 
# db.partialForm <- NULL 

if(exists("eligibilite") && !is.null(eligibilite)){
  # compte le nombre de questionnaire enregistrée partiellement (categoire==NA)
  # indication des identifiants avec ce cas  : retourne de la liste des identifiants
  # Suppression de toutes les informations donc eligibilite$categorie==NA
  # db.partialForm <- eligibilite %>% filter(is.na(categorie))
  # eligibilite <- eligibilite %>% filter(!is.na(categorie))
  
  # Ajout du nom de l'agent handicap ayant collecté les données
  eligibilite <- eligibilite %>%
    left_join(y=staff[,c("id","noms")],by = c("elig_agent"="id")) %>%
    rename(agentHandicap=noms) %>% mutate(agentHandicap=str_trim(agentHandicap))
}

# Statut des triplettes :
# 1.En cours, 2.Interviewé, 3. Apparié; 4. Refus/Absent; 5. Non éligible; 6.Disponible. 
# Le statut « En cours » est le statut par défaut après tirage des triplettes. 
# 
# 1. Lorsque le statut d’appariement est « En cours » alors le PH/PT est entrain 
# .. d’être interviewé ou attend d’être interviewé
# 2. Lorsque le statut d’appariement est « Interviewé » il indique que le PH a été 
# .. Enquêté mais que son témoin a un statut d’appariement  « En cours »
# 3. Lorsque le statut d’appariement est « Apparié » il indique que le PH et son PT 
# .. ont été interviewés tous les deux
# 4. Lorsque le statut d’appariement est « Refus/Absent » il indique que le PH/PT n’a 
# .. pas été interviewé ou n’a pas participé à l’enquête soit du fait d'un refus, soit 
# .. du fait d’une absence prolongée..
# 5. Le statut «  Non éligible » indique que la personne indiquée n’a pu être recrutée 
# .. comme PH/PT  de l’étude car un critère n’était pas concordant
# 6. Le statut d’appariement  «  Disponible »  est affecté automatiquement à tous les PT 
# .. ayant un statut « En cours » et dont les PH ont un statut « Apparié ». Il indique que 
# .. le PT est disponible pour l’appariement avec un autre PH.

# triplette.statut <- c("En cours","Interviewé","Apparié","Refus/Absent",
#                       "Non éligible","Disponible")

triplette.statut <- c("Enquêté(e)","Apparié(e)","En cours","Refus","Indisponible")

# trash.statut <- c("Identifiant","Categorie","Sexe","Age")
# inconsistencies.elig <- c("Identifiant non vérifé",
#                           "Numéro du sujet en double ou incorrect",
#                           "Année de naissance non vérifé")
# inconsistencies.handicap <- c("Identifiant non vérifé",
#                               "Année de naissance individu non vérifé",
#                               "Non respect critère âge PT vs âge PH",
#                               "Non respect critère sexe PT vs sexePH",
#                               "Incohérence grille ressource",
#                               "Incohérence grille préoccupation",
#                               "Incohérence sur le nombre de parents proches vivant à Bujumbura",
#                               "Incohérence sur le nombre de parents proches visités au moins 1 fois par mois",
#                               "Incohérence sur le nombre d'amis proches atteints d'un handicap",
#                               "Incohérence sur le nombre d'amis proches vivant à Bujumbura",
#                               "Incohérence sur le nombre d'amis proches visités au moins 1 fois par mois",
#                               "Nombre d'enfant déclaré différent du Nombre d'enfants renseigné sur la grille")
# err.names <- c("errName","errName",...)
# names(inconsistencies.elig) <- err.names[c()]
# names(inconsistencies.handicap) <- err.names[c()]
# err.msg<- list(errName="",
#                errName2="",...)
# Desription et solution à apporter pour une erreur : indicatif pour le suoerviseur
# ainsi que l'agent
# err.desc <- list(errName="",
#                  errName2="",...)

# Variables des triplettes issues de la base household.membres
# Triplette : CASEID	COMMUNE	ZONE	COLLINE	ZD	STRUCTURE	NUM_MENAGE	NUMERO_INDIVIDU(q111)
# .. NOM(q110) SEXE(q112)	AGE(q116)	TELEPHONE(q130)	
# .. DIFF_VISUEL(q120b) APPAREIL_AUDIO (q121a) DIFF_AUDITIVE(q121b)
# .. DIFF_MARCHER(q122a) DIFF_MEMOIRE(q123a) DIFF_A_PRENDRE_SOIN_DE_SOI(q124a)
# .. DIFF_COMMUNIQUER(q125a) LANGAGE_SIGNE(q126) DIFF_APPRENTISSAGE(q127a)
# .. DIFF_ANALYSER(q128a) AUTRE_DIFFICULTE(q129) ELIGIBLE
# A ajouter depuis household.habitat
# .. NOM_CM	TELEPHONE_CM PHONES_HOUSEHOLD COMMENTAIRE
# A creer pendant la production de la base de triplette
# .. CATEGORIE ID_PH STATUT(1:6) STATUT_LAB(valeur de triplette.statut)
# .. TODAY(date du jour de tirage)
triplette.varlist <- c("hhmid","commune","zone","colline","zd","structure","num_menage",
                       "q111","q110","q112","q113","q115b","q116","q130","q120b","q121a",
                       "q121b","q122a","q123a","q124a","q125a","q126","q127a","q128a",
                       "AUTRE_DIFFICULTE","eligible","nom_cm", "phone_cm", "phones_household",
                       "commentaire","hhid","avenue","numAgent","nomAgent")
triplette.varname <- c("numero_individu","nom","sexe","lien_parente","annne_naissance","age",
                       "telephone","diff_visuel","appareil_audio","diff_auditive",
                       "diff_marcher","diff_memoire","diff_a_prendre_soin_de_soi",
                       "diff_communication","langage_signe","diff_apprentissage",
                       "diff_analyser")
names(triplette.varname) <- triplette.varlist[8:24]
# triplette.varlist.PT <- triplette.varlist[c(1:14,26:33)]

infos.varlist <- c("hhmid","codeAgent","nomAgent","codeSujet","commune","zone",
                   "colline","zd","structure","nummenage","numindividu",
                   "is.ph","is.pt","is.standBy","is.ineligible","is.notDefined",
                   "is.unaivailable","is.matched","is.interviewed","interview.wasRefused",
                   "survey.date","survey.duration","data.isCompleted",
                   "interview.isFinalized","is.inconsistency","has.warning")

## Nombre d'incoherence corriges par le superviseur
handicap.incoherenceBySupervisor <- max(length(which(as.numeric(eligibilite$elig_commune)==4)),
  length(which(as.numeric(handicap$h_commune)==4)))

# Nettoyage de la base eligibilite des données identifiées par le superviseur comme
# incoherents
## sauvegarde au préalable des identifiants à supprimer
## idlist <- (eligibilite %>% filter(as.numeric(elig_commune)==4))$elig_hhmid
# eligibilite <- eligibilite %>% 
#   filter(as.numeric(elig_commune)!=4 & as.numeric(statut_denombrement) %in% c(1:3))

# ## pour tous les identifiants de idlist on va recherche des données qui aurait pu
# ## leur être associé : uniquement ceux trouvés dans handicap qui ne figure pas dans
# ## eligibilite
# # idlist <- str_sub(idlist,2,16)
# 
# Nettoyage de la base handicap des données identifiées par le superviseur comme
# incoherents
# handicap %>% filter(as.numeric(h_commune)==4) %>% select(h_commune,h_hhmid)
# handicap <- handicap %>% filter(as.numeric(h_commune)!=4)
# handicap <- handicap %>% filter(h_hhmid %in% eligibilite$elig_hhmid)

# Correction d'un éventuel oublie de l'identifiant caseid01
# eligibilite %>% filter(!is.na(caseid02) & str_trim(caseid02)!="") %>% 
#   select(elig_hhmid,caseid01,caseid02)

eligibilite$caseid01 <- eligibilite$elig_hhmid
# eligibilite.ph$caseid01 <- eligibilite.ph$elig_hhmid
# eligibilite.pt$caseid01 <- eligibilite.pt$elig_hhmid


#----------------------------------------------------------------------------------------------->
# Fonctions utilitaires
#----------------------------------------------------------------------------------------------->

# Identifie pour toutes les triplettes celles qui ne respectent pas les critéres
# de cohérence. 
# Prévoir 3 liste : un vecteur représentant la liste des erreurs à notifier à l'agent
# une liste représentant le message d'erreur indexé par le nom de l'erreur
# une liste indiquant la description de l'erreur et la solution a y apporter..
# La fonction check.Triplette sera appelé avant la fonction clean.Triplette
# elle produirat une base checkerTriplette où toutes ses verifications seront
# sauvegarder.. à chaque appel de la fonction check.Triplette : la base
# checkerTriplette sera totalement initalisé.
# la fonction clean.Triplette débutera par corriger les incohérences dans la
# base dbTriplette passé sur le contenu de la base checkerTriplette si elle existe
# les erreurs qui seront corrigé seront affecté à FALSE ou 0..
# la base trashTriplette sera produite par la fonction clean.triplette est contiendra
# les éléments que nous ne devons plus utilisé : elle sera mis à jour avec les identifiants
# des PH et des PTs supprimés : possiblement les Ph ayant migré en PT pourront être 
# réutilisé plus tard si on venait en manquer des PT...
# Au niveau du tirage des PH et des PT on supprimera l'appel à la fonction cleaning.Triplette
# cette fonction sera utilié directement dans setTriplette après l'appel de
# à la fonction check.Triplette (première fonction appelée).
# Dans la fonction select.PH on supprimera des potentiels candidats :
# tous les individus figurant dans la liste dbTriplette ainsi que ceux figurant dans la 
# base trashTriplette, ceux-ci vot également pour select.PT
# Avant d'appeler la fonction select.PT on ajoutera à la liste des PH tous les
# PH n'ayant pas de PT disponible sur la fiche des triplettes associé [codeSujet:statut]
# Exemple : [B0001:apparié] ou [B0001:en cours] 
# si codeSujet est vide : statut doit être également vide...
# Ainsi la fonction check.Triplette n'est appelé qu'une fois dans set.Triplette
# comme les fonctions cleaning.triplette, select.ph et select.pt.
# L'appel check.Triplette réinitialise la base checkerTriplette qui sera mis à jour
# au fur et à mesure des appels des 3 autres fonctions dans set.Triplette. Cette base
# contiendra les données sur les identifiants à la fois disponible dans eligibilite,
# handicap et dbTriplette
# L'appel à cleaning.Triplette sauvegardera d'abord les bases dbTriplette et trashTriplette
# par la suite : sans les effacer totalement les mettra à jour en déplaçant éventuellement
# des données de dbTriplette à trashTriplette tout en mettant à jour la base checkerTriplette
# les données qui seront mis à trashTriplette est disponible
check.Triplette <- function(){
  
  cat("[>] check.Triplette :: started...\n")
  
  result <- list(); idlist <- NULL
  # if(exists("dbTriplette") && !is.null(dbTriplette))
  #   idlist <- str_sort(unique(c(idlist,dbTriplette$hhmid)),numeric = TRUE)

  if(exists("eligibilite") && !is.null(eligibilite)){
    tmp <- (eligibilite %>% filter(as.numeric(elig_commune)!=4 &
                                     as.numeric(statut_denombrement) %in% c(1:3) &
                                     as.numeric(statut_handicap) %in% c(1:2)))$elig_hhmid
    idlist <- str_sort(unique(c(idlist,tmp)),numeric = TRUE)
    rm(tmp)
  }
  
  # if(exists("trashTriplette") &&  !is.null(trashTriplette))
  #   idlist <- setdiff(idlist,trashTriplette$hhmid)
  
  if(exists("checkerTriplette") && !is.null(checkerTriplette)){
    # # On retire de la liste des identifiants à vérifier ceux que nous estimons
    # # avoir déjà vérifié on les affectera manuellement # pour certains
    # id.checked <- names(checkerTriplette[unlist(lapply(checkerTriplette,
    #                                                    function(elt){elt$is.checked | !elt$is.inconsistency}))])
    
    # On va plutot retirer uniquement ceux qui sont considéré déjà comme valide
    id.checked <- names(checkerTriplette[unlist(lapply(checkerTriplette,
                                                       function(elt){!elt$is.inconsistency}))])
    idlist <- setdiff(idlist,id.checked)
  }
  
  if(is.null(idlist)){
    cat("[>] check.Triplette :: Pas de données disponibles pour effectuer la vérification\n")
  }else{
    # Liste des identifiants disponibles tant dans le module eligibilite que dans le
    # la base Triplette maintenu par le système.
    result <- lapply(idlist,function(id){
      result$hhmid<-id;
      # Ajout des infos de la base éligibilité
      infos <- eligibilite %>% filter(elig_hhmid %in% id)
      result$codeAgent <- infos$elig_agent
      result$nomAgent <- str_trim(infos$agentHandicap)
      result$codeSujet <- ifelse(is.na(infos$id01),"",sprintf("B%04d",infos$id01))  #str_trim(infos$idlab01)
      result$commune <- infos$elig_commune
      result$zone <- infos$elig_zone
      result$colline <- infos$elig_colline
      result$zd <- infos$elig_zd
      result$structure <- infos$elig_structure
      result$nummenage <- infos$elig_nummenage
      result$numindividu <- infos$elig_numindividu
      result$nb.handicapmineur <- infos$nb_handicapmineur
      result$nb.handicapmajeur <- infos$nb_handicapmajeur
      result$survey.date <- infos$elig_date
      result$survey.duration <- infos$elig_surveyduration
      result$caseid02 <- str_trim(infos$caseid02)
      result$codeSujet02 <- ifelse(is.na(infos$id01),"",sprintf("B%04d",infos$id02)) #str_trim(infos$idlab02)
      result$sex <- NA
      result$elig.birthyear <- infos$h001
      result$elig.age <- infos$elig_age
      
      # Pour module handicap
      result$indiv.birthyear <- NA
      result$indiv.age <- NA
      
      result$commentaire <- str_trim(infos$elig_commentaire)
      # Ajout des infos de la base handicap si existe
      infos <- handicap %>% filter(h_hhmid %in% id)
      if(dim(infos)[1]>0){
        result$survey.duration <- result$survey.duration + infos$h_surveyduration
        result$commentaire <- paste(result$commentaire,
                                    str_trim(infos$h_commentaire),
                                    sep=". ",collapse=". ")
        result$indiv.age <- infos$hage
        result$indiv.birthyear <- infos$h100
        result$sex <- infos$h101
      }
      
      invisible(result)
    })
    names(result) <- idlist
    
    # changeCategorie : TRUE(1); FALSE(0) 
    # Indique si une personne a changé de categorie (TRUE) ou non (FALSE) 
    # Base eligible (categorie!=statut_handicap)
    idlist <- (eligibilite %>% 
      filter(!is.na(categorie) & !is.na(statut_handicap) &
               as.numeric(categorie)!=as.numeric(statut_handicap)) %>%
      select(elig_hhmid,categorie,statut_handicap))$elig_hhmid
    # elt <- result[[sample(idlist,1)]]
    result <- lapply(result, function(elt,idSelected=idlist){ 
      elt$changeCategorie <- elt$hhmid %in% idSelected
      elt$changeCategorie.msg <- ""
      elt$changeCategorie.desc <- ""
      
      if(elt$changeCategorie){
        infos <- eligibilite %>% filter(elig_hhmid==elt$hhmid)
        elt$changeCategorie.msg <- paste(sprintf("'%s' au screning devenu '%s' au handicap",
                                                 infos$categorie,infos$statut_handicap),
                                         collapse = ",[Doublon] ")
        elt$changeCategorie.desc <- "Le statut de l'individu a changé après l'administration du module d'éligibilité"
      }
      invisible(elt)
    })
    #rm(elt)
    # verification du resultat ci-dessus
    # tmp <- names(result[unlist(lapply(result,function(elt) elt$changeCategorie))])
    # which(str_sort(idlist,numeric = TRUE) != str_sort(tmp,numeric = TRUE))
    
    # migrateToPT : TRUE (1); FALSE(0) 
    # Indique si une personne a changé(TRUE) ou non (FALSE)  de categorie : 
    # .. passant de PH à PT (faux-positif)
    idlist <- (eligibilite %>% 
                 filter(as.numeric(categorie)== 1 & as.numeric(statut_handicap)==2) %>%
                 select(elig_hhmid,categorie,statut_handicap))$elig_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){ 
      elt$migrateToPT <- elt$hhmid %in% idSelected
      elt$migrateToPT.msg <- ""
      elt$migrateToPT.desc <- ""
      if(elt$migrateToPT){
        infos <- eligibilite %>% filter(elig_hhmid==elt$hhmid)
        elt$migrateToPT.msg <- paste(sprintf("'%s' au screning devenu '%s' au handicap [faux-positif]",
                                             infos$categorie,infos$statut_handicap),
                                     collapse = ",[Doublon] ")
        elt$migrateToPT.desc <- "Le statut de l'individu a changé après l'administration du module d'éligibilité"
      }
      invisible(elt)
    })
    # verification du resultat ci-dessus
    # tmp <- names(result[unlist(lapply(result,function(elt) elt$migrateTo.PT))])
    # which(str_sort(idlist,numeric = TRUE) != str_sort(tmp,numeric = TRUE))
    rm(idlist)
    
    # migrateTo.PH : TRUE (1); FALSE(0) 
    # Indique si une personne a changé (TRUE) ou non (FALSE) de categorie : 
    # .. passant de PT à PH (vrai-négatif)
    idlist <- (eligibilite %>% 
                 filter(as.numeric(categorie)==2 & as.numeric(statut_handicap)==1) %>%
                 select(elig_hhmid,categorie,statut_handicap))$elig_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){ 
      elt$migrateToPH <- elt$hhmid %in% idSelected;
      elt$migrateToPH.msg <- ""
      elt$migrateToPH.desc <- ""
      if(elt$migrateToPH){
        infos <- eligibilite %>% filter(elig_hhmid==elt$hhmid)
        elt$migrateToPH.msg <- paste(sprintf("'%s' au screning devenu '%s' au handicap [vrai-négatif]",
                                             infos$categorie,infos$statut_handicap),
                                     collapse = ",[Doublon] ")
        elt$migrateToPH.desc <- "Le statut de l'individu a changé après l'administration du module d'éligibilité"
      }
      invisible(elt)
    })
    # verification du resultat ci-dessus
    # tmp <- names(result[unlist(lapply(result,function(elt) elt$migrateTo.PH))])
    # which(str_sort(idlist,numeric = TRUE) != str_sort(tmp,numeric = TRUE))
    rm(idlist)
    
    # isPh : TRUE (1); FALSE(0) 
    # Indique si l'individu est un PH à la fin de l'administration du questionnaire
    # éligibilité
    idlist <- (eligibilite %>% 
                 filter(as.numeric(statut_handicap)==1) %>%
                 select(elig_hhmid,statut_handicap))$elig_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){ 
      elt$is.ph <- elt$hhmid %in% idSelected
      elt$is.ph.msg <- "" 
      elt$is.ph.desc <- ""
      if(elt$is.ph){
        elt$is.ph.msg <- "Personne handicapée"
        elt$is.ph.desc <- "Statut établie à la fin du module éligibilité"
      }
      invisible(elt)
    })
    # Test des resultats de la fonction ci-dessus
    # idSelected <- sample(1:length(idlist),1)
    # result[[idlist[idSelected]]]$is.ph==TRUE # doit être TRUE car on l'a pris dans la liste des PH
    # rm(idSelected)
    rm(idlist)
    
    # is.pt : TRUE (1); FALSE(0) 
    # Indique si l'individu est un PT à la fin de l'administration du questionnaire
    # éligibilité
    idlist <- (eligibilite %>% 
                 filter(as.numeric(statut_handicap)==2) %>%
                 select(elig_hhmid,statut_handicap))$elig_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){ 
      elt$is.pt <- elt$hhmid %in% idSelected
      elt$is.pt.msg <- ""
      elt$is.pt.desc <- ""
      if(elt$is.pt){
        elt$is.pt.msg <- "Personne sans handicap"
        elt$is.pt.desc <- "Statut établie à la fin du module éligibilité"
      }
      invisible(elt)
    })
    # Test des resultats de la fonction ci-dessus
    # idSelected <- sample(1:length(idlist),1)
    # result[[idlist[idSelected]]]$is.pt==TRUE # doit être TRUE car on l'a pris dans la liste des PH
    # rm(idSelected)
    rm(idlist)
    
    # is.standBy : TRUE (1); FALSE(0) 
    # Indique si le statut de l'individu n'a pas été établi à la fin de l'administration du 
    # questionnaire éligibilité
    # L'agent avec son superviseur doivent compléter le questionnaire en indiquant
    # le statut correct : PH, PT ou inéligible
    idlist <- (eligibilite %>% 
                 filter(as.numeric(statut_handicap)==3) %>%
                 select(elig_hhmid,statut_handicap))$elig_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){ 
      elt$is.standBy <- elt$hhmid %in% idSelected
      elt$is.standBy.msg <- ""
      elt$is.standBy.desc <- ""
      if(elt$is.standBy){
        elt$is.standBy.msg <- "Statut en attente de définition"
        elt$is.standBy.desc <- "Le statut de l'individu n'a pas été établi à la fin de l'administration du questionnaire éligibilité. L'agent sur les instruction de son superviseur doit compléter le questionnaire en indiquant le statut correct : PH, PT ou Inéligible"
      }
      invisible(elt)
    })
    # Test des resultats de la fonction ci-dessus
    # idSelected <- sample(1:length(idlist),1)
    # result[[idlist[idSelected]]]$is.standBy==TRUE # doit être TRUE car on l'a pris dans la liste des PH
    # rm(idSelected)
    rm(idlist)
    
    # is.ineligible : TRUE (1); FALSE(0) 
    # Indique si l'individu est un inéligible à la fin de l'administration du questionnaire
    # éligibilité
    idlist <- (eligibilite %>% 
                 filter(as.numeric(statut_handicap)==4) %>%
                 select(elig_hhmid,statut_handicap))$elig_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){ 
      elt$is.ineligible <- elt$hhmid %in% idSelected
      elt$is.ineligible.msg <- ""
      elt$is.ineligible.desc <- ""
      if(elt$is.ineligible){
        elt$is.ineligible.msg <- "Personne inéligible à l'étude"
        elt$is.ineligible.desc <- "Statut établie à la fin du module éligibilité"
      }
      invisible(elt)
    })
    # Test des resultats de la fonction ci-dessus
    # idSelected <- sample(1:length(idlist),1)
    # result[[idlist[idSelected]]]$is.ineligible==TRUE # doit être TRUE car on l'a pris dans la liste des PH
    # rm(idSelected)
    rm(idlist)
    
    # is.notDefined : TRUE (1); FALSE(0) 
    # Indique si le questionnaire éligibilité est partiellement remplie
    # L'agent doit compléter ou finaliser la saisie du questionnaire
    idlist <- (eligibilite %>% 
                 filter(is.na(statut_handicap)) %>%
                 select(elig_hhmid,statut_handicap))$elig_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){ 
      elt$is.notDefined <- elt$hhmid %in% idSelected
      elt$is.notDefined.msg <- ""
      elt$is.notDefined.desc <- ""
      if(elt$is.notDefined){
        elt$is.notDefined.msg <- "Statut handicap non défini"
        elt$is.notDefined.desc <- "Le questionnaire éligibilité est partiellement remplie. L'agent doit compléter ou finaliser la saisie du questionnaire"
      }
      invisible(elt)
    })
    # Test des resultats de la fonction ci-dessus
    # idSelected <- sample(1:length(idlist),1)
    # result[[idlist[idSelected]]]$is.notDefined==TRUE # doit être TRUE car on l'a pris dans la liste des identifiants idlist
    # rm(idSelected)
    rm(idlist)
    
    # is.interviewed : TRUE (1); FALSE(0) 
    # Indique si une personne a été enquêtée
    # Questionnaire a ne pas prendre en compte.
    idlist <- (eligibilite %>% 
                 filter(as.numeric(statut_denombrement) %in% c(1,2)) %>%
                 select(elig_hhmid,statut_denombrement))$elig_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){ 
      elt$is.interviewed <- elt$hhmid %in% idSelected
      elt$is.interviewed.msg <- ""
      elt$is.interviewed.desc <- ""
      if(elt$is.interviewed){
        elt$is.interviewed.msg <- "Personne enquêtée."
        elt$is.interviewed.desc <- "Information fournie par l'application CSPro"
      }
      invisible(elt)
    })
    # Test des resultats de la fonction ci-dessus
    # idSelected <- sample(1:length(idlist),1)
    # result[[idlist[idSelected]]]$is.interviewed==TRUE # doit être TRUE car on l'a pris dans la liste des identifiants idlist
    # rm(idSelected)
    rm(idlist)
    
    # is.matched : TRUE (1); FALSE(0) 
    # Indique si une personne a été appariée à une autre
    # Questionnaire a ne pas prendre en compte.
    idlist <- (eligibilite %>% 
                 # filter(as.numeric(statut_denombrement)==2) %>%
                 filter(!is.na(caseid02) & str_trim(caseid02)!="" & 
                          str_trim(caseid01)!= str_trim(caseid02)) %>%
                 select(elig_hhmid,statut_denombrement))$elig_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){ 
      elt$is.matched <- elt$hhmid %in% idSelected
      elt$is.matched.msg <- ""
      elt$is.matched.desc <- ""
      if(elt$is.matched){
        idlab <- ifelse(elt$is.ph,"PH",ifelse(elt$is.pt,"PT","ID"))
        elt$is.matched.msg <- paste(sprintf("%s (%s-%s) appariée à %s (ID-%s)",
                                            elt$codeSujet,idlab,elt$hhmid,
                                            elt$codeSujet02,elt$caseid02),
                                    collapse=", ")
        elt$is.matched.desc <- "Information fournie par l'application CSPro"
      }
      invisible(elt)
    })
    # Test des resultats de la fonction ci-dessus
    # idSelected <- sample(1:length(idlist),1)
    # result[[idlist[idSelected]]]$is.matched==TRUE # doit être TRUE car on l'a pris dans la liste des identifiants idlist
    # rm(idSelected)
    rm(idlist)
    
    # is.unaivailable : TRUE (1); FALSE(0) 
    # Indique si un PH ou un PT a été déclarée indisponible pour l'administration d'un questionnaire
    # Questionnaire a ne pas prendre en compte.
    idlist <- (eligibilite %>% 
                 filter(as.numeric(statut_denombrement)==5) %>%
                 select(elig_hhmid,statut_denombrement))$elig_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){ 
      elt$is.unaivailable <- elt$hhmid %in% idSelected
      elt$is.unaivailable.msg <- ""
      elt$is.unaivailable.desc <- ""
      if(elt$is.unaivailable){
        elt$is.unaivailable.msg <- "Impossibilité à entrer en contact avec l'individu"
        elt$is.unaivailable.desc <- "Personne ayant été déclarée indisponible pour l'administration des questionnaires du volet handicap"
      }
      invisible(elt)
    })
    # Test des resultats de la fonction ci-dessus
    # idSelected <- sample(1:length(idlist),1)
    # result[[idlist[idSelected]]]$is.unaivailable==TRUE # doit être TRUE car on l'a pris dans la liste des identifiants idlist
    # rm(idSelected)
    rm(idlist)
    
    # interview.inProgress : TRUE (1); FALSE(0) 
    # Indique si un questionnaire est encore en cours d'administration 
    # Questionnaire a ne pas prendre en compte.
    idlist <- (eligibilite %>% 
                 filter(as.numeric(statut_denombrement)==3) %>%
                 select(elig_hhmid,statut_denombrement))$elig_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){ 
      elt$interview.inProgress <- elt$hhmid %in% idSelected
      elt$interview.inProgress.msg <-""
      elt$interview.inProgress.desc <- ""
      if(elt$interview.inProgress){
        elt$interview.inProgress.msg <- "Questionnaire en cours d'administration"
        elt$interview.inProgress.desc <- "Information fournie de l'application CSPro"
      }
      invisible(elt)
    })
    # Test des resultats de la fonction ci-dessus
    # idSelected <- sample(1:length(idlist),1)
    # result[[idlist[idSelected]]]$interview.inProgress==TRUE # doit être TRUE car on l'a pris dans la liste des identifiants idlist
    # rm(idSelected)
    rm(idlist)
    
    # interview.isFinalized : TRUE (1); FALSE(0) 
    # Indique si l'administration d'un questionnaire est finie : l'individu est soit enquêté, soit apparié
    # Questionnaire finalisé.
    idlist <- (handicap %>% 
                 filter(as.numeric(h_isclosed)==1) %>%
                 select(h_hhmid,h_isclosed))$elig_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){ 
      elt$interview.isFinalized <- elt$hhmid %in% idSelected
      elt$interview.isFinalized.msg <- ""
      elt$interview.isFinalized.desc <- ""
      if(elt$interview.isFinalized){
        elt$interview.isFinalized.msg <- "Questionnaire clôturé par l'agent"
        elt$interview.isFinalized.desc <- "Information fournie depuis l'application CSPro"
      }
      invisible(elt)
    })
    # Test des resultats de la fonction ci-dessus
    # idSelected <- sample(1:length(idlist),1)
    # result[[idlist[idSelected]]]$interview.isFinalized==TRUE # doit être TRUE car on l'a pris dans la liste des identifiants idlist
    # rm(idSelected)
    rm(idlist)
    
    # interview.wasRefused : TRUE (1); FALSE(0) 
    # Indique qu'une personne a refusé de répondre a refusé de participer à l'étude
    # Questionnaire a ne pas prendre en compte.
    idlist <- (eligibilite %>% 
                 filter(as.numeric(statut_denombrement)==4) %>%
                 select(elig_hhmid,statut_denombrement))$elig_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){ 
      elt$interview.wasRefused <- elt$hhmid %in% idSelected
      elt$interview.wasRefused.msg <- ""
      elt$interview.wasRefused.desc <- ""
      if(elt$interview.wasRefused){
        elt$interview.wasRefused.msg <- "Refus de participation à l'étude"
        elt$interview.wasRefused.desc <- "Information fournie par l'application CSPro"
      }
      invisible(elt)
    })
    # Test des resultats de la fonction ci-dessus
    # idSelected <- sample(1:length(idlist),1)
    # result[[idlist[idSelected]]]$interview.wasRefused==TRUE # doit être TRUE car on l'a pris dans la liste des identifiants idlist
    # rm(idSelected)
    rm(idlist)
    
    # form.notAdministered  : TRUE (1); FALSE(0) 
    # Indique si le questionnaire a déjà été administré
    result <- lapply(result, function(elt){ 
      elt$form.notAdministered <- length(elt$codeAgent)==0
      elt$form.notAdministered.msg <- "" 
      elt$form.notAdministered.desc <- ""
      if(elt$form.notAdministered){
        elt$form.notAdministered.msg <- "La personne n'a pas encore été enquêtée"
        elt$form.notAdministered.desc <- "Superviseur pensait à distribuer la triplette. Si fait, Agent n'oubliait pas d'administrer le questionnaire"
      }
      invisible(elt)
    })
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$form.notAdministered))])
    # idlist <- names(result[unlist(lapply(result,function(elt) form.notAdministered & elt$is.interviewed))])
    # result[[idlist[sample(1:length(idlist),1)]]][c("codeAgent","form.notAdministered","is.interviewed")]
    # rm(idlist)
    
    # eligibility.isPartial : données eligibilite partiellement remplies
    # si la première variable obligatoire et/ou la dernière du questionnaire eligibilite
    # ne sont pas définie
    # eligibilite.isPartial = is.na(categorie) | is.na(h001) | is.na(h016) | is.na(elig_confirmation)
    dbtmp <- eligibilite %>%
      mutate(is.checked= is.na(categorie) | is.na(h001) | is.na(h016) | is.na(elig_confirmation)) %>%
      select(elig_hhmid,is.checked)
    result <- lapply(result,function(elt,db=dbtmp){
      infos <- db %>% filter(elig_hhmid %in% elt$hhmid)
      elt$eligibility.isPartial <- !elt$form.notAdministered & 
        (elt$is.notDefined | (dim(infos)[1]>0 && infos$is.checked==TRUE))
      elt$eligibility.isPartial.msg <- ""
      elt$eligibility.isPartial.desc <- ""
      if(elt$eligibility.isPartial){
        elt$eligibility.isPartial.msg <- "Module éligibilité incomplet"
        elt$eligibility.isPartial.desc <- "Données partiellement remplies. L'agent doit compléter ou finaliser le module éligibilité"
      }
      invisible(elt)
    })
    # Verification des résultats ci-dessus obtenu
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$eligibility.isPartial))])
    # # idlist <- (eligibilite %>% filter(is.na(elig_confirmation)))$elig_hhmid
    # result[idlist[sample(1:length(idlist),1)]]
    # rm(idlist)
    rm(dbtmp)
    
    # eligibility.isCompleted : données eligibilite totalement saisies
    # Indique si le module éligibilité peut être estimé comme totalement administré
    # eligibilite.isCompleted = !is.na(categorie) & !is.na(h001) & !is.na(h016) & !is.na(elig_confirmation)
    dbtmp <- eligibilite %>%
      mutate(is.checked= !is.na(categorie) & !is.na(h001) & !is.na(h016) & !is.na(elig_confirmation)) %>%
      select(elig_hhmid,is.checked)
    result <- lapply(result,function(elt,db=dbtmp){
      infos <- db %>% filter(elig_hhmid %in% elt$hhmid)
      elt$eligibility.isCompleted <- !elt$form.notAdministered & 
        (elt$is.ph | elt$is.pt | elt$is.ineligible) & 
        (dim(infos)[1]>0 && infos$is.checked==TRUE)
      elt$eligibility.isCompleted.msg <- ""
      elt$eligibility.isCompleted.desc <- ""
      if(elt$eligibility.isCompleted){
        elt$eligibility.isCompleted.msg <- "Module éligibilité administré"
        elt$eligibility.isCompleted.desc <- "Les données éligibilité semblent avoir été totalement saisies."
      }
      invisible(elt)
    })
    # Verification des résultats ci-dessus obtenu
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$eligibility.isCompleted))])
    # idlist.2 <- eligibilite %>% filter(elig_hhmid %in% setdiff(dbtmp$elig_hhmid[dbtmp$is.checked],tmp))
    # result[idlist[sample(1:length(idlist),1)]]
    # View(idlist.2)
    # rm(idlist,idlist.2)
    rm(dbtmp)
    
    # handicap.isPartial : données handicap partiellement remplies
    # si la première variable obligatoire e/ou la dernière du questionnaire handicap
    # ne sont pas définie et si (is.ph | is.pt)
    # handicap.isPartial = is.na(h100) | is.na(h101) | is.na(h_ischecked) | is.na(h_confirmation)
    dbtmp <- handicap %>%
      mutate(is.checked= is.na(h100) | is.na(h101) | is.na(h_ischecked) | is.na(h_confirmation)) %>%
      select(h_hhmid,is.checked)
    result <- lapply(result,function(elt,db=dbtmp){
      infos <- db %>% filter(h_hhmid %in% elt$hhmid)
      elt$handicap.isPartial <- !elt$form.notAdministered & 
        (dim(infos)[1]>0 && infos$is.checked==TRUE)
      elt$handicap.isPartial.msg <- ""
      elt$handicap.isPartial.desc <- ""
      if(elt$handicap.isPartial){
        elt$handicap.isPartial.msg <- "Module handicap incomplet"
        elt$handicap.isPartial.desc <- "Données partiellement remplies. L'agent doit compléter ou finaliser le module handicap"
      }
      invisible(elt)
    })
    # Verification des résultats ci-dessus obtenu
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$handicap.isPartial))])
    # # idlist <- (handicap %>% filter(is.na(h_confirmation)))$h_hhmid
    # result[idlist[sample(1:length(idlist),1)]]
    # rm(idlist)
    # 
    rm(dbtmp)
    
    # handicap.isCompleted : données handicap totalement saisies
    # Indique si le module handicap peut être estimé comme totalement administré
    # handicap.isCompleted = !is.na(h100) & !is.na(h101) & !is.na(h_ischecked) & !is.na(h_confirmation)
    dbtmp <- handicap %>%
      mutate(is.checked= !is.na(h100) & !is.na(h101) & !is.na(h_ischecked) & !is.na(h_confirmation)) %>%
      select(h_hhmid,is.checked)
    result <- lapply(result,function(elt,db=dbtmp){
      infos <- db %>% filter(h_hhmid %in% elt$hhmid)
      elt$handicap.isCompleted <- !elt$form.notAdministered & 
        (dim(infos)[1]>0 && infos$is.checked==TRUE)
      elt$handicap.isCompleted.msg <- ""
      elt$handicap.isCompleted.desc <- ""
      if(elt$handicap.isCompleted){
        elt$handicap.isCompleted.msg <- "Module handicap administré"
        elt$handicap.isCompleted.desc <- "Les données handicap semblent avoir été totalement saisies."
      }
      invisible(elt)
    })
    # Verification des résultats ci-dessus obtenu
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$handicap.isCompleted))])
    # idlist.2 <- handicap %>% filter(h_hhmid %in% setdiff(dbtmp$h_hhmid[dbtmp$is.checked],tmp))
    # result[idlist[sample(1:length(idlist),1)]]
    # View(idlist.2)
    # rm(idlist,idlist.2)
    rm(dbtmp)
    
    #data.isPartial : si eligibility.isPartial | handicap.isPartial...
    # Indique si l'un des module a été partiellement complété
    result <- lapply(result,function(elt){
      elt$data.isPartial <- elt$eligibility.isPartial | elt$handicap.isPartial
      elt$data.isPartial.msg <- ""
      elt$data.isPartial.desc <- ""
      if(elt$data.isPartial){
        elt$data.isPartial.msg <- "Données partiellement remplies"
        elt$data.isPartial.desc <- "L'agent doit les compléter ou les finaliser"
      }
      invisible(elt)
    })
    # Verification des résultats ci-dessus obtenu
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$data.isPartial))])
    # result[[idlist[sample(1:length(idlist),1)]]]
    # rm(idlist)
    
    #data.isCompleted : si eligibility.isCompleted & handicap.isCompleted...
    # Indique que tous les deux modules ont été totalement administrés
    result <- lapply(result,function(elt){
      elt$data.isCompleted <- elt$eligibility.isCompleted & elt$handicap.isCompleted
      elt$data.isCompleted.msg <- ""
      elt$data.isCompleted.desc <- ""
      if(elt$data.isCompleted){
        elt$data.isCompleted.msg <- "Modules éligibilité et handicap administrés"
        elt$data.isCompleted.desc <- "Les données éligibilité et handicap semblent avoir été totalement saisies."
      }
      invisible(elt)
    })
    # Verification des résultats ci-dessus obtenu
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$data.isCompleted))])
    # idlist <- names(result[unlist(lapply(result,function(elt){ 
    #   elt$data.isCompleted & elt$interview.isFinalized
    # }))])
    # elt <- result[[idlist[sample(1:length(idlist),1)]]]
    # rm(idlist,elt)
    
    #filledDisability.partialEligibility
    # (handicap.isPartial | handicap.isCompleted) & (eligibility.isPartial)
    # [Erreur de programmation de l'application]
    # Verifie que les identifiants dans le module handicap ont bien tous un statut handicap
    # definie : dans le cas contraire : nécessité pour l'agent de compléter le module
    # éligibilité et le cas échéant de finaliser le module handicap...
    result <- lapply(result,function(elt){
      elt$filledDisability.partialEligibility <- (elt$handicap.isPartial | elt$handicap.isCompleted) & 
        (elt$eligibility.isPartial)
      elt$filledDisability.partialEligibility.msg <- ""
      elt$filledDisability.partialEligibility.desc <- ""
      if(elt$filledDisability.partialEligibility){
        elt$filledDisability.partialEligibility.msg <- "Module handicap administré alors que le module éligibilité n'a pas été finalisé"
        elt$filledDisability.partialEligibility.desc <- "L'agent doit compléter les données du module éligibilité depuis le questionnaire papier utilisé pour pallier ce défaut applicatif"
      }
      invisible(elt)
    })
    # Verification des résultats ci-dessus obtenu
    # tmp <- names(result[unlist(lapply(result,function(elt) elt$filledDisability.partialEligibility))])
    # result[[tmp[sample(1:length(tmp),1)]]]
    # rm(tmp)
      
    # eligibility.inconsistencyOnDeclaration
    # Données marquées incohérentes au niveau du module éligibilité
    # Données à ne pas prendre en compte.
    # (as.numeric(eligibilie$elig_commune)==4 ou )
    # La variable "commune" a pour valeur "Incohérences" dans le module éligibilté
    # Incohérence sur déclaration volontaire de l'agent ou du superviseur. Données à ne pas prendre en considération.
    idlist <- (eligibilite %>% filter(as.numeric(elig_commune)==4))$elig_hhmid
    result <- lapply(result,function(elt,idSelected=idlist){
      elt$eligibility.inconsistencyOnDeclaration <- elt$hhmid %in% idSelected
      elt$eligibility.inconsistencyOnDeclaration.msg <- ""
      elt$eligibility.inconsistencyOnDeclaration.desc <- ""
      if(elt$eligibility.inconsistencyOnDeclaration){
        elt$eligibility.inconsistencyOnDeclaration.msg <- "La variable 'commune' a pour valeur 'Incohérences' dans le module éligibilté"
        elt$eligibility.inconsistencyOnDeclaration.desc <- "Incohérence sur déclaration de l'agent ou du superviseur. Données à ne pas prendre en considération."
      }
      invisible(elt)
    })
    # Test du résultat ci-dessus
    # tmp <- names(result[unlist(lapply(result,function(elt) elt$eligibility.inconsistencyOnDeclaration))])
    # which(tmp!=idlist)
    # rm(tmp)
    rm(idlist)
    
    # handicap.inconsistencyOnDeclaration
    # Données marquées incohérentes au niveau du module handicap
    # Données à ne pas prendre en compte.
    # (as.numeric(handicap$h_commune)==4 ou )
    # La variable "commune" a pour valeur "Incohérences" dans le module handicap
    # Incohérence par déclaration volontaire de l'agent ou du superviseur. Données à ne pas prendre en considération.
    idlist <- (handicap %>% filter(as.numeric(h_commune)==4))$h_hhmid
    result <- lapply(result,function(elt,idSelected=idlist){
      elt$handicap.inconsistencyOnDeclaration <- elt$hhmid %in% idSelected
      elt$handicap.inconsistencyOnDeclaration.msg <- ""
      elt$handicap.inconsistencyOnDeclaration.desc <- ""
      if(elt$handicap.inconsistencyOnDeclaration){
        elt$handicap.inconsistencyOnDeclaration.msg <- "La variable 'commune' a pour valeur 'Incohérences' dans le module handicap"
        elt$handicap.inconsistencyOnDeclaration.desc <- "Incohérence sur déclaration de l'agent ou du superviseur. Données à ne pas prendre en considération."
      }
      invisible(elt)
    })
    # Test du résultat ci-dessus
    # tmp <- names(result[unlist(lapply(result,function(elt) elt$handicap.inconsistencyOnDeclaration))])
    # which(tmp!=idlist)
    # rm(tmp)
    rm(idlist)
    
    #inconsistency.onDeclaration
    # Donéées marquées comme incohérentes
    # Ce type de données est considérée comme incohérentes à partir du moment
    # Où l'identifant avec l'incohérence  figure au niveau handicap mais pas au niveau
    # éligibilite
    # handicap.inconsistencyOnDeclaration && !eligibility.inconsistencyOnDeclaration
    # Incohérence indiqué dans le module handicap mais non reporté dans le module éligibilité
    # L'agent doit indiquer au superviseur et au Datamanager l'identifiant a marqué incohérent dans le module éligibilité.
    result <- lapply(result,function(elt){
      elt$inconsistency.onDeclaration <- elt$handicap.inconsistencyOnDeclaration &
        !elt$eligibility.inconsistencyOnDeclaration
      elt$inconsistency.onDeclaration.msg <- ""
      elt$inconsistency.onDeclaration.desc <- ""
      if(elt$inconsistency.onDeclaration){
        elt$inconsistency.onDeclaration.msg <- "Incohérence déclarée dans le module handicap mais non reporté dans le module éligibilité"
        elt$inconsistency.onDeclaration.desc <- "L'agent doit indiquer au superviseur et au Datamanager l'identifiant a marqué incohérent dans le module éligibilité."
      }
      invisible(elt)
    })
    
    # sex.differentValue : TRUE (1); FALSE(0) 
    # Indique si le sexe d'un individu au handicap est différent(TRUE) ou non (FALSE) de celui
    # au screening # ID avec PB=1010751532600102
    dbtmp <- handicap %>% 
      mutate(handicap.sex = as.character(h101)) %>%
      select(h_hhmid,handicap.sex) %>%
      left_join(y=household.membres %>% 
                  mutate(screening.sex=if_else(q112=="Masculin","Homme",
                                               if_else(q112=="Féminin","Femme",NA_character_))) %>%
                  select(hhmid,screening.sex),
                by=c("h_hhmid"="hhmid"))
    result <- lapply(result, function(elt,db=dbtmp){
      infos <- db %>% filter(h_hhmid %in% elt$hhmid)
      elt$sex.differentValue <- !elt$form.notAdministered && !elt$is.unaivailable &&
      elt$eligibility.isCompleted && (elt$is.ph || elt$is.pt) &&
        ((!elt$handicap.isCompleted & !elt$handicap.isPartial) ||
        (is.na(infos$handicap.sex) || is.na(infos$screening.sex) || 
           infos$screening.sex!=infos$handicap.sex))
      elt$sex.differentValue.msg <- ""
      elt$sex.differentValue.desc <- ""
      if(elt$sex.differentValue){
        elt$sex.differentValue.msg <- paste(sprintf("'%s' au screning devenu(e) '%s' au handicap",
                                                    infos$screening.sex,infos$handicap.sex),
                                            collapse = ",[Doublon] ")
        elt$sex.differentValue.desc <- "Incohérence entre les données du screening et du handicap"
      }
      invisible(elt)
    })
    # Verification des resultats ci-dessus
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$sex.differentValue))])
    # varlist <- c("form.notAdministered","is.unaivailable","eligibility.isCompleted","is.ph","is.pt",
    #              "handicap.isCompleted","handicap.isPartial","sex.differentValue",
    #              "sex.differentValue.msg")
    # idSelected <- idlist[sample(1:length(idlist),1)]
    # (result[[idSelected]])[varlist]
    # dbtmp %>% filter(h_hhmid %in% idSelected)
    # rm(idlist,varlist,idSelected)
    rm(dbtmp)
    
    # birthdate.differentValue : TRUE (1); FALSE(0) 
    # Indique si la date de naissance d'un individu au handicap est différent(TRUE) ou non (FALSE) de celui
    # au screening
    dbtmp <- handicap %>% 
      mutate(handicap.birthdate = h100) %>%
      select(h_hhmid,handicap.birthdate) %>%
      left_join(y=household.membres %>% 
                  mutate(screening.birthdate=q115b) %>%
                  select(hhmid,screening.birthdate),
                by=c("h_hhmid"="hhmid"))
    result <- lapply(result, function(elt,db=dbtmp){
      infos <- db %>% filter(h_hhmid %in% elt$hhmid)
      elt$birthdate.differentValue <- elt$eligibility.isCompleted && (elt$is.ph || elt$is.pt) &&
        ((!elt$handicap.isCompleted & !elt$handicap.isPartial) ||
           (is.na(infos$handicap.birthdate) || is.na(infos$screening.birthdate) || 
              infos$screening.birthdate!=infos$handicap.birthdate))
      elt$birthdate.differentValue.msg <- ""
      elt$birthdate.differentValue.desc <- ""
      if(elt$birthdate.differentValue){
        elt$birthdate.differentValue.msg <- paste(sprintf("Année de naissance au screning '%d' devenue '%d' au handicap",
                                                    infos$screening.birthdate,infos$handicap.birthdate),
                                            collapse = ",[Doublon] ")
        elt$birthdate.differentValue.desc <- "Incohérence entre les données du screening et du handicap"
      }
      invisible(elt)
    })
    # Verification des resultats ci-dessus
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$birthdate.differentValue))])
    # varlist <- c("eligibility.isCompleted","is.ph","is.pt","handicap.isCompleted",
    #              "handicap.isPartial","birthdate.differentValue","birthdate.differentValue.msg")
    # idSelected <- idlist[sample(1:length(idlist),1)]
    # (elt<-result[[idSelected]])[varlist]
    # dbtmp %>% filter(h_hhmid %in% idSelected)
    # rm(idlist,varlist,idSelected,elt)
    rm(dbtmp)
    
    # eligibility.dataDuplicated  : TRUE (1); FALSE(0) 
    # Indique si les données ont été saisies plus d'une fois dans le module eligibilité
    # pour un identifiant donnée.
    # Possible problème de synchronisation des données...
    idlist <- unique((eligibilite %>% filter(duplicated(elig_hhmid)))$elig_hhmid)
    result <- lapply(result, function(elt,idSelected=idlist){
      
      elt$eligibility.dataDuplicated <- elt$hhmid %in% idSelected
      elt$eligibility.dataDuplicated.msg <- ""
      elt$eligibility.dataDuplicated.desc <- ""
      
      if(elt$eligibility.dataDuplicated){
        infos <-  eligibilite %>% filter(elig_hhmid %in% elt$hhmid)
        nb.duplicates <- dim(infos)[1]
        elt$eligibility.dataDuplicated.msg <- sprintf("Les informations de l'individu ont été saisies  %02d fois",
                                                      nb.duplicates)
        elt$eligibility.dataDuplicated.desc <- "Données saisies plus d'une fois dans le module éligibilité"
        elt$eligibility.dataDuplicated.varlist <- sapply(c(1:nb.duplicates), function(idx){
          names(infos)[which(infos[1,]!=infos[idx,])]
        })
        elt$eligibility.dataDuplicated.varlist <- sapply(elt$eligibility.dataDuplicated.varlist,
                                                         unlist)
        elt$eligibility.dataDuplicated.varlist <- unique(unlist(elt$eligibility.dataDuplicated.varlist))
        elt$eligibility.dataDuplicated.varlist <- paste(elt$eligibility.dataDuplicated.varlist,
                                                        collapse = ", ")
      }
      invisible(elt)
    })
    #Test de verification du resultat produit ci-dessus
    # elt <- result[[sample(idlist,1)]]
    # infos <-  eligibilite %>% filter(elig_hhmid %in% elt$hhmid)
    # vartmp <- unlist(sapply(sapply(c(1,2,2), function(idx){
    #   tmp <- names(infos)[which(infos[1,]!=infos[idx,])]
    #   if(length(tmp)){
    #     size.sample <- sample(1:length(tmp),1)
    #     cat("idx=",idx," length(tmp) = ", length(tmp)," size.sample=",size.sample,"\n")
    #     tmp[1:size.sample]
    #   }else{tmp}
    # }),unlist));
    # tmp <- names(result[unlist(lapply(result,function(elt) elt$eligibility.dataDuplicated))])
    # which(str_sort(idlist,numeric = T)!=str_sort(tmp,numeric = T))
    # rm(tmp,elt,infos)
    rm(idlist)
    
    #handicap.dataDuplicated  : TRUE (1); FALSE(0) 
    # Indique si un les données ont été saisies plus d'une fois pour un identifiant données
    # Possible problème de synchronisation des données
    idlist <- unique((handicap %>% filter(duplicated(h_hhmid)))$h_hhmid)
    result <- lapply(result, function(elt,idSelected=idlist){
      
      elt$handicap.dataDuplicated <- elt$hhmid %in% idSelected
      elt$handicap.dataDuplicated.msg <- ""
      elt$handicap.dataDuplicated.desc <- ""
      
      if(elt$handicap.dataDuplicated){
        infos <-  handicap %>% filter(h_hhmid %in% elt$hhmid)
        nb.duplicates <- dim(infos)[1]
        elt$handicap.dataDuplicated.msg <- sprintf("Les informations de l'individu ont été saisies  %02d",
                                                      nb.duplicates)
        elt$handicap.dataDuplicated.desc <- "Données saisies plus d'une fois dans le module handicap"
        elt$handicap.dataDuplicated.varlist <- sapply(c(1:nb.duplicates), function(idx){
          names(infos)[which(infos[1,]!=infos[idx,])]
        })
        elt$handicap.dataDuplicated.varlist <- sapply(elt$handicap.dataDuplicated.varlist,
                                                         unlist)
        elt$handicap.dataDuplicated.varlist <- unique(unlist(elt$handicap.dataDuplicated.varlist))
        elt$handicap.dataDuplicated.varlist <- paste(elt$handicap.dataDuplicated.varlist,
                                                        collapse = ", ")
      }
      invisible(elt)
    })
    # Test de verification du resultat produit ci-dessus
    # elt <- result[[sample(idlist,1)]]
    # infos <-  handicap %>% filter(h_hhmid %in% elt$hhmid)
    # tmp <- names(result[unlist(lapply(result,function(elt) elt$eligibility.dataDuplicated))])
    # which(str_sort(idlist,numeric = T)!=str_sort(tmp,numeric = T))
    # rm(elt,infos,tmp)
    rm(idlist)
    
    #data.duplicated  : TRUE (1); FALSE(0) 
    # Indique un les données ont été saisies plus d'une fois pour un identifiant données
    # Possible problème de synchronisation des données
    # data.duplicated = eligibility.dataDuplicated | handicap.dataDuplicated
    result <- lapply(result, function(elt){
      elt$data.duplicated <- elt$eligibility.dataDuplicated | elt$handicap.dataDuplicated
      elt$data.duplicated.msg <- ""
      elt$data.duplicated.desc <- ""
      if(elt$data.duplicated){
        elt$data.duplicated.msg <- " Présence d'un ou de plusieurs doublons dans la base de données"
        elt$data.duplicated.desc<- "Les données de l'individu ont été saisies plus d'une fois"   
      }
      invisible(elt)
    })
    # Test de verification du resultat produit ci-dessus
    # varname <- names(result[unlist(lapply(result,function(elt) elt$data.duplicated))])
    # idSelect <- varname[sample(1:length(varname),1)]
    # idSelect ## Cas de l'identifiant : 2052850101500602
    # result[[idSelect]][c("eligibility.dataDuplicated","handicap.dataDuplicated","data.duplicated")]
    # rm(varname,idSelect)
    
    # unknown.person
    # Indique si un individu a été enquêté alors qu'il n'a pas été tiré
    # Possible confusion de l'agent ou erreur survenue lors de la reprogrammation
    # de l'application
    # unknown.person.msg : PH/PT-xxxx (Bxxxx) non retrouvé dans la base de tirage
    # unknown.person.desc : Possible confusion de l'agent lors de la saisie d'identifiant
    idlist <- NULL
    if(exists("dbTriplette",envir = .GlobalEnv)){
      idlist <- (eligibilite %>% 
                   filter(as.numeric(elig_commune)!=4 & !(elig_hhmid %in% dbTriplette$hhmid))
                 )$elig_hhmid
    }
    result <- lapply(result, function(elt,idSelected=idlist){
      elt$unknown.person <- (elt$hhmid %in% idSelected) & !elt$form.notAdministered
      elt$unknown.person.msg <- ""
      elt$unknown.person.desc <- ""
      if(elt$unknown.person){
        infos <- eligibilite %>% filter(elig_hhmid %in% elt$hhmid)
        label <- ifelse(elt$is.ph,"PH",ifelse(elt$is.pt,"PT","ID"))
        elt$unknown.person.msg <- paste(sprintf("%s-%s (%s) non retrouvée dans la base de tirage",
                                                label,elt$hhmid,infos$idlab01),
                                        collapse = ", ")
        elt$unknown.person.desc<- "Possible confusion de l'identifiant de l'individu lors de la saisie"   
      }
      invisible(elt)
    })
    # Test des résultats ci-dessus
    # # idlist <- names(result[unlist(lapply(result,function(elt) elt$unknown.person))])
    # idSelected <- idlist[sample(1:length(idlist),1)]
    # varlist <- c("unknown.person","unknown.person.msg","unknown.person.desc")
    # idSelected %in% dbTriplette$hhmid
    # result[[idSelected]][varlist]
    # rm(idSelected,varlist)
    rm(idlist)
    
    # pt.localisationInconsistencyWithPh
    # Indique que le PT ne se trouve pas dans la même ZD ou ZD voisine que son PH 
    dbtmp <- eligibilite %>%
      select(elig_hhmid,categorie,elig_zd,idlab01,caseid02,idlab02) %>%
      # mutate(caseid02=str_trim(caseid02)) %>%
      filter(as.numeric(categorie)==2) %>% 
      left_join(y=(eligibilite %>% filter(as.numeric(categorie)==1) %>%
                     mutate(ph_zd=as.character(elig_zd)) %>%
                     select(idPH=elig_hhmid,ph_zd)),
                by=c("caseid02"="idPH")) %>%
      left_join(y=repartition.zd %>% 
                  mutate(zd_lab=as.character(zd.num)) %>%
                  select(zd_lab,zd_limit=zd.limit),
                by=c("ph_zd"="zd_lab")) %>%
      mutate(is.checked=!(elig_zd %in% c(ph_zd,unlist(str_split(zd_limit,";")))))
    result <- lapply(result, function(elt,db=dbtmp){
      infos <- db %>% filter(elig_hhmid %in% elt$hhmid)
      elt$pt.localisationInconsistencyWithPh <- any(infos$is.checked)
      elt$pt.localisationInconsistencyWithPh.msg <- ""
      elt$pt.localisationInconsistencyWithPh.desc <- ""
      if(elt$pt.localisationInconsistencyWithPh){
        # infos <- eligibilite %>% filter(elig_hhmid %in% elt$hhmid)
        elt$pt.localisationInconsistencyWithPh.msg <- paste(sprintf("Le PT-%s (%s) se trouve dans la ZD%s qui n'est ni égale ni voisine à  ZD%s de son PH-%s (%s)",
                                                infos$elig_hhmid,infos$idlab01,infos$elig_zd,
                                                infos$ph_zd,infos$caseid02,infos$idlab02),
                                        collapse = ", ")
        elt$pt.localisationInconsistencyWithPh.desc<- "Possible confusion de l'identifiant de l'individu lors de la saisie"   
      }
      invisible(elt)
    })
    # Testons le résultat ci-dessus
    # Bon pour le moment on ne peut pas tester car on a pas d'incoherence de cette nature
    # sum(dbtmp$is.checked)
    # names(result[unlist(lapply(result,function(elt) elt$pt.localisationInconsistencyWithPh))])
    # idSelected <- (dbtmp %>% filter(is.checked))$elig_hhmid[sample(1:dim(dbtmp)[1],1)]
    # varlist <- c("pt.localisationInconsistencyWithPh","pt.localisationInconsistencyWithPh.msg",
    #              "pt.localisationInconsistencyWithPh.desc")
    # result[[idSelected]][varlist]
    # rm(idSelected,varlist)
    rm(dbtmp)
    
    # id.notMatchedInHousehold
    # Indique si un identifiant du module éligibilité ne se retrouve pas dans le 
    # le module screening. Les identifiants marqués comme incohérences ne doit pas figuré
    # id.notMatchedInHousehold.msg
    # id.notMatchedInHousehold.desc  : Identifiant non trouvé dans le questionnaire ménage
    idlist <- (eligibilite %>% 
                 filter(as.numeric(elig_commune)!=4 & !(elig_hhmid %in% household.membres$hhmid))
               )$elig_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){
      elt$id.notMatchedInHousehold <- elt$hhmid %in% idSelected
      elt$id.notMatchedInHousehold.msg <- ""
      elt$id.notMatchedInHousehold.desc <- ""
      if(elt$id.notMatchedInHousehold){
        infos <- eligibilite %>% filter(elig_hhmid %in% elt$hhmid)
        label <- ifelse(elt$is.ph,"PH",ifelse(elt$is.pt,"PT","ID"))
        elt$id.notMatchedInHousehold.msg <- paste(sprintf("%s-%s (%s) non retrouvée dans la base ménage",
                                                label,elt$hhmid,infos$idlab01),
                                        collapse = ", ")
        elt$id.notMatchedInHousehold.desc<- "Possible confusion de l'identifiant de l'individu lors de la saisie"   
      }
      invisible(elt)
    })
    # Test des résultats ci-dessus
    # idSelected <- idlist[sample(1:length(idlist),1)]
    # varlist <- c("id.notMatchedInHousehold","id.notMatchedInHousehold.msg","id.notMatchedInHousehold.desc")
    # idSelected %in% dbTriplette$hhmid
    # result[[idSelected]][varlist]
    # rm(idSelected,varlist)
    rm(idlist)
    
    # id.notMatchedInEligibility
    # Indique si un identifiant du module handicap ne se retrouve pas dans 
    # le module éligibilité. 
    # id.notMatchedInEligibility.msg
    # id.notMatchedInEligibility.des  : Identifiant non trouvé dans le module éligibilité
    idlist <- (handicap %>% 
                 filter(as.numeric(h_commune)!=4 & !(h_hhmid %in% household.membres$hhmid))
               )$h_hhmid
    result <- lapply(result, function(elt,idSelected=idlist){
      elt$id.notMatchedInEligibility <- elt$hhmid %in% idSelected
      elt$id.notMatchedInEligibility.msg <- ""
      elt$id.notMatchedInEligibility.desc <- ""
      if(elt$id.notMatchedInEligibility){
        infos <- handicap %>% filter(h_hhmid %in% elt$hhmid)
        label <- ifelse(elt$is.ph,"PH",ifelse(elt$is.pt,"PT","ID"))
        elt$id.notMatchedInEligibility.msg <- paste(sprintf("%s-%s (%s) non retrouvée dans la base éligibilité.",
                                                          label,elt$hhmid,infos$idlab01),
                                                  collapse = ", ")
        elt$id.notMatchedInEligibility.desc<- "Possible confusion de l'identifiant de l'individu lors de la saisie"   
      }
      invisible(elt)
    })
    # Test des résultats ci-dessus
    # idSelected <- idlist[sample(1:length(idlist),1)]
    # varlist <- c("id.notMatchedInEligibility","id.notMatchedInEligibility.msg","id.notMatchedInEligibility.desc")
    # idSelected %in% dbTriplette$hhmid
    # result[[idSelected]][varlist]
    # rm(idSelected,varlist)
    rm(idlist)
    
    # id.notMatched
    # Indique si un identifiant du module handicap ne se retrouve pas dans au moins une des
    # phases antérieure
    # id.notMatched.msg
    # id.notMatched.des  : Identifiant non trouvé dans la base de donnée
    result <- lapply(result, function(elt,idSelected=idlist){
      elt$id.notMatched <- elt$id.notMatchedInHousehold || 
        ((elt$handicap.isPartial | elt$handicap.isCompleted) && elt$id.notMatchedInEligibility)
      elt$id.notMatched.msg <- ""
      elt$id.notMatched.desc <- ""
      if(elt$id.notMatched){
        infos <- handicap %>% filter(h_hhmid %in% elt$hhmid)
        label <- ifelse(elt$is.ph,"PH",ifelse(elt$is.pt,"PT","ID"))
        elt$id.notMatched.msg <- paste(sprintf("%s-%s (%s) non retrouvée dans la base de données.",
                                                            label,elt$hhmid,infos$idlab01),
                                                    collapse = ", ")
        elt$id.notMatched.desc<- "Possible confusion de l'identifiant de l'individu lors de la saisie"   
      }
      invisible(elt)
    })
    # Test des résultats ci-dessus
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$id.notMatched))])
    # idSelected <- idlist[sample(1:length(idlist),1)]
    # varlist <- c("id.notMatched","id.notMatched.msg","id.notMatched.desc")
    # idSelected %in% dbTriplette$hhmid
    # result[[idSelected]][varlist]
    # rm(idSelected,varlist,idlist)
    
    # duplicate.subjectCode
    # Indique pour un individu si son code sujet apparait plus d'une fois dans la base
    # de données Doublons au niveau du code sujet
    # duplicateSubjectCode.msg : string : PH-xxxxx : Bxxx; PT-xxxxx: Bxxxx
    # duplicate.subjectCode.desc : 
    result <- lapply(result, function(elt){
      
      elt$duplicate.subjectCode <- FALSE
      elt$duplicate.subjectCode.msg <- ""
      elt$duplicate.subjectCode.desc <- ""
      
      if(length(elt$codeSujet)){
        infos <- names(result[unlist(lapply(result,
                                            function(subelt,idlab=elt$codeSujet){
                                              length(subelt$codeSujet)>0 &&
                                                (subelt$is.interviewed || subelt$interview.inProgress) && 
                                                !subelt$interview.wasRefused && !subelt$is.unaivailable && 
                                                (subelt$codeSujet %in% idlab)
                                            }))])
        elt$duplicate.subjectCode <- length(infos)>1
        if(elt$duplicate.subjectCode){
          nb.duplicates <- length(infos)
          varlist <- paste(infos,sep=", ",collapse = ", ")
          elt$duplicate.subjectCode.msg <- sprintf("Le code sujet %s a été saisi %02d fois. Identifiants concernés : %s",
                                                   elt$codeSujet,nb.duplicates,varlist)
          elt$duplicate.subjectCode.desc <- "Données saisies plus d'une fois dans le module handicap"
        }
      }
      invisible(elt)
    })
    # Test de verification du resultat produit ci-dessus
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$duplicate.subjectCode))])
    # varlist <- c("hhmid","codeSujet","is.interviewed","interview.inProgress","duplicate.subjectCode",
    #              "duplicate.subjectCode.msg","duplicate.subjectCode.desc")
    # lapply(idlist,function(id,varnames=varlist) result[[id]][varlist])
    # rm(idlist,varlist)
    
    # inconsistency.subjectCode
    # Indique si le  code sujet d'un individu se trouve dans la bonne plage de valeur
    # Pour les PH [1-999] et pour les PT [1001,1999]
    # inconsistency.subjectCode.msg
    # inconsistency.subjectCode.desc : 
    # si PH : "Le code sujet n'est pas dans la plage de valeur [1;999]"
    # si PT : "Le code sujet n'est pas dans la plage de valeur [1001;1999]"
    result <- lapply(result, function(elt){
      value <- as.numeric(str_replace_all(elt$codeSujet,"B",""))
      elt$inconsistency.subjectCode <- !is.na(value) && ((elt$is.ph && (value<1 || value>999))||
                                                           (elt$is.pt && (value<1001 || value>1999)))
      elt$inconsistency.subjectCode.msg <- ""
      elt$inconsistency.subjectCode.desc <- ""
      
      if(elt$inconsistency.subjectCode){
        idlab<-"PH"; plage <- "[1;999]"
        if(elt$is.pt){idlab<-"PT"; plage <- "[1001;1999]"}
        elt$inconsistency.subjectCode.msg <- sprintf("le code sujet %s du %s n'est pas dans la plage de valeur %s",
                                                     elt$codeSujet,idlab,plage)
        elt$inconsistency.subjectCode.desc <- "Incohérence au niveau du code sujet"
      }
      invisible(elt)
    })
    # Test le résultat ci-dessus
    # idlist <- names(result[unlist(lapply(result,function(elt){
    #   elt$inconsistency.subjectCode
    # }))])
    # if(length(idlist)){
    #   result[[idlist[sample(1:length(idlist),1)]]][c("hhmid","codeSujet",
    #                                                  "inconsistency.subjectCode",
    #                                                  "inconsistency.subjectCode.msg",
    #                                                  "inconsistency.subjectCode.desc")]
    # }
    # rm(idlist)
    
    # Identifions les PT qui ont un code de sujet différent de leur PH (PH: 5--> PT 1005)
    # pt.subjectCodeInconsistency
    # pt.subjectCodeInconsistency.msg
    # pt.subjectCodeInconsistency.desc
    #"Le code sujet affecté au PT ne correspond pas du PH auquel il est apparié"
    result <- lapply(result,function(elt){
      code.pt <- as.numeric(str_replace_all(elt$codeSujet,"B",""))
      code.ph <- as.numeric(str_replace_all(elt$codeSujet02,"B",""))
      
      elt$pt.subjectCodeInconsistency <- elt$is.pt && (is.na(code.ph) || is.na(code.pt) ||
                                                          ((1000+code.ph)!=code.pt))
      elt$pt.subjectCodeInconsistency.msg <- ""
      elt$pt.subjectCodeInconsistency.desc <- ""
     
      if(elt$pt.subjectCodeInconsistency){
        elt$pt.subjectCodeInconsistency.msg <- sprintf("Le code sujet (%s) affecté au PT ne correspond pas à celui (%s) du PH auquel il est apparié",
                                                       elt$codeSujet,elt$codeSujet02)
        elt$pt.subjectCodeInconsistency.desc <- "Incohérence code sujet PT vs PH"
      }
      invisible(elt)
    })
    
    # inconsistency.personAge
    # Verifions les individus dont l'année de naissance ne respecte pas n'est pas dans
    # l'intervalle d'âge de 15 et 49 ans
    result <- lapply(result,function(elt){
      infos <- handicap %>% filter(h_hhmid %in% elt$hhmid)
      # elt$inconsistency.personAge <- any(infos$incoherences_detectees_02 & !infos$incoherences_justifiees_02)
      elt$inconsistency.personAge <- is.na(infos$hage) || any(!is.na(infos$hage) && infos$hage<15 | infos$hage>49) 
      if(is.na(elt$inconsistency.personAge)) elt$inconsistency.personAge <- TRUE
      
      elt$inconsistency.personAge.msg <- ""
      elt$inconsistency.personAge.desc <- ""
      if(elt$inconsistency.personAge){
        elt$inconsistency.personAge.msg <- paste(sprintf("L'âge de %s (ID-%s) est de %s",
                                                         elt$codeSujet,elt$hhmid,infos$hage),
                                                 collapse = ", ")
        elt$inconsistency.personAge.desc <- "Non respect du critère sur l'âge"
      }
      invisible(elt)
    })
    # Test du résultat
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$inconsistency.personAge))])
    # varlist <- c("hhmid","codeSujet","inconsistency.personAge","inconsistency.personAge.msg",
    #              "inconsistency.personAge.desc")
    # lapply(idlist,function(id,varnames=varlist) result[[id]][varlist])
    # rm(idlist,varlist)
    
    # # no.matchedPH
    # # Indique d'un PT n'est apparié à aucun PH
    # result <- lapply(result,function(elt){
    #   elt$no.matchedPH <- elt$is.pt && (length(str_trim(elt$caseid02))==0 || 
    #                                       length(str_trim(elt$codeSujet02))==0)
    #   elt$no.matchedPH.msg <- ""
    #   elt$no.matchedPH.desc <- ""
    #   if(elt$no.matchedPH){
    #     elt$no.matchedPH.msg <- paste(sprintf("%s (ID-%s) est apparié à aucun PH",
    #                                           elt$codeSujet,elt$hhmid),collapse = ", ")
    #     elt$no.matchedPH.desc <- "Le PT n'est apparié à aucun PH."
    #   }
    #   invisible(elt)
    # })
    # # Test du résultat
    # # idlist <- names(result[unlist(lapply(result,function(elt) elt$no.matchedPH))])
    # # varlist <- c("hhmid","codeSujet","no.matchedPH","no.matchedPH.msg","no.matchedPH.desc")
    # # lapply(idlist,function(id,varnames=varlist) result[[id]][varlist])
    # # rm(idlist,varlist)
    
    # pt.birthDateInconsistencyWithPh
    # Verifions les PT dont l'année de naissance n'est pas +/- 5 ans celui de son PH
    result <- lapply(result,function(elt){
      infos <- handicap %>% filter(h_hhmid %in% elt$hhmid)
      # elt$pt.birthDateInconsistencyWithPh <- elt$is.pt && 
      #   any(infos$incoherences_detectees_03 & !infos$incoherences_justifiees_03)
      
      elt$pt.birthDateInconsistencyWithPh <- FALSE
      ph.infos <- NULL
      if(elt$is.pt)  ph.infos <- handicap %>% filter(h_hhmid %in% elt$caseid02)
      
      if(!is.null(ph.infos) && dim(ph.infos)[1]){
        elt$pt.birthDateInconsistencyWithPh <- elt$is.pt && 
          ( (is.na(infos$h100) | is.na(ph.infos$h100)) || 
            (!is.na(infos$h100) & !is.na(ph.infos$h100) & infos$h100>(ph.infos$h100+5) |infos$h100<(ph.infos$h100-5))
          )
        if(is.na(elt$pt.birthDateInconsistencyWithPh)) elt$pt.birthDateInconsistencyWithPh <- TRUE
      }
      
      elt$pt.birthDateInconsistencyWithPh.msg <- ""
      elt$pt.birthDateInconsistencyWithPh.desc <- ""
      if(elt$pt.birthDateInconsistencyWithPh){
        pt.birthDate <- infos$h100
        ph.birthDate <- ph.infos$h100 #ifelse(!is.null(ph.infos) && dim(ph.infos)[1],ph.infos$h100,NA) 
        elt$pt.birthDateInconsistencyWithPh.msg <- paste(sprintf("L'année de naissance [%s] de %s (PT-%s) ne correspond pas à +/-5ans l'année de naissance [%s] de %s (PH-%s)",
                                                         as.character(pt.birthDate),elt$codeSujet,elt$hhmid,
                                                         as.character(ph.birthDate),elt$codeSujet02,elt$caseid02),
                                                 collapse = ", ")
        elt$pt.birthDateInconsistencyWithPh.desc <- "Non respect critère âge PT vs âge PH"
      }
      invisible(elt)
    })
    # Test du résultat
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$pt.birthDateInconsistencyWithPh))])
    # varlist <- c("hhmid","codeSujet","pt.birthDateInconsistencyWithPh",
    #              "pt.birthDateInconsistencyWithPh.msg","pt.birthDateInconsistencyWithPh.desc")
    # lapply(idlist,function(id,varnames=varlist) result[[id]][varlist])
    # rm(idlist,varlist)
    
    # pt.sexInconsistencyWithPh
    # Verifions les PT qui ont sexe différent de leur PH
    result <- lapply(result,function(elt){
      infos <- handicap %>% filter(h_hhmid %in% elt$hhmid)
      # elt$pt.sexInconsistencyWithPh <- elt$is.pt && 
      #   any(infos$incoherences_detectees_04 & !infos$incoherences_justifiees_04)
      
      elt$pt.sexInconsistencyWithPh <- FALSE
      ph.infos <- NULL
      if(elt$is.pt)  ph.infos <- handicap %>% filter(h_hhmid %in% elt$caseid02)
      
      if(!is.null(ph.infos) && dim(ph.infos)[1]){
        elt$pt.sexInconsistencyWithPh <- elt$is.pt && (
          (is.na(infos$h101) | is.na(ph.infos$h101)) || 
          (!is.na(infos$h101) & !is.na(ph.infos$h101) & infos$h101!=ph.infos$h101))
        if(is.na(elt$pt.sexInconsistencyWithPh)) elt$pt.sexInconsistencyWithPh <- TRUE
      }
      
      elt$pt.sexInconsistencyWithPh.msg <- ""
      elt$pt.sexInconsistencyWithPh.desc <- ""
      if(elt$pt.sexInconsistencyWithPh){
        pt.sex <- infos$h101
        ph.sex <- ph.infos$h101 #ifelse(!is.null(ph.infos) && dim(ph.infos)[1],ph.infos$h101,NA)
        elt$pt.sexInconsistencyWithPh.msg <- paste(sprintf("Le sexe [%s] de %s (PT-%s) ne correspond pas à celui [%s] de %s (PH-%s)",
                                                                 as.character(pt.sex),elt$codeSujet,elt$hhmid,
                                                                 as.character(ph.sex),elt$codeSujet02,elt$caseid02),
                                                         collapse = ", ")
        elt$pt.sexInconsistencyWithPh.desc <- "Non respect critère sexe PT vs sexe PH"
      }
      invisible(elt)
    })
    # Test du résultat
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$pt.sexInconsistencyWithPh))])
    # varlist <- c("hhmid","codeSujet","pt.sexInconsistencyWithPh",
    #              "pt.sexInconsistencyWithPh.msg","pt.sexInconsistencyWithPh.desc")
    # lapply(idlist,function(id,varnames=varlist) result[[id]][varlist])
    # rm(idlist,varlist)
    
    # inconsistency.numberOfResource
    # "Incohérence grille ressource" 
    # inconsistency.numberOfResource.msg : Le nombre de ressources déclaré (%d) différe de celui des périodes portées sur la grille (%d)
    # inconsistency.numberOfResource.desc= Contrôler la concordance entre les données dans l'application et celle de la grille
    result <- lapply(result,function(elt){
      infos <- handicap %>% filter(h_hhmid %in% elt$hhmid)
      # elt$inconsistency.numberOfResource <- any(infos$incoherences_detectees_05 & 
      #                                             !infos$incoherences_justifiees_05)
      elt$inconsistency.numberOfResource <- (!is.na(infos$resrc01) & is.na(infos$activ01)) || 
        (is.na(infos$resrc01) & !is.na(infos$activ01)) ||
        any(!is.na(infos$resrc01) & !is.na(infos$activ01) & as.numeric(infos$resrc01)!=as.numeric(infos$activ01)) 
      
      if(is.na(elt$inconsistency.numberOfResource)) elt$inconsistency.numberOfResource <- TRUE
      elt$inconsistency.numberOfResource.msg <- ""
      elt$inconsistency.numberOfResource.desc <- ""
      if(elt$inconsistency.numberOfResource){
        elt$inconsistency.numberOfResource.msg <- paste(sprintf("Le module ressource est %s appliquée alors que celui des activités est %s appliquée (nbre.ressource=%d vs nbre.activité=%d)",
                                                           ifelse(as.numeric(infos$resrc01)==1,"","non"),
                                                           ifelse(as.numeric(infos$activ01)==1,"","non"),
                                                           infos$resrc03,infos$activ03),
                                                   collapse = ", ")
        elt$inconsistency.numberOfResource.desc <- "Incohérence grille ressource : contrôler la concordance entre les données dans l'application et celle de la grille"
      }
      invisible(elt)
    })
    # Test du résultat
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$inconsistency.numberOfResource))])
    # varlist <- c("hhmid","codeSujet","inconsistency.numberOfResource",
    #              "inconsistency.numberOfResource.msg","inconsistency.numberOfResource.desc")
    # lapply(idlist,function(id,varnames=varlist) result[[id]][varlist])
    # rm(idlist,varlist)
    
    # inconsistency.numberOfPreoccupation
    # "Incohérence grille préoccupation"
    # Le nombre de préoccupations déclaré (%d) différe de celui des périodes portées sur la grille (%d)
    # Contrôler la concordance entre les données dans l'application et celle de la grille
    result <- lapply(result,function(elt){
      infos <- handicap %>% filter(h_hhmid %in% elt$hhmid)
      # elt$inconsistency.numberOfPreoccupation <- any(infos$incoherences_detectees_06 & 
      #                                             !infos$incoherences_justifiees_06)
      elt$inconsistency.numberOfPreoccupation <- (is.na(infos$preoc01) & !is.na(infos$activ01)) ||
        (!is.na(infos$preoc01) & is.na(infos$activ01)) ||
        any(!is.na(infos$preoc01) & !is.na(infos$activ01) & as.numeric(infos$preoc01)!=as.numeric(infos$activ01))
      
      if(is.na(elt$inconsistency.numberOfPreoccupation)) elt$inconsistency.numberOfPreoccupation <- TRUE
      elt$inconsistency.numberOfPreoccupation.msg <- ""
      elt$inconsistency.numberOfPreoccupation.desc <- ""
      if(elt$inconsistency.numberOfPreoccupation){
        elt$inconsistency.numberOfPreoccupation.msg <- paste(sprintf("Le module préoccupation est %s appliquée alors que celui des activités est %s appliquée (nbre.preoccupation=%d vs nbre.activité=%d)",
                                                                ifelse(as.numeric(infos$preoc01)==1,"","non"),
                                                                ifelse(as.numeric(infos$activ01)==1,"","non"),
                                                                infos$preoc03,infos$activ03),
                                                        collapse = ", ")
        elt$inconsistency.numberOfPreoccupation.desc <- "Incohérence grille préoccupation : contrôler la concordance entre les données dans l'application et celle de la grille"
      }
      invisible(elt)
    })
    # Test du résultat
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$inconsistency.numberOfPreoccupation))])
    # varlist <- c("hhmid","codeSujet","inconsistency.numberOfPreoccupation",
    #              "inconsistency.numberOfPreoccupation.msg","inconsistency.numberOfPreoccupation.desc")
    # lapply(idlist,function(id,varnames=varlist) result[[id]][varlist])
    # rm(idlist,varlist)
    
    # inconsistency.numberOfChild
    # "Nombre d'enfant déclaré différent du Nombre d'enfants renseigné sur la grille"
    # Le nombre d'enfants déclaré (%d) différe de celui des périodes portées sur la grille (%d)
    # Contrôler la concordance entre les données dans l'application et celle de la grille
    result <- lapply(result,function(elt){
      infos <- handicap %>% filter(h_hhmid %in% elt$hhmid)
      # elt$inconsistency.numberOfChild <- any(infos$incoherences_detectees_12 & 
      #                                          !infos$incoherences_justifiees_12)
      elt$inconsistency.numberOfChild <- (!is.na(infos$h512) & infos$h512!=0 & is.na(infos$enft03)) || 
        (is.na(infos$h512) & !is.na(infos$enft03)) ||
        any(!is.na(infos$h512) & !is.na(infos$enft03) & infos$h512 != infos$enft03)
        
      if(is.na(elt$inconsistency.numberOfChild)) elt$inconsistency.numberOfChild <- TRUE
      
      elt$inconsistency.numberOfChild.msg <- ""
      elt$inconsistency.numberOfChild.desc <- ""
      if(elt$inconsistency.numberOfChild){
        elt$inconsistency.numberOfChild.msg <- paste(sprintf("Nombre d'enfant (%d) déclaré différent du nombre d'enfants (%d) renseigné sur la grille",
                                                                     infos$h512,infos$enft03),
                                                             collapse = ", ")
        elt$inconsistency.numberOfChild.desc <- "Contrôler la concordance entre les données dans l'application et celle de la grille"
      }
      invisible(elt)
    })
    # Test du résultat
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$inconsistency.numberOfChild))])
    # varlist <- c("hhmid","codeSujet","inconsistency.numberOfChild",
    #              "inconsistency.numberOfChild.msg","inconsistency.numberOfChild.desc")
    # lapply(idlist,function(id,varnames=varlist) result[[id]][varlist])
    # rm(idlist,varlist)
    
    # "Incohérence sur le nombre de parents proches vivant à Bujumbura"
    # inconsistency.numberOfParentInBujumbura
    # Le nombre de parent proches vivant à bujumbura (%d) est supérieur au nombre de parents proches (%d)
    # Porter la correction dans le questionnaire
    result <- lapply(result,function(elt){
      infos <- handicap %>% filter(h_hhmid %in% elt$hhmid)
      # elt$inconsistency.numberOfParentInBujumbura <- any(infos$incoherences_detectees_07 & 
      #                                          !infos$incoherences_justifiees_07)
      elt$inconsistency.numberOfParentInBujumbura <- (!is.na(infos$h219) & is.na(infos$h218)) ||
        any(!is.na(infos$h219) & !is.na(infos$h218) & infos$h219 > infos$h218)
        
        
      if(is.na(elt$inconsistency.numberOfParentInBujumbura)) elt$inconsistency.numberOfParentInBujumbura <- TRUE
      elt$inconsistency.numberOfParentInBujumbura.msg <- ""
      elt$inconsistency.numberOfParentInBujumbura.desc <- ""
      if(elt$inconsistency.numberOfParentInBujumbura){
        elt$inconsistency.numberOfParentInBujumbura.msg <- paste(sprintf("Le nombre de parent proches vivant à bujumbura (%d) est supérieur au nombre de parents proches (%d)",
                                                             infos$h219,infos$h218),
                                                     collapse = ", ")
        elt$inconsistency.numberOfParentInBujumbura.desc <- "Contrôler la concordance entre les données dans l'application et celle de la grille"
      }
      invisible(elt)
    })
    # Test du résultat
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$inconsistency.numberOfParentInBujumbura))])
    # varlist <- c("hhmid","codeSujet","inconsistency.numberOfParentInBujumbura",
    #              "inconsistency.numberOfParentInBujumbura.msg","inconsistency.numberOfParentInBujumbura.desc")
    # lapply(idlist,function(id,varnames=varlist) result[[id]][varlist])
    # rm(idlist,varlist)
    
    # "Incohérence sur le nombre de parents proches visités au moins 1 fois par mois"
    # inconsistency.numberOfParentVisited
    # Le nombre de parent proches visités (%d) est supérieur au nombre de parents proches(%d)
    # Porter la correction dans le questionnaire
    result <- lapply(result,function(elt){
      infos <- handicap %>% filter(h_hhmid %in% elt$hhmid)
      # elt$inconsistency.numberOfParentVisited <- any(infos$incoherences_detectees_08 & 
      #                                                      !infos$incoherences_justifiees_08)
      elt$inconsistency.numberOfParentVisited <- (!is.na(infos$h220) & is.na(infos$h218)) ||
        any(!is.na(infos$h220) & !is.na(infos$h218) & infos$h220 > infos$h218)
        
      
      if(is.na(elt$inconsistency.numberOfParentVisited)) elt$inconsistency.numberOfParentVisited <- TRUE
      elt$inconsistency.numberOfParentVisited.msg <- ""
      elt$inconsistency.numberOfParentVisited.desc <- ""
      if(elt$inconsistency.numberOfParentVisited){
        elt$inconsistency.numberOfParentVisited.msg <- paste(sprintf("Le nombre de parent proches visités (%d) est supérieur au nombre de parents proches(%d)",
                                                                         infos$h220,infos$h218),
                                                                 collapse = ", ")
        elt$inconsistency.numberOfParentVisited.desc <- "Porter la correction dans le questionnaire"
      }
      invisible(elt)
    })
    # Test du résultat
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$inconsistency.numberOfParentVisited))])
    # varlist <- c("hhmid","codeSujet","inconsistency.numberOfParentVisited",
    #              "inconsistency.numberOfParentVisited.msg","inconsistency.numberOfParentVisited.desc")
    # lapply(idlist,function(id,varnames=varlist) result[[id]][varlist])
    # rm(idlist,varlist)
    
    # "Incohérence sur le nombre d'amis proches atteints d'un handicap"
    # inconsistency.numberOfDisabledFriend
    # Le nombre d'amis proches atteints d'un handicap (%d) est supérieur au nombre d'amis proches (%d)
    # Porter la correction dans le questionnaire
    result <- lapply(result,function(elt){
      infos <- handicap %>% filter(h_hhmid %in% elt$hhmid)
      # elt$inconsistency.numberOfDisabledFriend <- any(infos$incoherences_detectees_09 & 
      #                                                  !infos$incoherences_justifiees_09)
      elt$inconsistency.numberOfDisabledFriend <- (!is.na(infos$h222) & is.na(infos$h221)) ||
        any(!is.na(infos$h222) & !is.na(infos$h221) & infos$h222>infos$h221)
        
      if(is.na(elt$inconsistency.numberOfDisabledFriend)) elt$inconsistency.numberOfDisabledFriend <- TRUE
      elt$inconsistency.numberOfDisabledFriend.msg <- ""
      elt$inconsistency.numberOfDisabledFriend.desc <- ""
      if(elt$inconsistency.numberOfDisabledFriend){
        elt$inconsistency.numberOfDisabledFriend.msg <- paste(sprintf("Le nombre d'amis proches atteints d'un handicap (%d) est supérieur au nombre d'amis proches (%d)",
                                                                     infos$h222,infos$h221),
                                                             collapse = ", ")
        elt$inconsistency.numberOfDisabledFriend.desc <- "Porter la correction dans le questionnaire"
      }
      invisible(elt)
    })
    # Test du résultat
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$inconsistency.numberOfDisabledFriend))])
    # varlist <- c("hhmid","codeSujet","inconsistency.numberOfDisabledFriend",
    #              "inconsistency.numberOfDisabledFriend.msg","inconsistency.numberOfDisabledFriend.desc")
    # lapply(idlist,function(id,varnames=varlist) result[[id]][varlist])
    # rm(idlist,varlist)
    
    # "Incohérence sur le nombre d'amis proches vivant à Bujumbura"
    # Le nombre d'mis proches vivant à Bujumbura (%d) est supérieur au nombre d'amis proches (%d)
    # inconsistency.numberOfFriendInBujumbura
    # Porter la correction dans le questionnaire
    result <- lapply(result,function(elt){
      infos <- handicap %>% filter(h_hhmid %in% elt$hhmid)
      # elt$inconsistency.numberOfFriendInBujumbura <- any(infos$incoherences_detectees_10 & 
      #                                                   !infos$incoherences_justifiees_10)
      elt$inconsistency.numberOfFriendInBujumbura <- (!is.na(infos$h223) & is.na(infos$h221)) ||
        any(!is.na(infos$h223) & !is.na(infos$h221) & infos$h223>infos$h221)
        
      
      if(is.na(elt$inconsistency.numberOfFriendInBujumbura)) elt$inconsistency.numberOfFriendInBujumbura <- TRUE
      elt$inconsistency.numberOfFriendInBujumbura.msg <- ""
      elt$inconsistency.numberOfFriendInBujumbura.desc <- ""
      if(elt$inconsistency.numberOfFriendInBujumbura){
        elt$inconsistency.numberOfFriendInBujumbura.msg <- paste(sprintf("Le nombre d'amis proches vivant à Bujumbura (%d) est supérieur au nombre d'amis proches (%d)",
                                                                      infos$h223,infos$h221),
                                                              collapse = ", ")
        elt$inconsistency.numberOfFriendInBujumbura.desc <- "Porter la correction dans le questionnaire"
      }
      invisible(elt)
    })
    # Test du résultat
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$inconsistency.numberOfFriendInBujumbura))])
    # varlist <- c("hhmid","codeSujet","inconsistency.numberOfFriendInBujumbura",
    #              "inconsistency.numberOfFriendInBujumbura.msg","inconsistency.numberOfFriendInBujumbura.desc")
    # lapply(idlist,function(id,varnames=varlist) result[[id]][varlist])
    # rm(idlist,varlist)
    
    # inconsistency.numberOfFriendVisited
    # "Incohérence sur le nombre d'amis proches visités au moins 1 fois par mois"
    # Le nombre d'amis proches visités (%d) est supérieur au nombre d'amis proches (%d)
    # Porter la correction dans le questionnaire
    result <- lapply(result,function(elt){
      infos <- handicap %>% filter(h_hhmid %in% elt$hhmid)
      # elt$inconsistency.numberOfFriendVisited <- any(infos$incoherences_detectees_11 & 
      #                                                      !infos$incoherences_justifiees_11)
      elt$inconsistency.numberOfFriendVisited <- (!is.na(infos$h224) & is.na(infos$h221)) ||
        any(!is.na(infos$h224) & !is.na(infos$h221) & infos$h224>infos$h221)
      
      if(is.na(elt$inconsistency.numberOfFriendVisited)) elt$inconsistency.numberOfFriendVisited <- TRUE
      elt$inconsistency.numberOfFriendVisited.msg <- ""
      elt$inconsistency.numberOfFriendVisited.desc <- ""
      if(elt$inconsistency.numberOfFriendVisited){
        elt$inconsistency.numberOfFriendVisited.msg <- paste(sprintf("Le nombre d'amis proches visités (%d) est supérieur au nombre d'amis proches (%d)",
                                                                         infos$h224,infos$h221),
                                                                 collapse = ", ")
        elt$inconsistency.numberOfFriendVisited.desc <- "Porter la correction dans le questionnaire"
      }
      invisible(elt)
    })
    # Test du résultat
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$inconsistency.numberOfFriendVisited))])
    # varlist <- c("hhmid","codeSujet","inconsistency.numberOfFriendVisited",
    #              "inconsistency.numberOfFriendVisited.msg","inconsistency.numberOfFriendVisited.desc")
    # lapply(idlist,function(id,varnames=varlist) result[[id]][varlist])
    # rm(idlist,varlist)
    
    # ph.withoutPT
    # Liste des PH n'ayant plus de PT disponibles (tous ont refusé de participer à l'enquête ou sont 
    # indisponible) et on en a aucun dans la base des triplettes qui n'est pas encore été inséré
    # dans la base éligibilité
    # ph.withoutPT.msg
    # ph.withoutPT.desc
    result <- lapply(result, function(elt){
      
      pt.list.full <- pt.list.refusal <- NULL
      if(exists("dbTriplette") && !is.null(dbTriplette))
        pt.list.full <- (dbTriplette %>% filter(idPh==elt$hhmid))$hhmid
      if(!is.null(eligibilite))
        pt.list.refusal  <- (eligibilite %>% filter(caseid02==elt$hhmid &
                                                    as.numeric(statut_denombrement) %in% c(4,5)))$hhmid
      
      elt$ph.withoutPT <- elt$is.ph && !elt$interview.wasRefused && !elt$is.unaivailable&& 
        !elt$is.matched && 
        length(setdiff(str_sort(unique(pt.list.full),numeric = T),
                       str_sort(unique(pt.list.refusal),numeric = T)))==0
      elt$ph.withoutPT.msg <- ""
      elt$ph.withoutPT.desc <- ""
      if(elt$ph.withoutPT){
        elt$ph.withoutPT.msg <- paste(sprintf("Tirer des PT de remplacement pour le PH-%s (%s)",
                                              elt$hhmid,elt$codeSujet),
                                      collapse = ", ")
        elt$ph.withoutPT.desc <- "PH sans aucun PT disponible"
      }
      invisible(elt)
    })
    # Test des résultats ci-dessus
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$ph.withoutPT))])
    # result[[idlist[sample(1:length(idlist),1)]]]
    # rm(idlist)
    
    # pt.withoutPH
    # Identifie des PT qui auraient été interviewé sans avoir été apparié
    result <- lapply(result,function(elt){
      elt$pt.withoutPH <- elt$is.pt && all(nchar(elt$caseid02)==0)
      elt$pt.withoutPH.msg <- ""
      elt$pt.withoutPH.desc <- ""
      if(elt$pt.withoutPH){
        elt$pt.withoutPH.msg <- paste(sprintf("Le PT-%s (%s) n'est associé à aucun PH",
                                              elt$hhmid,elt$codeSujet),
                                      collapse = ", ")
        elt$pt.withoutPH.desc <- "PT sans PH"
      }
      invisible(elt)
    })
    # Test des résultats ci-dessus
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$pt.withoutPH))])
    # result[[idlist[sample(1:length(idlist),1)]]]
    # rm(idlist)
    
    # inconsistency.numberOfActivePT
    # Indique pour un PH si il a plus d'un PT encours d'interview ou enquêté
    # dans la base éligibilité
    result <- lapply(result, function(elt){
      
      pt.list <- NULL
      if(!is.null(eligibilite))
        pt.list <- (eligibilite %>% filter(caseid02==elt$hhmid &
                                             as.numeric(statut_denombrement) %in% c(1:3)))$hhmid
      
      elt$inconsistency.numberOfActivePT <- elt$is.ph && length(pt.list)>1
      elt$inconsistency.numberOfActivePT.msg <- ""
      elt$inconsistency.numberOfActivePT.desc <- ""
      if(elt$inconsistency.numberOfActivePT){
        elt$inconsistency.numberOfActivePT.msg <- paste(sprintf("Le PH-%s (%s) a %d PT actifs (%s)",
                                                                elt$hhmid,elt$codeSujet,length(pt.list),
                                                                paste("PT-",pt.list,collapse = ", ")),
                                                        collapse = ", ")
        elt$inconsistency.numberOfActivePT.desc <- "Incohérence sur le nombre de PT actif pour un PH"
      }
      invisible(elt)
    })
    # Test des résultats ci-dessus
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$inconsistency.numberOfActivePT))])
    # result[[idlist[sample(1:length(idlist),1)]]]
    # rm(idlist)
    
    # id.isReusable
    # La propriété reusable (1=Oui, 0=FALSE) : indique les identifiants dans la base  
    # dbTriplette qui peuvent être réutilisés lors des procédures de tirage
    # Si unknown.person et data.isCompleted et (is.ph et !ph.withoutPT)
    # alors on peut le réutiliser pour l'appariement avec un autre
    # peuvent être utilisées. Si Pt on peut abandonner)
    result <- lapply(result, function(elt){
      
      elt$id.isReusable <- elt$data.isCompleted && elt$is.interviewed && 
        ((elt$is.ph && elt$ph.withoutPT ) || (elt$is.pt && elt$pt.withoutPH))
      elt$id.isReusable.msg <- ""
      elt$id.isReusable.desc <- ""
      if(elt$id.isReusable){
        elt$id.isReusable.msg <- paste(sprintf("L'identifiant ID-%s (%s) peut être réutilisé dans une procédure de tirage",
                                               elt$hhmid,elt$codeSujet),collapse = ", ")
        elt$id.isReusable.desc <- "Identifiant peut être réutilisé dans la procèdure de tirage de PH et/ou de PT"
      }
      invisible(elt)
    })
    # Test des résultats ci-dessus
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$id.isReusable))])
    # result[[idlist[sample(1:length(idlist),1)]]]
    # rm(idlist)
    
    # Suppresion des doublons afin de faire migrer la
    # base de list à data.frame plsu facilement manipulable avec dplyr
    result <- lapply(result,function(elt){
      if(elt$data.duplicated){
        varlist <- names(elt)
        elt <- sapply(varlist,function(varname,subelt=elt){
          subelt[[varname]] <- head(subelt[[varname]],1)
          invisible(subelt[varname])
        })
        names(elt) <- varlist
      }
      invisible(elt)
    })
    
    # Suppression des éventuelles questionnaires qui n'ont pas encore été administrée
    # tous ceux qui dans dbTriplette et pas d'éligibilité => pas besoin même de faire un traitement
    # dessus
    result <- result[unlist(lapply(result, function(elt){!elt$form.notAdministered}))]  
    
    # tmp <- bind_rows(result)
    
    # is.inconsistency
    # Indique la liste des erreurs pour chaque individu
    # Chaque variable inconsistency.* représentera la liste des incohérences,
    # des messages et descriptions des incohérences identifiées pour un individu.
    # La fonction str_split avec pour modifier sera utilisée pour crééer des vecteurs
    # permettant de facilement travaillant sur les erreurs d'un questionnaire.
    # Lorsqu'une erreur sera traité on changera sa valeur à false et son messsage et sa description
    # à vide. Puis on les supprimera du vecteur obtenu par str_split.
    # Une fois le traitement fini : certainement dans clean. Si le vecteur est non vide
    # on utilisera paste pour réaffecter dans checkerTriplette l'élément
    # unlist(str_split(paste(c("b","a"),collapse = "#"),pattern = "#",simplify = F))
    # unlist(str_split(paste(c(NULL),collapse = "#"),pattern = "#",simplify = F))
    # Si par contre il vide is.incistency sera affectée à FALSE..
    #inconsistency.var# liste des variables erreurs séparées avec des # pour utiliser str_split
    #inconsistency.msg # liste des variables message des erreurs séparées avec des # pour utiliser str_split
    #inconsistency.desc # liste des variables descripition des erreurs séparées avec des # pour utiliser str_split
    result <- lapply(result,function(elt){
      errlist.var <-  errlist.msg <- errlist.desc <- NULL
      warning.var <- warning.msg <- warning.desc <- NULL

      # Liste des incohérences
      # Données partiellement remplies doit être signalées
      if(elt$data.isPartial){
        errlist.var <- c(errlist.var,"data.isPartial")
        errlist.msg <- c(errlist.msg,elt$data.isPartial.msg)
        errlist.desc <- c(errlist.desc,elt$data.isPartial.desc)
      }

      # Module handicap remplie alors que le module élibilité est partiellement remplie
      if(elt$filledDisability.partialEligibility){
        errlist.var <- c(errlist.var,"filledDisability.partialEligibility")
        errlist.msg <- c(errlist.msg,elt$filledDisability.partialEligibility.msg)
        errlist.desc <- c(errlist.desc,elt$filledDisability.partialEligibility.desc)
      }

      # Identifiant non retrouvé dans les modules Screening ou Eligibilité
      if(elt$id.notMatched){
        errlist.var <- c(errlist.var,"id.notMatched")
        errlist.msg <- c(errlist.msg,elt$id.notMatched.msg)
        errlist.desc <- c(errlist.desc,elt$id.notMatched.desc)
      }

      # Identifiant non présent dans la base dbTriplette
      if(elt$unknown.person){
        errlist.var <- c(errlist.var,"unknown.person")
        errlist.msg <- c(errlist.msg,elt$unknown.person.msg)
        errlist.desc <- c(errlist.desc,elt$unknown.person.desc)
      }

      # PT pas dans la même ZD, ni ZD voisine à celle de son PH
      if(elt$pt.localisationInconsistencyWithPh){
        errlist.var <- c(errlist.var,"pt.localisationInconsistencyWithPh")
        errlist.msg <- c(errlist.msg,elt$pt.localisationInconsistencyWithPh.msg)
        errlist.desc <- c(errlist.desc,elt$pt.localisationInconsistencyWithPh.desc)
      }

      # Répition du code sujet
      if(elt$duplicate.subjectCode){
        errlist.var <- c(errlist.var,"duplicate.subjectCode")
        errlist.msg <- c(errlist.msg,elt$duplicate.subjectCode.msg)
        errlist.desc <- c(errlist.desc,elt$duplicate.subjectCode.desc)
      }

      # Code sujet ne respectant pas l'intervalle [doute que cela se produise]
      if(elt$inconsistency.subjectCode){
        errlist.var <- c(errlist.var,"inconsistency.subjectCode")
        errlist.msg <- c(errlist.msg,elt$inconsistency.subjectCode.msg)
        errlist.desc <- c(errlist.desc,elt$inconsistency.subjectCode.desc)
      }

      # Le code du PT n'est pas conforme à celui de son PH [doute que cela se produise]
      if(elt$pt.subjectCodeInconsistency){
        errlist.var <- c(errlist.var,"pt.subjectCodeInconsistency")
        errlist.msg <- c(errlist.msg,elt$pt.subjectCodeInconsistency.msg)
        errlist.desc <- c(errlist.desc,elt$pt.subjectCodeInconsistency.desc)
      }

      # L'âge de l'individu n'est pas dans les 15-49 ans
      if(elt$inconsistency.personAge){
        errlist.var <- c(errlist.var,"inconsistency.personAge")
        errlist.msg <- c(errlist.msg,elt$inconsistency.personAge.msg)
        errlist.desc <- c(errlist.desc,elt$inconsistency.personAge.desc)
      }

      # L'année de naissance du PT ne respecte pas le critère de +/- 5 ans de celui de son PH
      if(elt$pt.birthDateInconsistencyWithPh){
        errlist.var <- c(errlist.var,"pt.birthDateInconsistencyWithPh")
        errlist.msg <- c(errlist.msg,elt$pt.birthDateInconsistencyWithPh.msg)
        errlist.desc <- c(errlist.desc,elt$pt.birthDateInconsistencyWithPh.desc)
      }

      # L'année de naissance du PT ne respecte pas le critère de +/- 5 ans de celui de son PH
      if(elt$pt.birthDateInconsistencyWithPh){
        errlist.var <- c(errlist.var,"pt.birthDateInconsistencyWithPh")
        errlist.msg <- c(errlist.msg,elt$pt.birthDateInconsistencyWithPh.msg)
        errlist.desc <- c(errlist.desc,elt$pt.birthDateInconsistencyWithPh.desc)
      }

      #Le sex du PT est différent de celui de son PH
      if(elt$pt.sexInconsistencyWithPh){
        errlist.var <- c(errlist.var,"pt.sexInconsistencyWithPh")
        errlist.msg <- c(errlist.msg,elt$pt.sexInconsistencyWithPh.msg)
        errlist.desc <- c(errlist.desc,elt$pt.sexInconsistencyWithPh.desc)
      }

      # Erreur au niveau de la déclaration des ressources
      if(elt$inconsistency.numberOfResource){
        errlist.var <- c(errlist.var,"inconsistency.numberOfResource")
        errlist.msg <- c(errlist.msg,elt$inconsistency.numberOfResource.msg)
        errlist.desc <- c(errlist.desc,elt$inconsistency.numberOfResource.desc)
      }

      # Erreur au niveau de la déclaration des préoccupations
      if(elt$inconsistency.numberOfPreoccupation){
        errlist.var <- c(errlist.var,"inconsistency.numberOfPreoccupation")
        errlist.msg <- c(errlist.msg,elt$inconsistency.numberOfPreoccupation.msg)
        errlist.desc <- c(errlist.desc,elt$inconsistency.numberOfPreoccupation.desc)
      }

      # Erreur au niveau du nombre d'enfant déclaré
      if(elt$inconsistency.numberOfChild){
        errlist.var <- c(errlist.var,"inconsistency.numberOfChild")
        errlist.msg <- c(errlist.msg,elt$inconsistency.numberOfChild.msg)
        errlist.desc <- c(errlist.desc,elt$inconsistency.numberOfChild.desc)
      }

      # Erreur au niveau du nombre de parents proches vivant à Bujumbura
      if(elt$inconsistency.numberOfParentInBujumbura){
        errlist.var <- c(errlist.var,"inconsistency.numberOfParentInBujumbura")
        errlist.msg <- c(errlist.msg,elt$inconsistency.numberOfParentInBujumbura.msg)
        errlist.desc <- c(errlist.desc,elt$inconsistency.numberOfParentInBujumbura.desc)
      }

      # Erreur au niveau du nombre de parents proches visités
      if(elt$inconsistency.numberOfParentVisited){
        errlist.var <- c(errlist.var,"inconsistency.numberOfParentVisited")
        errlist.msg <- c(errlist.msg,elt$inconsistency.numberOfParentVisited.msg)
        errlist.desc <- c(errlist.desc,elt$inconsistency.numberOfParentVisited.desc)
      }

      # Erreur au niveau du nombre d'amis handicapés
      if(elt$inconsistency.numberOfDisabledFriend){
        errlist.var <- c(errlist.var,"inconsistency.numberOfDisabledFriend")
        errlist.msg <- c(errlist.msg,elt$inconsistency.numberOfDisabledFriend.msg)
        errlist.desc <- c(errlist.desc,elt$inconsistency.numberOfDisabledFriend.desc)
      }

      # Erreur au niveau du nombre d'amis vivant à Bujumbura
      if(elt$inconsistency.numberOfFriendInBujumbura){
        errlist.var <- c(errlist.var,"inconsistency.numberOfFriendInBujumbura")
        errlist.msg <- c(errlist.msg,elt$inconsistency.numberOfFriendInBujumbura.msg)
        errlist.desc <- c(errlist.desc,elt$inconsistency.numberOfFriendInBujumbura.desc)
      }

      # Erreur au niveau du nombre d'amis visités
      if(elt$inconsistency.numberOfFriendVisited){
        errlist.var <- c(errlist.var,"inconsistency.numberOfFriendVisited")
        errlist.msg <- c(errlist.msg,elt$inconsistency.numberOfFriendVisited.msg)
        errlist.desc <- c(errlist.desc,elt$inconsistency.numberOfFriendVisited.desc)
      }

      # PH a plus de un PT actif au même moment => erreur agent
      if(elt$inconsistency.numberOfActivePT){
        errlist.var <- c(errlist.var,"inconsistency.numberOfActivePT")
        errlist.msg <- c(errlist.msg,elt$inconsistency.numberOfActivePT.msg)
        errlist.desc <- c(errlist.desc,elt$inconsistency.numberOfActivePT.desc)
      }

      # PT sans PH grosse erreur de programmation..
      if(elt$pt.withoutPH){
        errlist.var <- c(errlist.var,"pt.withoutPH")
        errlist.msg <- c(errlist.msg,elt$pt.withoutPH.msg)
        errlist.desc <- c(errlist.desc,elt$pt.withoutPH.desc)
      }

      elt$is.inconsistency <- !elt$eligibility.inconsistencyOnDeclaration & 
                              !elt$handicap.inconsistencyOnDeclaration &
                              !elt$is.unaivailable & !elt$interview.wasRefused & length(errlist.var)>0
      elt$inconsistency.var <- paste(errlist.var,collapse="#")
      elt$inconsistency.msg <- paste(errlist.msg,collapse="#")
      elt$inconsistency.desc <- paste(errlist.desc,collapse="#")

      # Liste des warning
      ## Changement de la catégorie après le module éligibilité
      if(elt$changeCategorie){
        warning.var <- c(warning.var,"changeCategorie")
        warning.msg <- c(warning.msg,elt$changeCategorie.msg)
        warning.desc <- c(warning.desc,elt$changeCategorie.desc)
      }

      ## Le sexe de l'individu est différent de celui déclaré au screening
      if(elt$sex.differentValue){
        warning.var <- c(warning.var,"sex.differentValue")
        warning.msg <- c(warning.msg,elt$sex.differentValue.msg)
        warning.desc <- c(warning.desc,elt$sex.differentValue.desc)
      }

      ## La date de naissance de l'individu est différente de celle déclarée au screening
      if(elt$birthdate.differentValue){
        warning.var <- c(warning.var,"birthdate.differentValue")
        warning.msg <- c(warning.msg,elt$birthdate.differentValue.msg)
        warning.desc <- c(warning.desc,elt$birthdate.differentValue.desc)
      }

      elt$has.warning <- !elt$is.unaivailable & !elt$interview.wasRefused & length(warning.var)>0
      elt$warning.var <- paste(warning.var,collapse="#")
      elt$warning.msg <- paste(warning.msg,collapse="#")
      elt$warning.desc <- paste(warning.desc,collapse="#")

      invisible(elt)
    })
    # Verifions les résultats ci-dessous
    #inconsistency.var, inconsistency.msg, inconsistency.desc
    # idlist <- names(result[unlist(lapply(result,function(elt) elt$is.inconsistency))])
    # length(idlist) # 29 questionnaires sur 87 avec des erreurs
    # varlist <- c("hhmid","is.inconsistency","inconsistency.var","inconsistency.msg","inconsistency.desc")
    # result[[idlist[sample(1:length(idlist),1)]]][varlist]
    # 
    # # #warning.var,warning.msg,warning.desc
    # idlist.2 <- names(result[unlist(lapply(result,function(elt) elt$has.warning))])
    # length(idlist.2) # 14 questionnaires sur 87 avec des warning;
    # length(setdiff(idlist.2,idlist)) # seulement 5 peuvent être vraiment considéré comme des warning : car les 9 autres ont également des incohérences
    # varlist.2 <- c("hhmid","has.warning","warning.var","warning.msg","warning.desc")
    # result[[idlist.2[sample(1:length(idlist.2),1)]]][varlist.2]
    
    result <- lapply(result,function(elt,check.date=as.numeric(format(Sys.time(),"%Y%m%d")),
                                     check.time=as.numeric(format(Sys.time(),"%H%M"))){
      
      elt$check.date <- check.date # Jour de la verification
      elt$check.time <- check.time # Heure de la verification
      
      # Un questionnaire est considéré comme checked, si il n'a pas d'incohérence et est finalizé
      elt$is.checked <- elt$inconsistency.onDeclaration || (!elt$is.inconsistency && !elt$has.warning && elt$interview.isFinalized)
      
      # Tout ce qui suit est géré par la fonction release
      # Les variables to.trash et trash.infos seront principalement utilisées lors de la procédure
      # d'appurement : les données dans la corbeille engloble tous les données supprimés de 
      # de la base triplette : trash est un data.frame et chaque élt envoyé dans la corbeille
      # aura u identifiant unique : checker : contiendra la même taille qu'eligibilité
      # dbTriplette sera inférieur car ne conservera que les informations valides à la fin de la collecte
      # to.trash : Indique les données à envoyer dans la corbeille, car
      # definitivement incohérent ou inexploitable
      # elt$to.trash <- FALSE
      # # Raison de la suppresion d'une donnée
      # elt$trash.infos <- "" 
        
      invisible(elt)
    })
    
    # tmp <- names(result[unlist(lapply(result,function(elt) elt$is.checked))])
    # tmp <- names(result[unlist(lapply(result,function(elt) !elt$is.inconsistency & !elt$has.warning))])
    
    # Mise à jour de la base checkerTriplette
    # 1/ ajout de ceux n'existant pas
    # tmp <- list(a=list(e1=2,e2="lettrea",e3=TRUE),b=list(e1=1,e2="lettreb",e3=FALSE))
    # subtmp <-list(c=list(e1=3,e2="lettrec",e3=TRUE))
    # tmp["c"] <- subtmp # ou tmp <- c(tmp,subtmp)
    # subtmp <-list(a=list(e1=0,e2="lettred",e3=FALSE))
    # tmp["a"] <- subtmp
    
    if(!is.null(get("checkerTriplette",envir = .GlobalEnv))){
      # Sauvegarde de la base avant tout éventuelle modification
      filename <- sprintf("./outputs/checkerTriplette_%s.xlsx",
                          format(Sys.time(),"%Y%m%d_%H%M"))
      wb <- createWorkbook("CheckingSystem")
      addWorksheet(wb,"checkerTriplette")
      db <- get("checkerTriplette",envir = .GlobalEnv)
      writeDataTable(wb,1,bind_rows(db))
      #writeDataTable(wb,1,bind_rows(result))
      saveWorkbook(wb,filename,TRUE)
      cat("[>] check.Triplette : ",filename," recorded \n")
      
      #Modification de la base de données d'évaluation
      # result <- c(db[unlist(lapply(db,function(elt){elt$is.checked | !elt$is.inconsistency}))],
      #             result)
      result <- c(db[unlist(lapply(db,function(elt){!elt$is.inconsistency}))],result)
    }
    
    # Sauvegarde de la nouvelle base de vérification des données
    assign("checkerTriplette",result,envir = .GlobalEnv)
    
    #conversion de la base en data.frame
    # result <- bind_rows(result)
    # ceux-correspond également à ceux-ci
    # result <- do.call(rbind,result) %>% as.data.frame
  }
  
  cat("[>] check.Triplette : ending...\n")
  invisible(result)
  # invisible(bind_rows(result)) 
  # la conversion sera utilisée uniquement pour enregistré la base dans le fichier excel
}

#Affecte des valeurs aux informations de vérification d'un ou de plusieurs identifiants
# values = list(varname.1=varvalue.1,varname.2=varvalue.2 ...)
# Pour le test
# result <- checkerTriplette
# idlist <- names(result[sample(1:length(result),3)])
# lapply(idlist, function(id) result[[id]][c("hhmid","is.checked","check.date")] )
# values <- list(is.checked=TRUE,check.date=20180122)
inconsistency.setValues <- function(idlist,values=list()){
  if(length(idlist) && length(values)){
    
    db <- get("checkerTriplette",envir = .GlobalEnv)
    # if(!is.null(db)){
    #   # Sauvegarde de la base avant tout éventuelle modification
    #   filename <- sprintf("./outputs/checkerTriplette_%s.xlsx",
    #                       format(Sys.time(),"%Y%m%d_%H%M"))
    #   wb <- createWorkbook("CheckingSystem")
    #   addWorksheet(wb,"checkerTriplette")
    #   writeDataTable(wb,1,bind_rows(db))
    #   saveWorkbook(wb,filename,TRUE)
    #   cat("inconsistency.setValues : ",filename," recorded \n")
    # }
    
    idwork <- idlist[which(idlist %in% names(db))]
    
    if(length(setdiff(idlist,idwork))){
      cat("[>] Les identifiants suivants ne figurent pas dans la base checkerTriplette : ",
          paste(setdiff(idlist,idwork),collapse = ", "),"\n")
    }
    
    varwork <- names(values)[which(names(values) %in% names(db[[1]]))]
    
    if(length(setdiff(names(values),varwork))){
      cat("[>] Les variables suivantes ne figurent pas dans la base checkerTriplette : ",
          paste(setdiff(names(values),varwork),collapse = ", "),"\n")
    }
    
    if(length(idwork) && length(varwork)){
     
      db <- lapply(db,function(elt,idSelected=idwork){
        if (elt$hhmid %in% idSelected)
          for (varname in varwork) elt[varname] <- values[varname]
        invisible(elt)  
      })
      
      # Sauvegarde de la nouvelle base de vérification des données
      #checkerTriplette
      assign("checkerTriplette",db,envir = .GlobalEnv)     
    }
  }
}

# Affecte pour une liste d'identifiants les valeurs suivantes
# is.checked=TRUE, is.inconsistency=FALSE & has.warning=FALSE
# On utilisera la fonction : inconsistency.setValues
release.inconsistency <- function(idlist){

  inconsistency.setValues(idlist,values = list(is.checked=TRUE,
                                               is.inconsistency=FALSE,
                                               has.warning=FALSE,
                                               check.date=as.numeric(format(Sys.time(),"%Y%m%d")),
                                               check.time=as.numeric(format(Sys.time(),"%H%M"))))
}

# Affecte pour une liste d'identifiants les valeurs suivantes
# > is.checked=FALSE
# > is.inconsistency=length(inconsistency.var)>0 
# > has.warning=length(warning.var)>0 
# On utilisera la fonction : inconsistency.setValues
restore.inconsistency <- function(idlist){
  if(length(idlist)){
    db <- get("checkerTriplette",envir = .GlobalEnv)
    idwork <- idlist[which(idlist %in% names(db))]
    if(length(setdiff(idlist,idwork))){
      cat("[>] Les identifiants suivants ne figurent pas dans la base checkerTriplette : ",
          paste(setdiff(idlist,idwork),collapse = ", "),"\n")
    }
    
    if(length(idwork)){
      db <- lapply(db,function(elt,idSelected=idwork){
        if (elt$hhmid %in% idSelected){
          elt$is.inconsistency <- length(elt$inconsistency.var)>0
          elt$has.warning <- length(elt$warning.var)>0
          elt$is.checked <- elt$inconsistency.onDeclaration || (!elt$is.inconsistency && !elt$has.warning && elt$interview.isFinalized)
          elt$check.date <- as.numeric(format(Sys.time(),"%Y%m%d"))
          elt$check.time <- as.numeric(format(Sys.time(),"%H%M"))
        }
        invisible(elt)  
      })
      # Sauvegarde de la nouvelle base de vérification des données
      #checkerTriplette
      assign("checkerTriplette",db,envir = .GlobalEnv)     
    }
  }
}

# Ajout manuel d'une observation d'identifiant dans la base des triplettes
add.triplette <- function(caseid,new.codeSujet=NA,new.idPh="",new.codePh=NA){
  if(!is.null(caseid)){
    caseid <- head(caseid,1L)
    cat("[>] add.triplette : starting for",caseid,"\n")
    dbtmp <- household.membres %>% 
      filter(hhmid %in% caseid) %>% ungroup() %>%
      select(one_of(triplette.varlist)) %>%
      mutate(categorie=if_else(as.numeric(eligible) %in% c(3,4),"PH","PT"),
             codeSujet=new.codeSujet,statut=3, # En cours
             statut_lab=triplette.statut[statut],
             # factor(triplette.statut[statut],levels = triplette.statut,ordered = F),
             idPh=new.idPh, codePh=new.codePh,
             today=strtoi(format(Sys.Date(),"%Y%m%d")),
             time=format(Sys.time(),"%H%M"))
    dbtmp$q110 <- str_trim(dbtmp$q110)
    
    # Mise à jour des données si l'identifiant se trouve dans la base de données élibilité
    if(!is.null(get("eligibilite",envir = .GlobalEnv))){
        infos <- eligibilite %>% filter(elig_hhmid==caseid)
        if(dim(infos)[1]){
         dbtmp$categorie <- as.character(infos$statut_handicap)
         dbtmp$statut <- as.numeric(infos$statut_denombrement)
         dbtmp$statut_lab <- triplette.statut[dbtmp$statut]
         dbtmp$codeSujet <- infos$idlab01
         if(dbtmp$categorie=="PT" & !is.na(infos$id02)){
           dbtmp$idPh <- infos$caseid02
           dbtmp$codePh <- infos$idlab02
         }
         dbtmp$q115b <- infos$h001
         dbtmp$q116 <- infos$elig_age
        }
        
        # Mise à jour du sexe si l'identifiant se trouve dans la base de données handicap
        if(!is.null(get("handicap",envir = .GlobalEnv))){
          infos <- handicap %>% filter(h_hhmid==caseid)
          if(dim(infos)[1] && !is.na(infos$h101)){
            dbtmp$q112 <- ifelse(infos$h101 %in% "Femme","Féminin","Masculin")
          }
        }
    }
    
    if(!is.null(get("dbTriplette",envir = .GlobalEnv))){
      # Sauvegarde de la base avant tout éventuelle modification
      cat("[>] add.triplette : sauvegarde de la base dbTriplette avant modification..\n")
      filename <- sprintf("./outputs/dbTriplette_%s.xlsx",
                          format(Sys.time(),"%Y%m%d_%H%M"))
      wb <- createWorkbook("DBTriplette")
      addWorksheet(wb,"Triplettes")
      writeDataTable(wb,1,get("dbTriplette",envir = .GlobalEnv))
      saveWorkbook(wb,filename,TRUE)
      cat(filename," recorded \n")
    }
    # Ajout de l'observation dans la base de données
    cat("[>] add.triplette : ajout des nouvelles triplettes\n")
    if(is.null(get("dbTriplette",envir = .GlobalEnv))){
      dbTriplette <- dbtmp
    }else{
      # Suppression des identifiants existants déjà dans la base
      database <- get("dbTriplette",envir = .GlobalEnv) %>%
        filter(!(hhmid %in% as.character(dbtmp$hhmid)))
      dbTriplette <- rbind(database,dbtmp)
    }
    dbTriplette <- dbTriplette %>% arrange(today,categorie,idPh,hhmid)
    cat("[>] add.triplette : ajout de l'observation à la base dbTriplette\n")
    assign("dbTriplette",dbTriplette,envir = .GlobalEnv)
    cat("[>] add.triplette : ending\n")
  }
}

# Mise à jour de la base des triplettes avec les informations disponibles sur la
# base éligibilité # Pas une bonne idée la partie tirage doit rester indépendant
# de celle effectivement enquêté
update.triplette <- function(idlist=NULL){
  
  cat("[>] update.triplette : starting \n")
  
  if(!is.null(get("dbTriplette",envir = .GlobalEnv)) & 
     !is.null(get("eligibilite",envir = .GlobalEnv))){
    
    if(is.null(idlist)) idlist <- unique(as.character(dbTriplette$hhmid))
    
    dbtmp <- dbTriplette
    dbtmp$q110  <-str_trim(dbtmp$q110)
      
    for(caseid in idlist){
      cat("[>] >> ",caseid," starting")
      rowid <- which(dbtmp$hhmid==caseid)
      infos <- eligibilite %>% filter(elig_hhmid==caseid)
      if(dim(infos)[1]){
        if(all(!is.na(infos$statut_handicap)))
          dbtmp$categorie[rowid] <- as.character(infos$statut_handicap)
        if(all(!is.na(infos$statut_denombrement)))
          dbtmp$statut[rowid] <- as.numeric(infos$statut_denombrement)
        if(all(!is.na(dbtmp$statut[rowid])))
          dbtmp$statut_lab[rowid] <- triplette.statut[dbtmp$statut[rowid]]
        if(all(!is.na(infos$idlab01)))
          dbtmp$codeSujet[rowid] <- infos$idlab01
        if(!is.na(dbtmp$categorie[rowid]) && dbtmp$categorie[rowid]=="PT" && !is.na(infos$id02)){
          if(all(!is.na(infos$caseid02))) dbtmp$idPh[rowid] <- infos$caseid02
          if(all(!is.na(infos$idlab02))) dbtmp$codePh[rowid] <- infos$idlab02
        }
        if(all(!is.na(infos$h001))) dbtmp$q115b[rowid] <- infos$h001
        if(all(!is.na(infos$elig_age))) dbtmp$q116[rowid] <- infos$elig_age
      }
      
      # Mise à jour du sexe si l'identifiant se trouve dans la base de données handicap
      if(!is.null(get("handicap",envir = .GlobalEnv))){
        infos <- handicap %>% filter(h_hhmid==caseid)
        if(dim(infos)[1] && !is.na(infos$h101)){
          dbtmp$q112[rowid] <- ifelse(infos$h101 %in% "Femme","Féminin","Masculin")
        }
      }
      cat(" : treated.\n")
    }
    
    if(!is.null(get("dbTriplette",envir = .GlobalEnv))){
      # Sauvegarde de la base avant tout éventuelle modification
      cat("[>] update.triplette : sauvegarde de la base dbTriplette avant modification..\n")
      filename <- sprintf("./outputs/dbTriplette_%s.xlsx",
                          format(Sys.time(),"%Y%m%d_%H%M"))
      wb <- createWorkbook("DBTriplette")
      addWorksheet(wb,"Triplettes")
      writeDataTable(wb,1,get("dbTriplette",envir = .GlobalEnv))
      saveWorkbook(wb,filename,TRUE)
      cat(filename," recorded \n")
    }
    
    cat("[>] update.triplette : mises à jour de la base de données des triplettes\n")
    assign("dbTriplette",dbtmp,envir = .GlobalEnv)
    
  }else{
    cat("[>] update.triplette : base de données (dbTriplette ou eligibilite) inacessible pour l'execution de la fonction\n")
  }
  cat("[>] update.triplette : ending\n")
}

# Transforme pour un ensemble d'identifiants et de variable les informations de vérification 
# en chaîne de caractére. Quelque soit la liste des variables on ajoutera toujours par défaut
# hhmid (identifiant) et codeSujet et nomAgent
infos.toString <- function(dbase=NULL,idlist=NULL,varlist=infos.varlist){

  if(is.null(dbase)) dbase <- get("checkerTriplette",envir = .GlobalEnv)
  
  if(!is.null(idlist)) dbase <- dbase[unlist(lapply(dbase,function(elt,idSelected=idlist){ 
    elt$hhmid %in% idSelected }))] 

  if(is.null(varlist)) varlist <- names(dbase[[1]])
  varlist <- varlist[!str_detect(varlist,".msg|.desc|.var")]
  
  result <- lapply(dbase,function(elt){
    varlist.2 <- varlist
    
    if(elt$is.inconsistency){
      varlist.2 <- setdiff(varlist.2,str_split(elt$inconsistency.var,"#"))
      varlist.2 <- unique(c(varlist.2,"is.inconsistency"))
    }else{
      varlist.2 <- setdiff(varlist.2,
                           c("is.consistency","inconsistency.msg", "inconsistency.desc",
                             "inconsistency.var"))
    }
    
    if(elt$has.warning){
      varlist.2 <- setdiff(varlist.2,str_split(elt$warning.var,"#"))
      varlist.2 <- unique(c(varlist.2,"has.warning"))
    }else{
      varlist.2 <- setdiff(varlist.2,
                           c("has.warning","warning.msg", "warning.desc","warning.var"))
    }
    
    values <- unlist(lapply(setdiff(varlist.2,"hhmid"),function(varname,subelt=elt){
      tmp <- NULL;
      if(sprintf("%s.msg",varname) %in% names(subelt)){
        if(subelt[[varname]]){
          tmp <- sprintf("[>] %s : %s > %s",
                         varname,
                         subelt[[sprintf("%s.msg",varname)]],
                         subelt[[sprintf("%s.desc",varname)]])
          tmp <- paste(tmp,sep="\n",collapse = "\n")
        }
      }else{
        if(varname=="is.inconsistency" && subelt[[varname]] ){
          tmp <- sprintf("[>] %s : %s > %s",
                         unlist(str_split(subelt$inconsistency.var,"#")),
                         unlist(str_split(subelt$inconsistency.msg,"#")),
                         unlist(str_split(subelt$inconsistency.desc,"#")))
          tmp <- paste(c("\n*** [Incohérences] ***",tmp),sep="\n",collapse = "\n")
        }
        
        if(varname=="has.warning" && subelt[[varname]]){
          tmp <- sprintf("[>] %s : %s > %s",
                         unlist(str_split(subelt$warning.var,"#")),
                         unlist(str_split(subelt$warning.msg,"#")),
                         unlist(str_split(subelt$warning.desc,"#")))
          tmp <- paste(c("\n*** [Warning] ***",tmp),sep="\n",collapse = "\n")
        }
        
        if(!(varname %in% c("is.inconsistency","has.warning"))) {
          tmp <- sprintf("[>] %s : %s",varname,paste(subelt[[varname]],sep=", ",collapse = ", "))
        }
        
      }
      invisible(tmp)
    }))
    values <- c(sprintf("\nIdentifiant : %s\n",elt$hhmid),values)
    invisible(paste(values,sep="\n",collapse = "\n"))
  })
  
  result.toString <- unlist(result)
  return(paste(result.toString,
               sep="\n\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n",
               collapse = "\n\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"))
}
# Test de la fonction ci-dessus
# idlist <- names(checkerTriplette[unlist(checkerTriplette,function(elt) 
#   elt$is.inconsistency || elt$has.warning)])
# cat(infos.toString(idlist =idlist[sample(1:length(idlist),2)]))

# La fonction recherche et affiche les informations pour un PH,un PT ou un menage
# On peut rechercher les informations sur un identifiant complet ou sur des parties
# des identifians. Les informations sont ceux présentent dans la base
# checkerTriplette. On utilisera la fonction : infos.toString
find.infos <- function(communeList=NULL,zoneList=NULL,collineList=NULL,zdList=NULL,
                       num.structList=NULL,num.menageList=NULL,num.individuList=NULL,
                       agentList=NULL,hhmidList=NULL,any.inconsistency = FALSE,
                       with.inconsistency=FALSE,with.warning=FALSE,is.unknownPerson=FALSE,
                       is.nochecked=FALSE,varnames=infos.varlist,toString=TRUE,
                       startDate=NULL,endDate=NULL,verbatim=TRUE,only.administeredForm=FALSE){
  
  dbase <- get("checkerTriplette",envir = .GlobalEnv)
  
  if(only.administeredForm)  dbase <- dbase[unlist(lapply(dbase,function(elt){
   !elt$interview.wasRefused & !elt$is.unaivailable
  }))]
  
  if(!is.null(communeList)) dbase <- dbase[unlist(lapply(dbase,function(elt,varlist=communeList){ 
    elt$commune %in% varlist }))]
  
  if(!is.null(zoneList)) dbase <- dbase[unlist(lapply(dbase,function(elt,varlist=zoneList){ 
    elt$zone %in% varlist }))]
  
  if(!is.null(collineList)) dbase <- dbase[unlist(lapply(dbase,function(elt,varlist=collineList){ 
    elt$colline %in% varlist }))]
  
  if(!is.null(zdList)) dbase <- dbase[unlist(lapply(dbase,function(elt,varlist=zdList){ 
    elt$zd %in% varlist }))]
  
  if(!is.null(num.structList)) dbase <- dbase[unlist(lapply(dbase,function(elt,varlist=num.structList){ 
    elt$structure %in% varlist }))]
  
  if(!is.null(num.menageList)) dbase <- dbase[unlist(lapply(dbase,function(elt,varlist=num.menageList){ 
    elt$nummenage %in% varlist }))]
  
  if(!is.null(num.individuList)) dbase <- dbase[unlist(lapply(dbase,function(elt,varlist=num.individuList){ 
    elt$numindividu %in% varlist}))]
  
  if(!is.null(agentList)) dbase <- dbase[unlist(lapply(dbase,function(elt,varlist=agentList){ 
    elt$codeAgent %in% varlist}))]
  
  if(!is.null(hhmidList)) dbase <- dbase[unlist(lapply(dbase,function(elt,varlist=hhmidList){ 
    elt$hhmid %in% varlist}))]
  
  if(!is.null(hhmidList)) dbase <- dbase[unlist(lapply(dbase,function(elt,varlist=hhmidList){ 
    elt$hhmid %in% varlist}))]
  
  min.date <- startDate; max.date <- endDate;
  checkDate <- unlist(lapply(dbase,function(elt) elt$check.date))
  if(length(checkDate) & is.null(min.date)) min.date <- min(checkDate,na.rm =TRUE)
  if(length(checkDate) & is.null(max.date)) max.date <- max(checkDate,na.rm = TRUE)
  
  if(!is.null(min.date) && !is.na(min.date) && !is.null(max.date) && !is.na(max.date)) 
    dbase <- dbase[unlist(lapply(dbase,function(elt,minDate=min.date,maxDate=max.date){
      elt$check.date>=minDate & elt$check.date<=maxDate}))]
  
  if(any.inconsistency)
    dbase <- dbase[unlist(lapply(dbase,function(elt){elt$is.inconsistency || elt$has.warning}))]
  
  if(with.inconsistency)
    dbase <- dbase[unlist(lapply(dbase,function(elt){elt$is.inconsistency}))]
  
  if(with.warning)
    dbase <- dbase[unlist(lapply(dbase,function(elt){elt$has.warning}))]
  
  if(is.unknownPerson)
    dbase <- dbase[unlist(lapply(dbase,function(elt){elt$unknown.person}))]
  
  if(is.nochecked) dbase <- dbase[unlist(lapply(dbase,function(elt){!elt$is.checked}))]
  
  # varnames=infos.varlist,toString=TRUE,verbatim=TRUE
  result <- infos.toString(dbase=dbase,varlist=varnames)

  #si verbatim on affiche le contenu du resultat sur la console avant de retourner l'objet
  # ce contenu sera une seule chaîne de caractère
  if(verbatim) if(result!="" && length(result)) cat(result) else cat("-- Pas d'informations disponibles dans le module eligibilité et par conséquent dans checkerTriplette --")
  if(toString){ 
    invisible(result) 
  }else{
    dbase <- lapply(dbase,function(elt,varlist=varnames){
      # elt <- elt[varlist]
      invisible(elt[varlist])
    })
    invisible(bind_rows(dbase)) # invisible(dbase)
  } 
}

# Visualisation des données avec erreur de l'agent 57 qui n'ont pas encore été vérifié
# find.infos(agentList = 57,with.inconsistency = TRUE,is.nochecked = T)
# Pour avoir toute la base de données avec les incohérences non vérifiées et facilement
# manipulable :
# dbase <- find.infos(with.inconsistency = TRUE,is.nochecked = T,toString=F,verbatim=FALSE)
# dbase %>% mutate(total=n()) %>% group_by(nomAgent) %>% 
#   summarise(eff=n(),prop=round(100*eff/first(total),1)) %>%
#   arrange(desc(prop))
# # Ci-dessus on a la contribution des agents aux erreurs : sera utile

# Envoie dans la corbeille les identifiants en paramètre
# .. Les données sont supprimées des bases dbTriplette et checkerTriplette
# .. et insérées dans la base trashTriplette avec l'ajout de 5 variables supplémentaires :
# .. >trash.id  : indiquant le numéro d'ordre ou d'insertion dans la corbeille (numéro de ligne)
# .. >is.reusable : indiquant si l'on peut reutiliser l'identidant
# .. >trash.date : indiquant le jour d'insertion dans la corbeille
# .. >trash.time : indiquant l'heure de son insertion dans la corbeille
# .. >trash.infos : indiqant l'information accompagnant la suprresion de l'information 
# Lorsque l'identifiant est celui d'un PH les données de ces PT
# .. sont également surpprimé de la base. Avant de supprimer
# .. des identifiants : utiliser la fonction get
# release.Triplette <- function(dbRelease=list()) {
#   varlist <- c("hhmid","trash.id","is.reusable","trash.date","trash.time","trash.infos")
#   
#   if(length(dbRelease) && all(varlist %in% 
#                               str_replace_all(names(unlist(dbRelease[1])),"^([0-9]{16}.)",""))){
#     
#     db <- get("dbTriplette",envir = .GlobalEnv)
#     wdb  <- get("checkerTriplette",envir = .GlobalEnv)
#     trash <- get("trashTriplette",envir = .GlobalEnv)
#     num.onglet <- 1
#     # Sauvegarde de la base avant tout éventuelle modification
#     filename <- sprintf("./outputs/Handicap_%s.xlsx",
#                         format(Sys.time(),"%Y%m%d_%H%M"))
#     wb <- createWorkbook("Handicap")
#     if(!is.null(db)){
#       addWorksheet(wb,"Triplettes")
#       writeDataTable(wb,num.onglet,db)
#       num.onglet <- num.onglet + 1
#     }
#     
#     if(length(wdb)){
#       addWorksheet(wb,"Evaluation")
#       writeDataTable(wb,num.onglet,bind_rows(wdb))
#       num.onglet <- num.onglet + 1
#     }
#     
#     if(!is.null(trash)){
#       addWorksheet(wb,"Corbeille")
#       writeDataTable(wb,num.onglet,trash)
#     }
#     
#     saveWorkbook(wb,filename,TRUE)
#     cat("release.Triplette :: ",filename," recorded \n")
#     
#     idlist <- names(dbRelease)
#     
#     # Suppression des identifiants de la base des Triplettes
#     db.new <- db %>%  filter(!(hhmid %in% idlist) & !(idPh %in% idlist))
#     db.trash <- db %>%  filter((hhmid %in% idlist) | (idPh %in% idlist))
#     
#     # Suppression des identifiants de la base d'évaluation des triplettes
#     wdb.new <- wdb[unlist(lapply(wdb,function(elt,idSelected=idlist){
#       !(elt$hhmid %in% idSelected) & !(elt$caseid02 %in% idSelected)
#     }))]
#     wdb.trash <- bind_rows(wdb[unlist(lapply(wdb,function(elt,idSelected=idlist){
#       (elt$hhmid %in% idSelected) | (elt$caseid02 %in% idSelected)
#     }))])
#     
#     #find.infos(hhmidList = wdb.trash$hhmid)
#     
#     # Création de la base des nouveaux éléments à ajouter dans la corbeille
#     trash.new <- db.trash 
#     # if(dim(trash.new)[1]==0) trash.new <- wdb.trash else{
#     #   if(dim(wdb.trash)[1]>0)
#     #     trash.new <- db.trash %>% 
#     #       full_join(y=wdb.trash[,c("hhmid",setdiff(names(wdb.trash),names(db.trash)))],
#     #                 by=c("hhmid"="hhmid"))
#     # }
#     
#     init.id <- 1; end.id <- 0
#     if(dim(trash.new)[1]) end.id <- dim(trash.new)[1]
#       
#     if(!is.null(trash)){ 
#       init.id <- dim(trash)[1]+1
#       end.id <- end.id + dim(trash)[1]
#     }
#     
#     # des éléments à suppriner
#     trash.tosave <- bind_rows(dbRelease) %>%
#       full_join(y=trash.new,by=c("hhmid"="hhmid"))
#     
#     # Modification des informations relative à la corbeille : identifiants
#     # date et heure de la suppression des données
#     rowid <- which(is.na(trash.tosave$trash.id) | trash.tosave$trash.id==0)
#     trash.tosave$trash.id[rowid] <- c(init.id:end.id)
#     trash.tosave$trash.date[rowid] <- as.numeric(format(Sys.time(),"%Y%m%d"))
#     trash.tosave$trash.time[rowid] <- as.numeric(format(Sys.time(),"%H%M"))
#     
#     # Affectation des variables is.reusable=FALSE et trash.infos
#     trash.tosave$is.reusable <- FALSE
#     #trash.tosave$trash.infos <- "Tout élément envoyé à la corbeille est considérée comme non reutilisable"
#     
#     # Ajout de nouveaux éléments à la corbeille
#     trash <- rbind(trash,trash.tosave)
#     
#     # Enregystrement des données dans l'envirommement de travail
#     assign("dbTriplette",db.new,envir = .GlobalEnv)
#     assign("checkerTriplette",wdb.new,envir = .GlobalEnv)
#     assign("trashTriplette",trash.tosave,envir = .GlobalEnv)
#     cat("release.Triplette :: data recorded in workspace\n")
#   }else{
#     vartmp <- str_replace_all(names(unlist(dbRelease[1])),"^([0-9]{16}.)","")
#     varnames <- setdiff(varlist,vartmp)
#     cat("[>] La structure de la base dbRelease est incorrect. Les variables suivantes sont absentes :",
#         paste(varnames,sep = ", ",collapse = ", "),"\n")
#   }
# }

# # Test : choisions des une liste d'identifiants au harsard
# # identifiant uniquement dans dbTriplette :
# idlist.db <- (dbTriplette %>% 
#   filter(!(hhmid %in% eligibilite$elig_hhmid)))$hhmid
# 
# # identifiant uniquement dans éligibilité :
# idlist.elig <- (eligibilite %>% 
#                 filter(!(elig_hhmid %in% dbTriplette$hhmid)))$elig_hhmid
# 
# # identifiant uniquement dans éligibilité :
# # idlist.handicap <- (handicap %>% 
# #                       filter(!(h_hhmid %in% eligibilite$elig_hhmid)))$h_hhmid
# 
# # identifiant dans dbTriplette et éligibilité :
# idlist.1 <- (dbTriplette %>% 
#                filter((hhmid %in% dbTriplette$hhmid) & (hhmid %in% eligibilite$elig_hhmid)))$hhmid
# 
# # identifiant dans aucun des deux : 
# #household.membres
# idlist.2 <- (household.membres %>%
#                filter(!(hhmid %in% dbTriplette$hhmid) & (hhmid %in% eligibilite$elig_hhmid)))$hhmid
# 
# idlist.db <- setdiff(setdiff(setdiff(idlist.db,idlist.elig),idlist.1),idlist.2)
# idlist.elig <- setdiff(setdiff(setdiff(idlist.elig,idlist.db),idlist.1),idlist.2)
# idlist.1 <- setdiff(setdiff(setdiff(idlist.1,idlist.db),idlist.elig),idlist.2)
# idlist.2 <- setdiff(setdiff(setdiff(idlist.2,idlist.db),idlist.elig),idlist.1)
# 
# #Liste des identifiants
# idSelected <- c(idlist.db[sample(1:length(idlist.db),1)],
#                 idlist.elig[sample(1:length(idlist.elig),1)],
#                 idlist.1[sample(1:length(idlist.1),1)],
#                 idlist.2[sample(1:length(idlist.2),1)])
# idSelected <- intersect(idSelected,names(checkerTriplette))
# 
# dbase <- list()
# dbase <- lapply(idSelected,function(id){
#   #"hhmid","trash.id","is.reusable","trash.infos","trash.date","trash.time"
#   dbase$hhmid <- id
#   dbase$trash.id <- 0
#   dbase$is.reusable <- id %in% idSelected[1]
#   dbase$trash.infos <- sprintf("Suppression de la variable %s",id)
#   dbase$trash.date <- as.numeric(format(Sys.time(),"%Y%m%d"))
#   dbase$trash.time <- as.numeric(format(Sys.time(),"%H%M"))
#   invisible(dbase)
# })
# names(dbase) <- idSelected
# dbRelease <- dbase
# rm(idSelected,idlist.1,idlist.2,idlist.db,idlist.elig,dbase,dbRelease)

# La fonction supprime de la base des triplettes tous les PT avec un statut encours 
# .. dont les PH ont un statut apparié (Suppression principalement des PT de remplacement)
# .. Cette suppression permettra que ces PT puissent être réutilisé pour un autre tirage
# Elle permet également de mettre à jour le statut des triplettes conformément avec les 
# .. statut_denombrement et statut_handicap de la base eligibilite.
# .. Cette fonction est à utiliser avant la sélection de PH et de PT
# Cette fonction ne s'applique qu'aux informations déjà vérifié is.checked dans le 
# cas contraire aucun traitement automatique n'est appliqué : si l'on
# souhaite faire un traitement automatique : on affecte manuellement la valeur
# is.checked=TRUE aux identifiants concernés.
# cleaning.Triplette <- function() {
#   cat("[>] cleaning.Triplette : started .. \n")
#   if(exists("checkerTriplette",envir = .GlobalEnv) && 
#      !is.null(get("checkerTriplette",envir = .GlobalEnv))){
#     
#       # Sauvegarde des bases avant tout éventuelles modifications
#       filename <- sprintf("./outputs/BeforeCleaning_%s.xlsx",
#                           format(Sys.time(),"%Y%m%d_%H%M"))
#       wb <- createWorkbook("Handicap")
#       addWorksheet(wb,"Triplette")
#       writeDataTable(wb,1,get("dbTriplette",envir = .GlobalEnv))
#       addWorksheet(wb,"Evaluation")
#       writeDataTable(wb,2,bind_rows(get("checkerTriplette",envir = .GlobalEnv)))
#       saveWorkbook(wb,filename,TRUE)
#       cat("cleaning.Triplette : ",filename," recorded \n")
#     
#     # Pour tous les champs on met à jour le statut d'enquête
#     idtmp <- (dbTriplette %>% filter(hhmid %in% eligibilite$elig_hhmid))$hhmid
#     # for(idvar in idtmp)
#     tmp <- lapply(idtmp, function(idvar){
#       rowid.1 <- which(dbTriplette$hhmid==idvar)
#       rowid.2 <- which(eligibilite$elig_hhmid==idvar)
#       statut <- as.numeric(eligibilite$statut_denombrement[rowid.2])
#       # if(length(statut)!=length(rowid.2) | length(rowid.1)!=length(rowid.2) | 
#       #    length(rowid.1)>1 | length(rowid.2)>1 | length(statut)>1 )
#       #   cat("id=",idvar," dbTriplette$statut = ",dbTriplette$statut[rowid.1],
#       #       " eligibilite$statut_denombrement=",statut,"\n")
#       if(!is.na(statut) & dbTriplette$statut[rowid.1]!=statut){
#         # cat("Statut différe pour id=",idvar," dbTriplette = ",dbTriplette$statut[rowid.1],
#         #     "eligibilite",statut,"\n")
#         dbTriplette$statut[rowid.1] <- as.numeric(eligibilite$statut_denombrement[rowid.2])
#         dbTriplette$statut_lab[rowid.1] <- triplette.statut[dbTriplette$statut[rowid.1]]
#         #eligibilite$statut_denombrement[rowid.2]
#         assign("dbTriplette",dbTriplette,.GlobalEnv)
#       }
#     })
#     
#     db <- checkerTriplette[unlist(lapply(checkerTriplette,function(elt){
#       elt$is.inconsistency || elt$has.warning
#     }))]
#     
#     db.release <- list();
#     idlist <- NULL
#     reusable.list <- c("pt.sexInconsistencyWithPh","pt.birthDateInconsistencyWithPh",
#                        "pt.localisationInconsistencyWithPh","migrateToPT")
#     
#     # elt <- db[[names(db)[sample(1:length(db),1)]]]
#     
#     for(eltname in names(db)){
#       elt <- db[[eltname]]
#       infos.elig <- eligibilite %>% filter(elig_hhmid %in% elt$hhmid)
#       rowid <- which(dbTriplette$hhmid %in% elt$hhmid)
#       
#       w.varlist <- unlist(str_split(elt$warning.var,"#"))
#       names(w.varlist) <- w.varlist
#       w.msglist <- unlist(str_split(elt$warning.msg,"#"))
#       names(w.msglist) <- w.varlist
#       w.desclist <- unlist(str_split(elt$warning.desc,"#"))
#       names(w.desclist) <- w.varlist
#       
#       i.varlist <- unlist(str_split(elt$inconsistency.var,"#"))
#       names(i.varlist) <- i.varlist
#       i.msglist <- unlist(str_split(elt$inconsistency.msg,"#"))
#       names(i.msglist) <- i.varlist
#       i.desclist <- unlist(str_split(elt$inconsistency.desc,"#"))
#       names(i.desclist) <- i.varlist
#       i.vartmp <- w.vartmp <- NULL
#       
#       if(elt$pt.withoutPH){
#         idlist <- c(idlist,elt$hhmid)
#         idtmp <- setdiff(idtmp,idlist)
#         if(is.null(db.release[[elt$hhmid]])) db.release[[elt$hhmid]] <- list()
#         db.release[[c(elt$hhmid,"hhmid")]] <- elt$hhmid
#         db.release[[c(elt$hhmid,"trash.id")]] <- 0
#         db.release[[c(elt$hhmid,"is.reusable")]] <- FALSE
#         db.release[[c(elt$hhmid,"trash.infos")]] <- paste(c(db.release[[c(elt$hhmid,"trash.infos")]],
#                                                       "pt.withoutPH"),
#                                                       sep=", ",collapse=", ")
#         db.release[[c(elt$hhmid,"trash.date")]] <- as.numeric(format(Sys.time(),"%Y%m%d"))
#         db.release[[c(elt$hhmid,"trash.time")]] <- as.numeric(format(Sys.time(),"%H%M"))
#         i.vartmp <- c(i.vartmp,"pt.withoutPH")
#       }
#       
#       if(elt$is.matched){
#         idtmp <- setdiff((dbTriplette %>% filter(idPh %in% elt$hhmid))$hhmid,elt$caseid02)
#         idtmp <- setdiff(idtmp,idlist)
#         for(idsubelt in idtmp){
#           if(!(idsubelt %in% eligibilite$elig_hhmid) || !(checkerTriplette[[idsubelt]])$is.matched){
#             idlist <- c(idlist,idsubelt)
#             if(is.null(db.release[[idsubelt]])) db.release[[idsubelt]] <- list()
#             db.release[[c(idsubelt,"hhmid")]] <- idsubelt
#             db.release[[c(idsubelt,"trash.id")]] <- 0
#             db.release[[c(idsubelt,"is.reusable")]] <- !(idsubelt %in% eligibilite$elig_hhmid)
#             db.release[[c(idsubelt,"trash.infos")]] <- paste(c(db.release[[c(idsubelt,"trash.infos")]],
#                                                          "PH matched with another PT"),
#                                                          sep=", ",collapse=", ")
#             db.release[[c(idsubelt,"trash.date")]] <- as.numeric(format(Sys.time(),"%Y%m%d"))
#             db.release[[c(idsubelt,"trash.time")]] <- as.numeric(format(Sys.time(),"%H%M")) 
#           }
#         }
#       }
#       
#       if(elt$migrateToPT){
#         i.vartmp <- c(i.vartmp,"migrateToPT")
#         idtmp <- setdiff((dbTriplette %>% filter(idPh %in% elt$hhmid))$hhmid,elt$caseid02)
#         idtmp <- setdiff(idtmp,idlist)
#         for(idsubelt in idtmp){
#           if(!(idsubelt %in% eligibilite$elig_hhmid) || !(checkerTriplette[[idsubelt]])$is.matched){
#             idlist <- c(idlist,idsubelt)
#             if(is.null(db.release[[idsubelt]])) db.release[[idsubelt]] <- list()
#             db.release[[c(idsubelt,"hhmid")]] <- idsubelt
#             db.release[[c(idsubelt,"trash.id")]] <- 0
#             db.release[[c(idsubelt,"is.reusable")]] <- !(idsubelt %in% eligibilite$elig_hhmid)
#             db.release[[c(idsubelt,"trash.infos")]] <- paste(c(db.release[[c(idsubelt,"trash.infos")]],
#                                                          "PH matched with another PT"),
#                                                          sep=", ",collapse=", ")
#             db.release[[c(idsubelt,"trash.date")]] <- as.numeric(format(Sys.time(),"%Y%m%d"))
#             db.release[[c(idsubelt,"trash.time")]] <- as.numeric(format(Sys.time(),"%H%M")) 
#           }
#         }
#       }
#       
#       if(elt$inconsistency.onDeclaration){
#         idlist <- c(idlist,elt$hhmid)
#         i.vartmp <- c(i.vartmp,"inconsistency.onDeclaration")
#         idtmp <- setdiff((dbTriplette %>% filter(idPh %in% elt$hhmid))$hhmid,elt$caseid02)
#         idtmp <- setdiff(idtmp,idlist)
#         for(idsubelt in idtmp){
#           if(!(idsubelt %in% eligibilite$elig_hhmid) || !(checkerTriplette[[idsubelt]])$is.matched){
#             idlist <- c(idlist,idsubelt)
#             if(is.null(db.release[[idsubelt]])) db.release[[idsubelt]] <- list()
#             db.release[[c(idsubelt,"hhmid")]] <- idsubelt
#             db.release[[c(idsubelt,"trash.id")]] <- 0
#             db.release[[c(idsubelt,"is.reusable")]] <- FALSE
#             db.release[[c(idsubelt,"trash.infos")]] <- paste(c(db.release[[c(idsubelt,"trash.infos")]],
#                                                          "inconsistency.onDeclaration"),
#                                                          sep=", ",collapse=", ")
#             db.release[[c(idsubelt,"trash.date")]] <- as.numeric(format(Sys.time(),"%Y%m%d"))
#             db.release[[c(idsubelt,"trash.time")]] <- as.numeric(format(Sys.time(),"%H%M")) 
#           }
#         }
#       }
#       
#       if(!(elt$hhmid %in% idlist) & elt$is.checked){
#         if(elt$has.warning){
#           # Mise à jour des données des individus niveau dbtriplette depuis les données du module
#           # eligibilite
#           if(elt$sex.differentValue){
#             dbTriplette$q112[rowid] <- ifelse(dbTriplette$q112[rowid]=="Masculin","Féminin","Masculin")
#             w.vartmp <- c(w.vartmp,"sex.differentValue")
#             assign("dbTriplette",dbTriplette,.GlobalEnv)
#           }
#           
#           if(elt$birthdate.differentValue){
#             dbTriplette$q115b[rowid] <- infos.elig$h001
#             dbTriplette$q116[rowid] <- infos.elig$elig_age
#             w.vartmp <- c(w.vartmp,"birthdate.differentValue")
#             assign("dbTriplette",dbTriplette,.GlobalEnv)
#           }
#           
#           if(elt$changeCategorie){
#             dbTriplette$categorie[rowid] <- as.character(infos.elig$statut_handicap)
#             w.vartmp <- c(w.vartmp,"changeCategorie")
#             assign("dbTriplette",dbTriplette,.GlobalEnv)
#           }
#         }
#         
#         if(elt$is.inconsistency){
#           
#           vartmp <- c("pt.sexInconsistencyWithPh","pt.birthDateInconsistencyWithPh",
#                       "inconsistency.personAge","pt.localisationInconsistencyWithPh",
#                       "unknown.person")
#           lapply(vartmp, function(i.varname){
#             if(elt[[i.varname]]){
#               i.vartmp <- c(i.vartmp,i.varname)
#               idlist <- unique(c(idlist,elt$hhmid))
#             }
#             invisible()
#           })
#         }
#         
#         if(elt$is.ineligible){
#           # Pour tous les individus ayant un statut Inéligible est ayant
#           # d'éventuelles PT on les supprime de la base si pas apparié à d'autre
#             idtmp <- setdiff((dbTriplette %>% filter(idPh %in% elt$hhmid))$hhmid,elt$caseid02)
#             idtmp <- setdiff(idtmp,idlist)
#             lapply(idtmp,function(idsubelt){
#               if(!(idsubelt %in% eligibilite$elig_hhmid) || !(checkerTriplette[[idsubelt]])$is.matched){
#                 idlist <- c(idlist,idsubelt)
#                 if(is.null(db.release[[idsubelt]])) db.release[[idsubelt]] <- list()
#                 db.release[[c(idsubelt,"hhmid")]] <- idsubelt
#                 db.release[[c(idsubelt,"trash.id")]] <- 0
#                 db.release[[c(idsubelt,"is.reusable")]] <- !(idsubelt %in% eligibilite$elig_hhmid)
#                 db.release[[c(idsubelt,"trash.infos")]] <- paste(c(db.release[[c(idsubelt,"trash.infos")]],
#                                                              "PH matched with another PT"),
#                                                              sep=", ",collapse=", ")
#                 db.release[[c(idsubelt,"trash.date")]] <- as.numeric(format(Sys.time(),"%Y%m%d"))
#                 db.release[[c(idsubelt,"trash.time")]] <- as.numeric(format(Sys.time(),"%H%M")) 
#               }
#               invisible()
#             })
#         }
#       }
#       
#       lapply(c(w.vartmp,i.vartmp),function(varname){
#         elt[[varname]] <- FALSE
#         elt[[sprintf("%s.msg",varname)]] <- ""
#         elt[[sprintf("%s.desc",varname)]]  <- ""
#         # invisible(unlist(elt))
#       })
#       
#       if(length(w.vartmp)){
#         w.varlist <- setdiff(w.varlist,w.vartmp)
#         elt$has.warning <- length(w.varlist)>0
#         elt$warning.var <- paste(w.varlist,collapse="#")
#         elt$warning.msg <- paste(w.msglist[w.varlist],collapse="#")
#         elt$warning.desc <- paste(w.desclist[w.varlist],collapse="#")
#       }
#       
#       if(length(i.vartmp)){
#         i.varlist <- setdiff(i.varlist,i.vartmp)
#         elt$is.inconsistency <- length(i.varlist)>0
#         elt$inconsistency.var <- paste(i.varlist,collapse="#")
#         elt$inconsistency.msg <- paste(i.msglist[i.varlist],collapse="#")
#         elt$inconsistency.desc <- paste(i.desclist[i.varlist],collapse="#")  
#       }
#       
#       if(elt$hhmid %in% idlist){
#         idsubelt <- elt$hhmid
#         if(is.null(db.release[[idsubelt]])) db.release[[idsubelt]] <- list()
#         db.release[[c(idsubelt,"hhmid")]] <- idsubelt
#         db.release[[c(idsubelt,"trash.id")]] <- 0
#         #pour se simplifier la vie : tous les éléments avec incohérence(s) sont
#         #indiqués inexploitable... A la fin de l'enquête on recherchera ceux que
#         #l'on peut redresser...
#         db.release[[c(idsubelt,"is.reusable")]] <- FALSE #any(i.vartmp %in% reusable.list)
#         db.release[[c(idsubelt,"trash.infos")]] <- paste(db.release[c(idsubelt,"trash.infos")],
#                                                           i.vartmp,collape=", ",sep=", ")
#         db.release[[c(idsubelt,"trash.date")]] <- as.numeric(format(Sys.time(),"%Y%m%d"))
#         db.release[[c(idsubelt,"trash.time")]] <- as.numeric(format(Sys.time(),"%H%M"))
#       }
#       db[[eltname]] <-  elt
#     }
#     
#     # Mise à jour de la base de données des triplettes avant éventuelles suppression
#     tmp <- lapply(names(db),function(varname){
#       if(varname %in% names(checkerTriplette) & varname %in% names(db)){
#         elt.1 <- bind_rows(checkerTriplette[varname])
#         elt.2 <- bind_rows(db[varname])
#         if(any(elt.1!=elt.2,na.rm=TRUE)){
#           cat("[>] cleaning.Triplette : ",varname," cleaning > ")
#           checkerTriplette[varname] <- db[varname]
#           assign("checkerTriplette",checkerTriplette,.GlobalEnv)
#           cat("cleaned and saved.\n")
#         }else{
#           cat("[>] cleaning.Triplette : ",varname," en attente de corrections manuelles.\n")
#         }
#       }else{
#         cat("[>] cleaning.Triplette : ",varname," pas disponible dans checkerTriplette\n")
#       }
#       invisible()
#     })
#     
#     # Suppresion des données des bases Triplettes et autres
#     if(length(db.release)){
#       release.Triplette(db.release)
#       cat("[>] cleaning.Triplette : Triplette realesed > ", 
#           paste(names(db.release),sep = ", ",collapse = ", ")
#           ,"\n")
#     }else{
#       cat("[>] cleaning.Triplette :  Pas de triplette à supprimer\n")
#     }
#     
#     # Mise à jour de la corbeille pour certains identifiants 
#     idlist <- names(checkerTriplette[unlist(lapply(checkerTriplette,function(elt){
#       elt$is.unaivailable || elt$interview.wasRefused || elt$is.interviewed
#     }))])
#     if(!is.null(trashTriplette) & dim(trashTriplette)[1]){
#       idselect <- unique(intersect(unique(trashTriplette$hhmid),idlist))
#       if(length(idselect)){
#         cat("[>] cleaning.Triplette : mise à jour de is.reusable à FALSE pour > ",
#             paste(idselect,sep=", ",collapse = ", "),"\n")
#         rowid <- which(trashTriplette$hhmid %in% idselect)
#         trashTriplette$is.reusable[rowid] <- FALSE
#         assign("trashTriplette",trashTriplette,.GlobalEnv)
#         cat("[>] cleaning.Triplette : update trash completed")
#       }
#     }
#   }
#   cat("[>] cleaning.Triplette :  completed \n")
# }

# Sélection des PH à enquêter depuis la base screening
# Si zdList est précisé alors la selection des PH se fait uniquement dans les zd listées
# ..  Seules les PH identifiés dans les zd listés et non disponibles dans la bases des 
# .. triplettes sont retournées par la procédure
# Par défaut zdList concerne toutes les ZD disponibles dans la base screening
select.PH <- function(zdList=ifelse(exists("zd.finalized",where = .GlobalEnv),zd.finalized,NULL),
                      inclusion.PHQ129=FALSE){
  
  cat("[>] Sélection de PH","\n")
  cat("    ---------------","\n")
  
  if (is.null(zdList))
    zdList <- str_sort(unique(household.membres$zd),numeric = TRUE)
  
  # Liste des identifiants des menages effectivement inclus dans l'étude
  hhidTriplette <- NULL 
  # Liste des identifiants des individus effectivement inclus dans l'étude
  hhmidTriplette <- NULL
  
  if(!is.null(dbTriplette)){ # Test avec l'ajout inclusion.PHQ129=TRUE
    
    # Liste des identifiants ménages effectivement inclus dans l'étude
    hhidTriplette <- (dbTriplette %>% #4. Refus/Absent; 5. Non éligible
                        filter(!(statut %in% c(4,5))) %>% 
                        select(hhmid,hhid) %>% distinct())$hhid
    hhidTriplette <- str_sort(unique(hhidTriplette),numeric = TRUE)
    
    # Liste des identifiants des individus effectivement inclus dans l'étude
    hhmidTriplette <- str_sort(unique(dbTriplette$hhmid),numeric = TRUE)
    
    # # AJOUTER EGALEMENT LA LISTE DES MENAGES ET DES IDENTIFIANTS DES INDIVIDUS
    # # PRESENTS DANS LA CORBEILLE... DONT LES IDENTIFIANTS NE SONT PAS REUTILISABLE
    # # !is.reusable. VU QUE L'ON PEUT AVOIR PLUSIEURS FOIS LE MEME IDENTIFIANT dans la
    # # CORBEILLE. LES SEULES IDENTIFIANTS A INTEGRER SONT CEUX POUR QUI POUR TOUTES LES
    # # LIGNES, il n'existe pas une qui a is.reusable==TRUE donc on a all(!is.reusable) ou
    # # any(is.resusable)
    # if(!is.null(trashTriplette)){
    #   db.tmp <- trashTriplette %>%group_by(hhmid) %>%
    #     filter(all(!is.reusable)) %>% select(hhmid,hhid) %>% ungroup()
    #   if(dim(db.tmp)[1]){
    #     hhidTriplette <- str_sort(unique(c(hhidTriplette,db.tmp$hhid)),numeric = TRUE)
    #     hhmidTriplette <- str_sort(unique(c(hhmidTriplette,db.tmp$hhmid)),numeric = TRUE)
    #   }
    # }
    
    # Ajout de tous les identifiants de la base trashTriplette [Pour ne pas trop réfléchir]
    if(!is.null(trashTriplette)){
      hhidTriplette <- str_sort(unique(c(hhidTriplette,trashTriplette$hhid)),numeric = TRUE)
      hhmidTriplette <- str_sort(unique(c(hhmidTriplette,trashTriplette$hhmid)),numeric = TRUE)
    }
    
  } 
  
  # Selection des menages ayant des PH pas encore inclus dans l'étude
  # 3.Possible PH; 4. PH
  # list.inclusionCriteria <- c(4) ;
  # if(inclusion.PHQ129) list.inclusionCriteria <- c(list.inclusionCriteria,3)
  #  
  # hhidList <- str_sort(unique((household.membres %>% 
  #                                filter(as.numeric(eligible) %in% list.inclusionCriteria &
  #                                         !(hhid %in% hhidTriplette) &
  #                                         !(hhmid %in% hhmidTriplette) &
  #                                         zd %in% zdList) %>%
  #                                select(hhid,hhmid) %>% arrange(hhmid) %>% 
  #                                distinct())$hhid),numeric = TRUE)
  
  if(!inclusion.PHQ129){
    # Selection des menages ayant des PH pas encore inclus dans l'étude
    # 3.Possible PH; 4. PH
    hhidList <- str_sort(unique((household.membres %>%
                                   filter(as.numeric(eligible)==4 &
                                            !(hhid %in% hhidTriplette) &
                                            !(hhmid %in% hhmidTriplette) &
                                            zd %in% zdList) %>%
                                   select(hhid,hhmid) %>% arrange(hhmid) %>%
                                   distinct())$hhid),numeric = TRUE)
    
    ## Selection de la liste des PH pas encore inclus dans l'étude
    hhmidList <- (household.membres %>%
                    filter(hhid %in% hhidList & !(hhmid %in% hhmidTriplette) &
                             !(hhid %in% hhidTriplette) &
                             as.numeric(eligible)==4) %>%
                    group_by(hhid) %>% sample_n(size = 1) %>% select(hhid,hhmid) %>%
                    distinct())$hhmid
    
  }else{
    # Selection des menages ayant des PH pas encore inclus dans l'étude
    # 3.Possible PH; 4. PH
    hhidList <- str_sort(unique((household.membres %>%
                                   filter( (as.numeric(eligible)==4 |
                                              (as.numeric(eligible)==3 & 
                                                 !(q129 %in% c("","1","9",NA)))) &
                                             !(hhid %in% hhidTriplette) &
                                             !(hhmid %in% hhmidTriplette) &
                                             zd %in% zdList) %>%
                                   select(hhid,hhmid) %>% arrange(hhmid) %>%
                                   distinct())$hhid),numeric = TRUE)
    
    ## Selection de la liste des PH pas encore inclus dans l'étude
    hhmidList <- (household.membres %>%
                    filter(hhid %in% hhidList & !(hhmid %in% hhmidTriplette) &
                             !(hhid %in% hhidTriplette) &
                             (as.numeric(eligible)==4 |
                                (as.numeric(eligible)==3 & !(q129 %in% c("","1","9",NA))))) %>%
                    group_by(hhid) %>% sample_n(size = 1) %>% select(hhid,hhmid) %>%
                    distinct())$hhmid
  }
  
  # household.membres %>% filter(hhmid %in%hhmidList) %>% select(hhmid,q110,q116,eligible)
  ## Selection des PH âgés de 15-24 ans
  PHList01 <- (household.membres %>%
                 filter(hhmid %in% hhmidList & between(q116,15,24)))$hhmid
  # household.membres %>% filter(hhmid %in%PHList01) %>% select(hhmid,q110,q116,eligible)
    
  ## Selection des PH âgés de 25 ans et plus
  PHList02 <- (household.membres %>%
                 filter(hhmid %in% hhmidList & between(q116,25,49)))$hhmid
  # household.membres %>% filter(hhmid %in%PHList01) %>% select(hhmid,q110,q116,eligible)
  
  ## Liste definitive de la liste des PH
  PHList <- NULL
  if(length(PHList01)>0 && length(PHList02)>0){
    nbPH <- min(length(PHList01),length(PHList02))
    PHList <- c(sample(PHList01,size = nbPH,replace = FALSE),
                sample(PHList02,size = nbPH,replace = FALSE))
    PHList01 <- setdiff(PHList01,PHList)
    PHList02 <- setdiff(PHList02,PHList)
  }
  
  if(inclusion.PHQ129==FALSE && (length(PHList01)>0 || length(PHList02)>0)){
    # list.inclusionCriteria <- 3
    tmp.hhidList <- str_sort(unique((household.membres %>% 
                                       filter((as.numeric(eligible) ==4 |
                                                 (as.numeric(eligible)==3 & 
                                                    !(q129 %in% c("","1","9",NA)))) & 
                                            !(hhid %in% hhidTriplette) &
                                            !(hhmid %in% hhmidTriplette) &
                                            zd %in% zdList) %>%
                                   select(hhid,hhmid) %>% arrange(hhmid) %>% 
                                   distinct())$hhid),numeric = TRUE)
    tmp.hhmidList <- (household.membres %>%
                        filter(hhid %in% tmp.hhidList & !(hhmid %in% hhmidTriplette) &
                                 (as.numeric(eligible)==4 |
                                    (as.numeric(eligible)==3 & 
                                       !(q129 %in% c("","1","9",NA))))) %>%
                        group_by(hhid) %>% sample_n(size = 1) %>% select(hhid,hhmid) %>%
                        distinct())$hhmid
    # hhmidList <- c(hhmidList,tmp.hhmidList)
    # hhidList <- c(hhidList,tmp.hhidList)
    tmp.PHList01 <- (household.membres %>%
                   filter(hhmid %in% tmp.hhmidList & between(q116,15,24)))$hhmid
    tmp.PHList02 <- (household.membres %>%
                   filter(hhmid %in% tmp.hhmidList & between(q116,25,49)))$hhmid
    
    nb.missing <- abs(length(PHList01)-length(PHList02))
    assign.phList01 <- FALSE
    
    if(length(PHList01)==0){
      PHList01 <- tmp.PHList01
      assign.phList01 <- TRUE
    }else{
      if(length(PHList01)<length(PHList02)){
        PHList01 <- c(PHList01,
                      sample(tmp.PHList01,
                             size = min(length(tmp.PHList01),nb.missing),
                             replace = FALSE))
        assign.phList01 <- TRUE
      }
    }
    
    # nb.missing <- abs(length(PHList01)-length(PHList02))
    if(length(PHList02)==0){
      PHList02 <- tmp.PHList02
    }else{
      if(!assign.phList01 && length(PHList02)<length(PHList01))
        PHList02 <- c(PHList02,
                      sample(tmp.PHList02,
                             size = min(length(tmp.PHList02),nb.missing),
                             replace = FALSE))
    } 
    
    if(length(PHList01)>0 & length(PHList02)>0){
      nbPH <- min(length(PHList01),length(PHList02))
      PHList <- c(PHList,
                  sample(PHList01,size = nbPH,replace = FALSE),
                  sample(PHList02,size = nbPH,replace = FALSE))
    } 
  }
  
  if(is.null(PHList)){
    cat("[>] Pas de PH disponibles","\n")
  }else{
    print(PHList)
  }
  cat("------------------","\n")
  
  # Retour de la liste des identifiants des PH
  if(length(PHList)){
    rowid <- str_which(PHList,"^9")
    if(length(rowid)) PHList <- PHList[-rowid]
  }
  invisible(PHList)
}

# household.membres %>% filter(hhmid %in% select.PH()) %>% 
#   arrange(q116) %>%
#   select(hhmid,q110,q116,eligible)

# Selection pour chaque PH indiquée dans idPHList de nb.PT
#.. Si idPHList est null alors pour tous les PH présent dans la base
#.. des triplettes on associe nb.PT
select.PT <- function(idPHList=NULL,nb.PT=2){
  
  # Au moment du choix des PT si dans sa Zd si on a moins du nombre indiqué
  # en paramètre on retient le nombre disponible et automatiquement on tire
  # le reste dans les zd voisines sans préférence d'une en particulier
  
  if(is.null(idPHList) & is.null(dbTriplette)){
    cat("[>] La base des triplettes ou la liste des PH est vide","\n")
    cat("[>] Corriger erreur avant de poursuivre....","\n")
    invisible()
  } 
  
  cat("[>] Sélection de PT","\n")
  cat("    ---------------","\n")
  
  # Liste des identifiants ménages effectivement inclus dans l'étude
  hhidTriplette <- NULL
  # Liste des identifiants des individus effectivement inclus dans l'étude
  hhmidTriplette <- NULL
  
  if(!is.null(idPHList)){
    hhmidTriplette <- unique(idPHList)
    rowid <- which(household.membres$hhmid %in% hhmidTriplette)
    hhidTriplette <- as.character(unique(household.membres$hhid[rowid]))
  }
  
  if(!is.null(dbTriplette)){
    # Liste des identifiants ménages effectivement inclus dans l'étude
    tmp <- (dbTriplette %>% #4. Refus/Absent; 5. Non éligible
              filter(!(statut %in% c(4,5))) %>% 
              select(hhid,hhmid) %>% distinct())$hhid
    hhidTriplette <- str_sort(c(hhidTriplette,unique(as.character(tmp))),numeric = TRUE)
    # Liste des identifiants des individus effectivement inclus dans l'étude
    hhmidTriplette <- str_sort(c(hhmidTriplette,
                                 as.character(unique(dbTriplette$hhmid))),
                               numeric = TRUE)
    
    # AJOUTE DANS LES VARIABLES hhmidTriplette et hhidTriplette
    # LES INFORMATIONS EN PROVENANCE DE LA CORBEILLE trashTriplette
    # SI DISPONIBLE ET UNIQUE POUR LES IDENTIFIANTS AYANT LA PROPRIETE is.reusable=FALSE
    # vu que l'on peut avoir pour un même identifiant plusieurs lignes dans la corbeille
    # on utilisera all(!is.reusable) pour décider quels variables on ajoutera ou pas
    # dans les listes : on devra procéder d'abord à un groupby hhmid et déterminer la
    # valeur de is.reusable pour chaque hhmid contenu dans trashTriplette
    # if(!is.null(trashTriplette)){
    #   db.tmp <- trashTriplette %>%group_by(hhmid) %>% 
    #     filter(all(!is.reusable)) %>% select(hhmid,hhid) %>% ungroup()
    #   if(dim(db.tmp)[1]){
    #     hhidTriplette <- str_sort(unique(c(hhidTriplette,db.tmp$hhid)),numeric = TRUE)
    #     hhmidTriplette <- str_sort(unique(c(hhmidTriplette,db.tmp$hhmid)),numeric = TRUE)
    #   }
    # }
    
    # Pour faire simple, on supprime tous les identifiants de la corbeille comme
    # possible donnée à réutiliser
    if(!is.null(trashTriplette)){
      hhidTriplette <- str_sort(unique(c(hhidTriplette,trashTriplette$hhid)),numeric = TRUE)
      hhmidTriplette <- str_sort(unique(c(hhmidTriplette,trashTriplette$hhmid)),numeric = TRUE)
    }
  }
  
  idPTList <- list() 
  for(idPH in idPHList){
    
    ph <- household.membres %>% filter(hhmid==idPH)
    
    cat("[>] PT pour PH ",idPH,"\n")
    age.min <- 15; age.max <-49
    
    #si l'individu est déjà dans la base éligibilité
    #alors c'est l'âge et le sexe de la base éligibilité
    #que l'on utilise pour le tirage ...
    
    if(exists("eligibilite",.GlobalEnv) && dim(eligibilite)[1]){
      infos <- eligibilite %>% filter(elig_hhmid==idPH)
      if(dim(infos)[1]){
        ph$q115b <- infos$h001
        ph$q116 <- infos$elig_age
        if(exists("handicap") && dim(handicap)[1]){
          infos <- handicap %>% filter(h_hhmid==idPH)
          if(dim(infos)[1]) ph$q112 <- ifelse(infos$h101=="Femme","Féminin","Masculin")
        }
      }
    }else{
      if(!is.null(dbTriplette)){
        infos <- dbTriplette %>% filter(hhmid==idPH)
        ph$q115b <- infos$q115b
        ph$q116  <- infos$q116
        ph$q112  <- infos$q112
      }
    }
    
    if(all(!is.na(ph$q116))){
      # h001,elig_age
      if(ph$q116!=49 & ph$q116>=15) age.max <- ph$q116+5
      if(ph$q116!=15 & ph$q116<=49) age.min <- ph$q116-5;
      
      # Selection des menages ayant des PT (eligible==2) elegible pour le PH 
      # .. pas encore inclus dans l'étude
      PTList <- (household.membres %>% 
                   filter(as.numeric(eligible) ==2 & 
                            !(hhid %in% hhidTriplette) &
                            !(hhmid %in% hhmidTriplette)
                          & zd==ph$zd & q112==ph$q112 & 
                            between(q116,age.min,age.max)) %>%
                   group_by(hhid) %>% sample_n(size = 1) %>% 
                   select(hhmid,hhid) %>% distinct())$hhmid
      
      hhmidPTList <- NULL
      nbPT.dispo <- length(PTList)
      if(nbPT.dispo>=nb.PT){
        hhmidPTList <- sample(PTList,nb.PT,FALSE)
      }else{
        nbPT.missing <- nb.PT - nbPT.dispo
        if(nbPT.dispo>0){
          hhmidPTList <- sample(PTList,nbPT.dispo,FALSE)
          hhmidPTList <- str_sort(hhmidPTList,numeric = TRUE)
          hhidPTList <- (household.membres %>% filter(hhmid %in% hhmidPTList) %>%
                           arrange(hhmid) %>% select(hhid) %>% distinct())$hhid
          
          hhidTriplette <- c(hhidTriplette,hhidPTList)
          hhmidTriplette <- c(hhmidTriplette,hhmidPTList)
        }
        
        # ZD voisines à la zd du PH où des témoins peuvent être tirés
        zdList <- str_split(repartition.zd$zd.limit[strtoi(ph$zd)==repartition.zd$zd.num],
                            ";")[[1]]
        # On ne retient que les ZD qui sont clôturées
        if(exists("zd.finalized",where = .GlobalEnv)) zdList <- zdList[zdList %in% zd.finalized]
        
        if(length(zdList)){
          PTList <- (household.membres %>% 
                       filter(as.numeric(eligible) ==2 & 
                                !(hhid %in% hhidTriplette) &
                                !(hhmid %in% hhmidTriplette)
                              & (zd %in% zdList) & (q112==ph$q112) & 
                                between(q116,age.min,age.max)) %>%
                       group_by(hhid) %>% sample_n(size = 1) %>% 
                       select(hhmid,hhid) %>% distinct())$hhmid
          nbPT.dispo <- length(PTList)
          if(nbPT.dispo>0)
            hhmidPTList <- c(hhmidPTList,sample(PTList,
                                                min(nbPT.dispo,nbPT.missing,na.rm = TRUE),
                                                FALSE))
        }
      }
      
      hhmidPTList <- str_sort(hhmidPTList,numeric = TRUE)
      hhidPTList <- str_sort(unique((household.membres %>% 
                                       filter(hhmid %in% hhmidPTList) %>%
                                       arrange(hhmid) %>% select(hhid,hhmid) %>% 
                                       distinct())$hhid),
                             numeric = TRUE)
      
      nbPT <- length(hhmidTriplette)
      if(nbPT>0){
        hhidTriplette <- c(hhidTriplette,hhidPTList)
        hhmidTriplette <- c(hhmidTriplette,hhmidPTList)
        idPTList[[idPH]] <- hhmidPTList
        cat("[>] .. Liste des (",length(hhmidPTList),") PT :",hhmidPTList,"\n")
        # Marqué dans le checker que l'identifiant n'est plus reutilisable id.isReusable
        # qu'il a des PT
        if(!is.null(checkerTriplette) && idPH %in% names(checkerTriplette) && 
           (checkerTriplette[[idPH]])$ph.withoutPT){
          checkerTriplette[[idPH]]$ph.withoutPT <- FALSE
          checkerTriplette[[idPH]]$ph.withoutPT.msg <- ""
          checkerTriplette[[idPH]]$ph.withoutPT.desc <- ""
          checkerTriplette[[idPH]]$id.isReusable <- FALSE
          checkerTriplette[[idPH]]$id.isReusable.msg <- ""
          checkerTriplette[[idPH]]$id.isReusable.desc <- ""
          assign("checkerTriplette",checkerTriplette,.GlobalEnv)
        }
      }else{
        # idPTList[[idPH]] <- ""
        # Indiquer dans le checker que le PH n'a pas de PT
        # donc que sont id.isReusable pour une autre procédure
        if(!is.null(checkerTriplette) && idPH %in% names(checkerTriplette) && 
           !(checkerTriplette[[idPH]])$ph.withoutPT){
          checkerTriplette[[idPH]]$ph.withoutPT <- TRUE
          checkerTriplette[[idPH]]$ph.withoutPT.msg <- sprintf("ID-%s n'a pas de PT associées",idPH)
          checkerTriplette[[idPH]]$ph.withoutPT.desc <- "PH sans PT associées"
          checkerTriplette[[idPH]]$id.isReusable <- TRUE
          checkerTriplette[[idPH]]$id.isReusable.msg <- sprintf("ID-%s n'a pas de PT associées",idPH)
          checkerTriplette[[idPH]]$id.isReusable.desc <- "Identifiant PH réutilisable pour une prochain tirag"
          assign("checkerTriplette",checkerTriplette,.GlobalEnv)
        }
        cat("[>] .. Pas de PT pour PH-",idPH,"\n")
      }
    }else{
      cat("[>] .. Pas de PT pour PH-",idPH,"\n")
    }
   
  }
  
  if(length(idPTList)==0){
    cat("[>] Pas de PT pour la liste de PH : ",idPHList,"\n")
  }
  
  invisible(idPTList)
}

# household.membres %>% 
#   filter(hhmid %in% unlist(idPTList,use.names = F)) %>% 
#   arrange(q116) %>% mutate(name=str_trim(q110)) %>%
#   select(hhmid,name,q116,eligible)

# Ajoute à la base des triplettes tous les PH disponibles actuellement dans 
# .. la base screening et qui ne sont pas dans la base Triplette,
# .. et pour chaque nouveau PH lui associe un nombre de PT (nb.PT).
# Si zdList est précisé le tirage ne concerne que les zd listés, 
# .. sinon toutes les zd ayant des données sont concernées
# Si idPHList est précisé alors seules les PH se trouvant dans cette liste et 
# .. dans l'une des zd de zdList sont concernées par l'ajout de nouveaux PT dans la
# .. base des triplettes. 
# Si au contraire idPHList=NULL alors l'ajout concerne la selection de PH et de PT
set.Triplette<- function(zdList=ifelse(exists("zd.finalized",where = .GlobalEnv),zd.finalized,NULL),
                         inclusion.PHQ129=FALSE,idPHList=NULL,nb.PT=2,
                         check=TRUE,cleaning=check){
  
  cat("[>] set.triplette : starting...\n")
  
  #1- on appelle d'abord checkerTriplette : pour les vérification
  if(check) check.Triplette()
  
  #2- on appelle ensuite cleaningTriplette : pour la consolidation des bases 
  # if(cleaning) cleaning.Triplette()
   
  if(is.null(idPHList)) idPHList <- select.PH(zdList,inclusion.PHQ129)
  # if(is.null(zdList)) zdlist <- (household.membres %>% 
  #                                  filter(hhmid %in% idPHList) %>% 
  #                                  select(zd) %>% distinct())$zd
  if(is.null(idPHList) & is.null(dbTriplette))
    stop("set.Triplette:: Impossible de tirer des PT et de créer dbTriplette :: Pas de PH disponible")
  
  # #3- Ajouter à la liste des identifiants de PH : celles des PH n'ayant plus de témoins
  # # disponibles (aucun de ses témoins ne lui est apparié ou est encours)
  # # Ajout automatique # on le récupére du checkerTriplette.. je pense que l'on doit récupérer
  # # les PH dont l'identifiant est réutilisable : id.isReusable (uniquement vrai pour les PH sans PT)
  # idlist <- names(checkerTriplette[unlist(lapply(checkerTriplette, function(elt) 
  #   (elt$is.checked || !elt$is.inconsistency) & elt$is.ph & elt$id.isReusable))])
  # if(length(idlist)) idPHList <- unique(c(idPHList,idlist))
  # 
  # # De la liste des PH : on retire éventuellement tout ceux ayant une incohérence
  # # ou n'étant pas vérifié
  # idlist <- names(checkerTriplette[unlist(lapply(checkerTriplette, function(elt) 
  #   (!elt$is.checked || elt$is.inconsistency)))])
  # if(length(idlist)) idPHList <- setdiff(idPHList,idlist)
  
  PTList <- select.PT(idPHList,nb.PT)
  idPTList <- unlist(PTList,use.names = F)
  
  # Construction de la base temporaire des triplettes
  # cat("length(idPHList)=",length(idPHList),", length(idPTList)=",length(idPTList),"\n")
  cat("[>] set.triplette : construction de la base intermédiaire des triplettes...\n")
  if(length(idPHList)>0 | length(idPTList)>0){
    dbtmp <- household.membres %>% 
      filter(hhmid %in% c(idPHList,idPTList)) %>% ungroup() %>%
      select(one_of(triplette.varlist)) %>%
      mutate(categorie=if_else(as.numeric(eligible) %in% c(3,4),"PH","PT"),
             codeSujet=NA,statut=3, # En cours
             statut_lab=triplette.statut[statut],
             # factor(triplette.statut[statut],levels = triplette.statut,ordered = F),
             idPh="", codePh=NA,
             today=strtoi(format(Sys.Date(),"%Y%m%d")),
             time=format(Sys.time(),"%H%M"))
    dbtmp$q110 <- str_trim(dbtmp$q110)
    for(idPH in names(PTList)) dbtmp$idPh[dbtmp$hhmid %in% PTList[[idPH]]] <- idPH
    # cat("is.null(dbTriplette)=",is.null(dbTriplette),"\n")
    if(!is.null(get("dbTriplette",envir = .GlobalEnv))){
      # Sauvegarde de la base avant tout éventuelle modification
      cat("[>] set.triplette : sauvegarde de la base dbTriplette avant modification..\n")
      filename <- sprintf("./outputs/dbTriplette_%s.xlsx",
                          format(Sys.time(),"%Y%m%d_%H%M"))
      wb <- createWorkbook("DBTriplette")
      addWorksheet(wb,"Triplettes")
      writeDataTable(wb,1,get("dbTriplette",envir = .GlobalEnv))
      saveWorkbook(wb,filename,TRUE)
      cat(filename," recorded \n")
    }
    # Ajout des nouvelles des données dans la base de données
    cat("[>] set.triplette : ajout des nouvelles triplettes\n")
    if(is.null(get("dbTriplette",envir = .GlobalEnv))){
      dbTriplette <- dbtmp
    }else{
      dbTriplette <- rbind(get("dbTriplette",envir = .GlobalEnv),dbtmp)
    }
    dbTriplette <- dbTriplette %>% arrange(today,categorie,idPh,hhmid)
    cat("[>] set.triplette : sauvegarde de la nouvelle base dbTriplette\n")
    assign("dbTriplette",dbTriplette,envir = .GlobalEnv)
  }else{
    cat("set.Triplette:: Pas de nouveaux PH ou PT à ajouter")
  }
  cat("[>] set.triplette : completed..\n")
  invisible(get("dbTriplette",envir = .GlobalEnv))
}

# Retourne la base triplette si aucun des parametres n'est précisé
# Si zdlist est precisé, seules les triplettes de cette zd y figureront.
# .. par défaut si on retiendra toutes les zd listées dans la base triplette
# Si idPHList est précisé seules les triplettes des PH y listés qui sont localisées 
# .. dans l'une des zd de zdList seront listées. Par défaut on idPHList est la liste
# .. représente la liste de tous les PH disponibles dans la base des Triplettes
# Si startDate est précisé on affichera tous les triplettes identifées pour les
# .. dates de tirage >= startDate. Par défaut startDate est affectée à la date maximale
# .. disponible (date dernier tirage).
# Si endDate est précisé on affichera tous les triplettes indentifées pour les dates de
# .. tirage <= endDate. Par défaut endDate est affectée à la date la plus élevée de tirage
# .. disponible dans la base des triplettes (date dernier tirage).
get.Triplette <- function (zdList=NULL,idList=NULL,startDate=NULL,endDate=NULL,
                           startTime=NULL,endTime=NULL) {
  if(is.null(zdList))
    zdList <- str_sort(unique(dbTriplette$zd),numeric = TRUE)
  if(is.null(idList))
    idList <- str_sort(unique(dbTriplette$hhmid),numeric = TRUE)
  if(is.null(startDate)) startDate <- max(dbTriplette$today)
  if(is.null(endDate)) endDate <- max(dbTriplette$today)
  
  if(is.null(startTime)) startTime <- 0#max(dbTriplette$time)
  if(is.null(endTime)) endTime <- 2359#max(dbTriplette$time)
  
  result <- dbTriplette %>% 
    filter(zd %in% zdList & (hhmid %in% idList | idPh  %in% idList ) &
              between(today,startDate,endDate) & between(as.numeric(time),startTime,endTime))
  
  invisible(result)
}

# Informations sur le handicap produit dans un fichier Excel
# Sauvegarde # en plus de la base des triplettes on insére
# l'évaluation compléte des données sur le serveur : la base checkerTriplette
# et également la base trashTriplette..

# Produit la feuille excel à partager avec la coordination mais aussi à archiver
# L'onglet triplette : contiendra toujours toute la base des triplettes actuellement
# .. disponible (à jour de tous les états)
# Les onglets PH et PT contiennent l'extrait de la base des triplettes passée en paramétre
# .. Par défaut ce sera toutes triplettes du dernier tirage.
# L'onglet Evaluation : contient les informations de checkerTriplette qui se rapporte à toutes 
# .. les informations disponibles sur le serveur à date
# L'onglet corbeille : contient l'ensemble des informations que nous avons supprimées de la base
# .. de données ou du moins que nous considéreons inutilisable. Ces informations proviennent de
# .. la base sur le serveur et ne sont plus présente dans les bases triplettes et evaluation.
write.handicapExcel <- function(dbase=get.Triplette()) {
  
  filename <- "./outputs/Handicap.xlsx"
  idx.varname <- c(8:24)
  idx.varPT <- c(1:14,26:38,41:42)
  idx.varPT <- c(1:14,26:42)
  
  ## Creation d'un nouveau classeur
  wb <- createWorkbook("Handicap")
  
  addWorksheet(wb,"Triplette")
  db <- get("dbTriplette",envir = .GlobalEnv)
  names(db)[idx.varname] <- triplette.varname
  writeDataTable(wb,1,db)
  
  addWorksheet(wb,"Evaluation")
  db <- get("checkerTriplette",envir = .GlobalEnv)
  if(!is.null(db) && length(db)) writeDataTable(wb,2,bind_rows(db))
  
  addWorksheet(wb,"Corbeille")
  db <- get("trashTriplette",envir = .GlobalEnv)
  if(!is.null(db) && dim(db)[1]) writeDataTable(wb,3,db)
  
  db <- dbase
  names(db)[idx.varname] <- triplette.varname
  
  addWorksheet(wb,"PH")
  dbtmp <- db %>% filter(categorie=="PH") %>% ungroup()
  writeDataTable(wb,4,dbtmp)
  
  addWorksheet(wb,"PT")
  pt.varlist <- as.character(names(db)[idx.varPT])
  dbtmp <- db %>% filter(categorie=="PT") %>% 
    select(one_of(pt.varlist)) %>% ungroup()
  writeDataTable(wb,5,dbtmp)
  
  saveWorkbook(wb,filename,TRUE)
  cat("[>] write.handicapExcel : ",filename," [recorded]\n")
}

# Informations sur le handicap produit dans un document word
write.handicapWord <- function(agentList=NULL,filename=NULL) {
  
  if(is.null(agentList))
    agentList <- sort(unique(eligibilite$elig_agent))
  if(is.null(filename)) 
    filename <- "./outputs/handiSSR.docx"
  
  #.... A REDIGER ....
}

# Rapport d'évaluation pour chaque Agent
write.handicapByAgent <- function(agentList=NULL){
  if(is.null(agentList))
    agentList <- (household.habitat %>%
                    inner_join(y=staff,by=c("agent"="id")) %>% 
                    arrange(noms) %>% select(agent,noms) %>% distinct())$agent
  
  filepath <- "./outputs/handicap/"; graphes <- list()
  
  # ADAPTER LA LISTE DES AGENTS A CELLE DISPONIBLE DANS ELIGIBILITE
  # A L'IMMEDIAT ELLE SERT JUSTE A PRODUIRE LA LISTE DES ERREURS
  dbase <- find.infos(any.inconsistency = TRUE,is.nochecked = T,toString=F,verbatim=FALSE)
  agentList <- unique(dbase$codeAgent)
  
  for(codeAgent in agentList){
    #.... A COMPLETER ....
    nom_agent <- (staff %>% filter(id==codeAgent))$noms
    filename <- sprintf("%s/%s.docx",filepath,nom_agent)
    cat(sprintf("[>] %s (codeAgent=%d)\n",nom_agent,codeAgent))
    
    dateLab <- ftext(as.character(sprintf("Agent%02d : %s - %s",codeAgent,nom_agent,
                                          format(Sys.time(),"le, %d %B %Y à %H:%M"))),
                     prop=fp_text(bold = TRUE,font.size = 12))
    
    # S'ASSURER QUE CETTE CHAINE EST NON NULL AVANT DE L'UTILISER DANS
    # docAgent
    infos.inconsistencies <- find.infos(agentList = codeAgent,only.administeredForm=TRUE,
                                        with.inconsistency = TRUE,is.nochecked = T,
                                        toString=T,verbatim=FALSE)
    
    docAgent <- read_docx() %>% 
      body_add_img(src = logo, width = 2, height = 1,style = "centered",pos="on") %>%
      body_add_fpar(fpar(dateLab),style = "centered") %>%
      body_add_par(value = "Listing des incohérences", 
                   style = "heading 1")
      
    infos.values <- unlist(str_split(infos.inconsistencies,"\n"))
    for(i in 1:length(infos.values)) docAgent <- docAgent %>% 
      body_add_par(value=infos.values[i],style="Normal")
    
      #body_add_fpar(fpar(ftext(infos.inconsistencies,prop = fp_text()))) 
      docAgent %>% print(target = filename) %>% 
      invisible()  
  }
    
}

# Informations sur le handicap
write.handicapInfos <- function(agentList=NULL,output=c("all","xlsx","docx","agent"),
                                startDate.val=NULL,endDate.val=NULL){
  
  if(is.null(agentList))
    agentList <- sort(unique(eligibilite$elig_agent))
  
  if(is.null(output)|length(output)==0){
    output <- "all"
  }else{
    output <- output[1]
  }
  
  if(output %in% c("all","xlsx")) write.handicapExcel(get.Triplette(startDate=startDate.val,
                                                                    endDate=endDate.val))
  if(output %in% c("all","docx")) write.handicapWord(agentList)
  if(output %in% c("all","agent"))write.handicapByAgent(agentList)
  
  invisible()
}

save.image("HandiSSR.RData"); save.image()
