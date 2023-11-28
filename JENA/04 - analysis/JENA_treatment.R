# Script de telechargement des donnees completes depuis le serveur Kobo
# Nous ne retenons que les donnees non approuvées seeon


#*********************************************************************************
# Configuration de l'espace de travail
#*********************************************************************************
# Import toutes les données pour le traitement et l'analyse des données
# Nettoyage de l'espace de travail
rm(list=ls())

# Chargement des librairires utilitaires
pacman::p_load(openxlsx,robotoolbox,janitor,rio,labelled,tidyverse)

# Import des fonctions de traitement 
source("JENA_utilities.R")


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
  arrange(order_id) %>% # Suppression du formulaire de synthese des donnees a telecharger
  filter(!agent %in% c("coordo_supervisor")) %>% 
  mutate(variable=c("informateur","ecole","mepst","cluster","enseignant",
                    #es=enfant scolarise; ens=enfant non scolarise; 0609=6-9ans; 1316=13 a 16 abs
                    "es_0609","ens_0609","ens_1316","es_1316", 
                    "parent_es","parent_ens"))

# Objet regroupant tous les parametres d'interet pour le moniteur
param <- list(
  dbname = databases,
  survey_startdate = ymd("2023-05-30"),
  survey_enddate =  ymd("2023-05-30") + 21,
  pilote_startdate = ymd("2023-05-26"),
  pilote_enddate = ymd("2023-05-28"),
  validation_status = c("Aucun statut"="","En attente"="On Hold","Approuvé"="Approved","Non Approuvé"="Not Approved"),
  varname_todelete = c("deviceid","short_message_length",
                        "long_message_length","short_message",
                        "long_message","tmp","horodatage","survey_year",
                        "_uuid","rootUuid","deprecatedID","_status","_submitted_by"),
  STATUT_APPROVED="Approved",
  STATUT_NOT_APPROVED="Not Approved",
  STATUT_UNDEFINED="",
  STAUT_ON_HOLD ="On Hold"
)


#*********************************************************************************
# Telechargement de toutes les donnees a traiter
#*********************************************************************************
# .............................................................................
# ETAPE 01 : Téléchargement des données depuis le serveur
# .............................................................................
# # Le code ci-dessous est executee une seule fois pour telecharger les
# # depuis le serveur. Ceci afin de gagner en temps de telechargement
# # vu que la collecte de donnees est fini...
# databases <- list()
# for(i in 1:nrow(param$dbname)){
#   
#   #Identifiant du questionnaire a telecharger
#   formid <- param$dbname$uid[i]
#   #nom de variable associe a cette base de données
#   varname <- param$dbname$variable[i]
#   # Telechargement des donnees brutes que nous allons traiter  
#   databases[[varname]] <- kobo_data(kobo_asset(formid))
# }
# 
# # Nettoyage de l'espace de travail
# rm(i,varname,formid)
# 
# # sauvegarde des données brutes téléchargées
# rio::export(databases,"deliverable/databases.rds")

# .............................................................................
# ETAPE 02 : Import des donnees brutes precedement telecharger
# .............................................................................
databases <- import("deliverable/databases.rds") 

#*********************************************************************************
# Traitement des données MEPST [quali]
#*********************************************************************************
# Entretien quali avec les points focaux du ministere de l'education
database_name <- "mepst"
db_raw <- databases[[database_name]] # variable de stockage des donnees brutes


# dm::dm_gui(dm=db_raw) # Pour visualiser dynamiquement la base de donnees relationnelles

# db_raw %>% dm::dm_draw() # Visualisation statistique des relations
# db_raw %>% dm::dm_draw(view_type = "title_only")

#................................................................................
# # Si on souhaite tout fusionner dans une seule structure comme cela ete le cas
# # dans l'ancienne version de robotoolbox
# db_raw <- db_raw %>% dm::dm_wrap_tbl(root = main) %>% glimpse()
#................................................................................

db_clean <- list() # variable de stockage des resultats des traitements

# (1) Traitement de la principale structure
# glimpse(db_raw %>% dm::dm_zoom_to(main))
db <- db_raw$main %>% 
  select(-any_of(param$varname_todelete)) %>% 
  select(-c(enum_name,nom_agent,sprintf("attachment%d",1:3),responsabilites_count,
            q02a_count)) %>% 
  janitor::clean_names(case="none") %>% 
  rename(form_id=xform_id_string,comment=attachment0) %>% 
  mutate(
    start = lubridate::as_datetime(start),
    end = lubridate::as_datetime(end),
    today=lubridate::as_date(today),
    submission_time= lubridate::as_datetime(submission_time),
    validation_status=to_character(validation_status),
    poste_travail = str_to_upper(poste_travail),
    telephone = ifelse(telephone=='000','',telephone),
    enum_id = parse_number(enum_id),
    province1_lab = ifelse(province_1==1,"ITURI",''),
    province2_lab = ifelse(province_2==1,"KASAI-CENTRAL",''),
    province3_lab = ifelse(province_3==1,"KASAI-ORIENTAL",''),
    province4_lab = ifelse(province_4==1,"NORD-KIVU",''),
    province5_lab = ifelse(province_5==1,"SUD-KIVU",''),
    province6_lab = ifelse(province_6==1,"TANGANYIKA",''),
    province = str_trim(str_c(province1_lab,province2_lab,province3_lab,
                              province4_lab,province5_lab,province6_lab,
                              sep=" ",collapse = NULL))
  ) %>% 
  select(-any_of(sprintf("province%d_lab",1:6))) %>% 
  # On ne conserve que les données correspondant à la période d’enquête
  filter(start>=param$survey_startdate,end<=param$survey_enddate) %>% 
  # Correction de la structure des donnees afin de faciliter l'export des resultats
  correcting.dataStructure() %>% 
  set_variable_labels(
    start = "Date d'ouverture du questionnaire",
    end = "Date de clôture du questionnaire",
    today = "Jour de collecte",
    hhid = "Identifiant observation",
    comment = "Compte rendu de l'entretien",
    poste_travail = "Poste de travail",
    telephone = "Numéro de téléphone de la source d’informations",
    enum_id = "Identifiant agent enquêteur",
    form_id = "Identifiant formulaire",
    index = "Clé primaire de la table",
    id = "Numéro séquentielle",
    uuid="Numéro de série",
    submission_time="Date de soumission",
    validation_status="Statut de validation du questionnaire",
    version="Idenfiant de la version du formulaire",
    territoire = "Territoires couverts",
    province = "Provinces couvertes",
    province_1="Province couverte :: ITURI",
    province_2="Province couverte :: KASAI-CENTRAL",
    province_3="Province couverte :: KASAI-ORIENTAL",
    province_4="Province couverte :: NORD-KIVU",
    province_5="Province couverte :: SUD-KIVU",
    province_6="Province couverte :: TANGANYIKA"
  )
# Sauvegarde le traitement dans la base de resultat qui sera sexport
db_clean$main <- db 

# (2) Traitement en boucle des bases de donnees  issus des boucles repeat
# dput(names(db_raw)[-1])
# dbnames_list <- c(
#   "responsabilites", "opinons", "raisons", "interventions", 
#   "situation_enft_pdi", "mesures_enft_pdi", "obstacles", "besoins", 
#   "mesures_secu_enft", "besoins_iee", "defis_ministere", "formation_mepst", 
#   "mesures_elearning", "mesures_verification", "actions_mepst", 
#   "politique_pro_genre", "politique_handicap_genre"
# )

dbnames_list <- names(db_raw)[-1]
dbnames_params <- list(
  responsabilites=list(# glimpse(db_raw %>% dm::dm_zoom_to(responsabilites))
    label = "TACHES DU REPONDANT",
    new_name = "tache_repondant",#db_clean
    varnames_select = c("idx_q02a","q02a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q02a="Numéro sequentiel",
      q02a = "Tâche du répondant",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q02a = "{str_to_upper(q02a)}"
    )
  ),
  opinons=list(# glimpse(db_raw %>% dm::dm_zoom_to(opinons))
    label = "CATEGORIES D'ENFANTS NON SCOLARISES",
    new_name = "cat_enft_non_scolarise", #db_clean
    varnames_select = c("idx_q03a","q03a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q03a="Numéro sequentiel",
      q03a = "Catégorie d'enfants non scolarisés",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q03a = "{str_to_upper(q03a)}"
    )
  ),
  raisons=list(# glimpse(db_raw %>% dm::dm_zoom_to(raisons))
    label = "MOTIFS NON SCOLARISATION DES ENFANTS",
    new_name = "motif_non_scolarisation", #db_clean
    varnames_select = c("idx_q03b","q03b","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q03b="Numéro sequentiel",
      q03b = "Motif non scolarisation des enfants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q03b = "{str_to_upper(q03b)}"
    )
  ),
  interventions=list(# glimpse(db_raw %>% dm::dm_zoom_to(interventions))
    label = "INTERVENTIONS MEPST POUR LA PROMOTION DE LA SCOLARISATION DES ENFANTS AFFECTES PAR LES CRISES",
    new_name =  "intervention_mepst",#db_clean
    varnames_select = c("idx_q05a","q05a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q05a="Numéro sequentiel",
      q05a = "Intervention MEPST en faveur de l’accès à l’éducation",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q05a = "{str_to_upper(q05a)}"
    )
  ),
  situation_enft_pdi=list(# glimpse(db_raw %>% dm::dm_zoom_to(situation_enft_pdi))
    label = "DIFFICULTES DES ENFANTS DEPLACES INTERNES",
    new_name = "diff_enft_pdi", #db_clean
    varnames_select = c("idx_q09a","q09a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q09a="Numéro sequentiel",
      q09a = "Difficulté des enfants PDI",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q09a = "{str_to_upper(q09a)}"
    )
  ),
  mesures_enft_pdi=list(# glimpse(db_raw %>% dm::dm_zoom_to(mesures_enft_pdi))
    label = "MESURES MEPST EN FAVEUR DES ENFANTS DEPLACES INTERNES",
    new_name = "mesure_mepst_pdi", #db_clean
    varnames_select = c("idx_q10a","q10a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q10a="Numéro sequentiel",
      q10a = "Mesure MEPST en faveur les enfants PDI",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q10a = "{str_to_upper(q10a)}"
    )
  ),
  obstacles=list(# glimpse(db_raw %>% dm::dm_zoom_to(obstacles))
    label = "OBSTACLES EN MATIERE D'ACCES A L'EDUCATION",
    new_name = "obstacles", #db_clean
    varnames_select = c("idx_q11a","q11a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q11a="Numéro sequentiel",
      q11a = "Obstacle en matière d'accès à l'éducation",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q11a = "{str_to_upper(q11a)}"
    )
  ),
  besoins=list(# glimpse(db_raw %>% dm::dm_zoom_to(besoins))
    label = "BESOINS PRIORITAIRES POUR L'EDUCATION",
    new_name = "besoins_educ", #db_clean
    varnames_select = c("idx_q12a","q12a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q12a="Numéro sequentiel",
      q12a = "Besoin prioritaire en matière d'éducation",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q12a = "{str_to_upper(q12a)}"
    )
  ),
  mesures_secu_enft=list(# glimpse(db_raw %>% dm::dm_zoom_to(mesures_secu_enft))
    label = "MESURES MEPST POUR LA SECURITE DES ENFANTS DANS L'ENVIRONNEMENT SCOLAIRE",
    new_name = "mesure_mepst_secu", #db_clean
    varnames_select = c("idx_q13a","q13a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q13a="Numéro sequentiel",
      q13a = "Mesure MEPST pour sécurité des enfants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q13a = "{str_to_upper(q13a)}"
    )
  ),
  besoins_iee=list(# glimpse(db_raw %>% dm::dm_zoom_to(besoins_iee))
    label = "BESOINS EN INTERVENTIONS EiE (Education en situation d'urgence)",
    new_name = "besoins_iee", #db_clean
    varnames_select = c("idx_q14a","q14a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q14a="Numéro sequentiel",
      q14a = "Besoin en intervention EiE",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q14a = "{str_to_upper(q14a)}"
    )
  ),
  defis_ministere=list(# glimpse(db_raw %>% dm::dm_zoom_to(defis_ministere))
    label = "DIFFICULTES DU MINISTERE PAR RAPPORT AUX ENSEIGNANTS",
    new_name = "diff_ministere", #db_clean
    varnames_select = c("idx_q15a","q15a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q15a="Numéro sequentiel",
      q15a = "Difficulté du Ministère",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q15a = "{str_to_upper(q15a)}"
    )
  ),
  mesures_verification=list(# glimpse(db_raw %>% dm::dm_zoom_to(mesures_verification))
    label = "MESURES DE VERIFICATION LORS DU RECRUTEMENT DES ENSEIGNANTS",
    new_name = "mesures_verification", #db_clean
    varnames_select = c("idx_q17a","q17a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q17a="Numéro sequentiel",
      q17a = "Mesure de vérification avant recrutement des enseignants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q17a = "{str_to_upper(q17a)}"
    )
  ),
  formation_mepst=list(# glimpse(db_raw %>% dm::dm_zoom_to(formation_mepst))
    label = "FORMATIONS MEPST POUR ENSEIGNANT",
    new_name = "formation_mepst", #db_clean
    varnames_select = c("idx_q18a","q18a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q18a="Numéro sequentiel",
      q18a = "Formation MEPST pour enseignants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q18a = "{str_to_upper(q18a)}"
    )
  ),
  actions_mepst=list(# glimpse(db_raw %>% dm::dm_zoom_to(actions_mepst))
    label = "ACTION MEPST POUR ECOLES ET ENSEIGNANTS",
    new_name = "actions_mepst", #db_clean
    varnames_select = c("idx_q19a","q19a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q19a="Numéro sequentiel",
      q19a = "Action MEPST en faveur des ecoles et des enseignants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q19a = "{str_to_upper(q19a)}"
    )
  ),
  mesures_elearning=list(# glimpse(db_raw %>% dm::dm_zoom_to(mesures_elearning))
    label = "MESURES PRISES POUR ASSURER L'ENSEIGNEMENT A DISTANCE",
    new_name = "mesures_elearning", #db_clean
    varnames_select = c("idx_q20a","q20a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q20a="Numéro sequentiel",
      q20a = "Mesure prise pour assurer l'enseignement à distance",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q20a = "{str_to_upper(q20a)}"
    )
  ),
  politique_pro_genre=list(# glimpse(db_raw %>% dm::dm_zoom_to(politique_pro_genre))
    label = "MESURES POUR LA SCOLARISATION DES FILLES",
    new_name = "politique_pro_genre", #db_clean
    varnames_select = c("idx_q21a","q21a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q21a="Numéro sequentiel",
      q21a = "Mesure pour la scolarisation des fille",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q21a = "{str_to_upper(q21a)}"
    )
  ),
  politique_handicap_genre=list(# glimpse(db_raw %>% dm::dm_zoom_to(politique_handicap_genre))
    label = "MESURES POUR LA PROTECTION ET L'ACCOMPAGNEMENT DES ENFANTS EN SITUATION DE HANDICAP",
    new_name = "politique_pro_handicap", #db_clean
    varnames_select = c("idx_q22a","q22a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q22a="Numéro sequentiel",
      q22a = "Mesure pour la protection et l'accompagnement des enfants en situation de handicap",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q22a = "{str_to_upper(q22a)}"
    )
  )
)

# Traitement semi-automatique des donnees
for(dbname in dbnames_list){
 infos <- dbnames_params[[dbname]]
 suppressMessages(
   db <- db_raw[[dbname]] %>% 
     janitor::clean_names(case="none") %>% 
     select(all_of(infos$varnames_select)) %>% 
     # (1) traitement sepecifiques des donnees avec la logique dplyr
     mutate(
       across(
         names(infos$treatment_list),
         ~ str_glue(infos$treatment_list[cur_column()]))
     ) %>% 
     # Conversion en type de donnees standard afin que set_variable_label
     # fonctionne correctement
     type_convert() %>% 
     # (2) Etiquettage 
     labelled::set_variable_labels(.labels = infos$varnames_set_labs)
 )
 # (1b) Methode de traitement spécifiques des données a l'ancienne
 # sans la logique de dplyr
 # varlist <- names(infos$treatment_list)
 # for(varname in varlist) 
 #   db <- db %>%  mutate("{varname}":= str_glue(infos$treatment_list[varname]))
 
 # # Script pour visualiser le traitement et surtout afficher le dictionnaire
 # glimpse(db)
 # View(labelled::generate_dictionary(db,details = "basic") %>% 
 #        labelled::lookfor_to_long_format(),"dico")
 
 # Enregistrement des donnees traotes
 db_clean[[infos$new_name]] <- db
   
}

# Liste des identifiants 
idlist <- db_clean$main %>% 
  filter(validation_status!=param$STATUT_NOT_APPROVED) %>% 
  pull(index)
# On ne conserve que les donnnees approuvees
db_clean <- db_clean %>% map(
  function(.x){
    if('parent_index' %in% names(.x)){
      .x %>% filter(parent_index %in% idlist)
    }else{
      .x
    } 
  }
)

# (3) Export des donnees au format excel

file_name <- with(param$dbname,name[variable==database_name])
file_id <- with(param$dbname,order_id[variable==database_name])

# (3.1) Export des donnees brutes
dico <- list()
for(dbname in names(db_raw)){
  db <- db_raw[[dbname]] %>% 
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>% 
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=db_raw %>% as.list(),
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)#,colWidths="auto"

# (3.2) Export des donnees dites nettoyees
# Construction d'un dataset readme pour informer sur la nature de chaque onglet
db_readme <- tibble(database="main",description="BASE DE DONNEES PRINCIPALES") %>% 
  bind_rows(
    dbnames_params %>% names() %>% 
      # set_names() %>% 
      map_dfr( ~ dbnames_params[[.x]] %>% as_tibble() %>% select(new_name,label) %>% distinct()) %>% 
      rename(database=new_name,description=label)
  )

dico <- list()
#Ajout du dictionnaire Globale... celui per
dico$readme <- db_readme
# Extraction des dictionnaires associees a chaque table relationnelle
for(dbname in names(db_clean)){
  infos <- dbnames_params[[dbname]]
  db <- db_clean[[dbname]] %>% 
    # labelled::set_variable_labels(.labels = infos$varnames_set_labs) %>%
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>% 
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DICO.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=c(readme=list(db_readme),db_clean %>% as.list() %>% map(~ to_factor(.x))),
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DATA.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE,colWidths="auto"
)

# On crée la colonne JENA_attachements pour faciliter l'ouverture du dossier
# contenant les pièces jointes 
# Ouverture du fichier excel DATA avec openxls
# utilisation de la fonction makelink pour creer le iens dans l'onglet main
# HYPERLINK(LEFT(CELL("filename"),FIND("[",CELL("Filename"))-1)&"test.pdf","Link to PDF")
# nom_fichier <- sprintf("deliverable/02_clean/quali/%02d_%s_DATA.xlsx",file_id,file_name)
# wb <- loadWorkbook(file = nom_fichier)
# urls <- with(
#   db_clean$main,
#   sprintf("deliverable/02_clean/quali/%s/media/%s",file_name,hhid),
# )
# # names(urls) <- with(db_clean$main,hhid)
# # class(urls) <- "hyperlink"
# # writeDataTable(wb,sheet="main",x=urls,startRow=2, startCol = ncol(db_clean$main)+1)
# hyperlinks <- sprintf('HYPERLINK(LEFT(CELL("filename"),FIND("[",CELL("Filename"))-1)&"%s")',urls)
# writeFormula(
#   wb, sheet="main",
#   x = '=HYPERLINK("#Sheet2!B3", "Text to Display - Link to Sheet2")',
#   startRow=2, startCol = ncol(db_clean$main)+1
# )
# #Export
# saveWorkbook(wb,nom_fichier,overwrite = TRUE)

# (3.3) Les donnees traitées pour l'analyse l'ont été faites par Vanda, manuellement
# pour éviter des biais, nous n'avons pas proceder a leur traitement dans ce fichier
rm(db,db_raw,db_clean,dbnames_list,dbnames_params,infos,dico,database_name,file_name,file_id,
   idlist,db_readme,dbname)


#*********************************************************************************
# Traitement des données CLUSTER EDUCATION [quali]
#*********************************************************************************
database_name <- "cluster"
db_raw <- databases[[database_name]]  %>% # variable de stockage des donnees brutes
  #renomme les nom trop long
  dm::dm_rename_tbl(activites_copa_coges=activites_copa_coges_fonctionnel)



db_clean <- list() # variable de stockage des resultats des traitements

# (1) Traitement de la principale structure
# glimpse(db_raw %>% dm::dm_zoom_to(main))
db <- db_raw$main %>% 
  select(-any_of(param$varname_todelete)) %>% 
  select(-c(enum_name,nom_agent,sprintf("attachment%d",1:3),responsabilites_count,
            handicap_count,q02a_count,q18a_count)) %>% 
  janitor::clean_names(case="none") %>% 
  rename(form_id=xform_id_string,comment=attachment0) %>% 
  mutate(
    start = lubridate::as_datetime(start),
    end = lubridate::as_datetime(end),
    today=lubridate::as_date(today),
    submission_time= lubridate::as_datetime(submission_time),
    validation_status=to_character(validation_status),
    poste_travail = str_to_upper(poste_travail),
    telephone = ifelse(telephone=='000','',telephone),
    enum_id = parse_number(enum_id),
    province1_lab = ifelse(province_1==1,"ITURI",''),
    province2_lab = ifelse(province_2==1,"KASAI-CENTRAL",''),
    province3_lab = ifelse(province_3==1,"KASAI-ORIENTAL",''),
    province4_lab = ifelse(province_4==1,"NORD-KIVU",''),
    province5_lab = ifelse(province_5==1,"SUD-KIVU",''),
    province6_lab = ifelse(province_6==1,"TANGANYIKA",''),
    province = str_trim(str_c(province1_lab,province2_lab,province3_lab,
                              province4_lab,province5_lab,province6_lab,
                              sep=" ",collapse = NULL))
  ) %>% 
  select(-any_of(sprintf("province%d_lab",1:6))) %>% 
  # On ne conserve que les données correspondant à la période d’enquête
  filter(start>=param$survey_startdate,end<=param$survey_enddate) %>% 
  # Correction de la structure des donnees afin de faciliter l'export des resultats
  correcting.dataStructure() %>% 
  set_variable_labels(
    start = "Date d'ouverture du questionnaire",
    end = "Date de clôture du questionnaire",
    today = "Jour de collecte",
    hhid = "Identifiant observation",
    comment = "Compte rendu de l'entretien",
    poste_travail = "Poste de travail",
    telephone = "Numéro de téléphone de la source d’informations",
    enum_id = "Identifiant agent enquêteur",
    form_id = "Identifiant formulaire",
    index = "Clé primaire de la table",
    id = "Numéro séquentielle",
    uuid="Numéro de série",
    submission_time="Date de soumission",
    validation_status="Statut de validation du questionnaire",
    version="Idenfiant de la version du formulaire",
    territoire = "Territoires couverts",
    province = "Provinces couvertes",
    province_1="Province couverte :: ITURI",
    province_2="Province couverte :: KASAI-CENTRAL",
    province_3="Province couverte :: KASAI-ORIENTAL",
    province_4="Province couverte :: NORD-KIVU",
    province_5="Province couverte :: SUD-KIVU",
    province_6="Province couverte :: TANGANYIKA"
  )
# Sauvegarde le traitement dans la base de resultat qui sera sexport
db_clean$main <- db 

# (2) Traitement en boucle des bases de donnees  issus des boucles repeat
# dput(names(db_raw)[-1])
# dbnames_list <- c(
#   "responsabilites", "opinons", "raisons", "interventions", 
#   "situation_enft_pdi", "mesures_enft_pdi", "obstacles", "besoins", 
#   "mesures_secu_enft", "besoins_iee", "defis_ministere", "formation_mepst", 
#   "mesures_elearning", "mesures_verification", "actions_mepst", 
#   "politique_pro_genre", "politique_handicap_genre"
# )

dbnames_list <- names(db_raw)[-1]
dbnames_params <- list(
  responsabilites=list(# glimpse(db_raw %>% dm::dm_zoom_to(responsabilites))
    label = "TACHES DU REPONDANT",
    new_name = "tache_repondant",#db_clean
    varnames_select = c("idx_q02a","q02a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q02a="Numéro sequentiel",
      q02a = "Tâche du répondant",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q02a = "{str_to_upper(q02a)}"
    )
  ),
  situation_enft_pdi=list(# glimpse(db_raw %>% dm::dm_zoom_to(situation_enft_pdi))
    label = "DIFFICULTES DES ENFANTS DEPLACES INTERNES",
    new_name = "diff_enft_pdi", #db_clean
    varnames_select = c("idx_q06a","q06a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q06a="Numéro sequentiel",
      q06a = "Difficulté des enfants PDI",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q06a = "{str_to_upper(q06a)}"
    )
  ),
  mesures_enft_pdi=list(# glimpse(db_raw %>% dm::dm_zoom_to(mesures_enft_pdi))
    label = "MESURES MEPST EN FAVEUR DES ENFANTS DEPLACES INTERNES",
    new_name = "mesure_mepst_pdi", #db_clean
    varnames_select = c("idx_q07a","q07a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q07a="Numéro sequentiel",
      q07a = "Mesure MEPST en faveur les enfants PDI",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q07a = "{str_to_upper(q07a)}"
    )
  ),
  obstacles=list(# glimpse(db_raw %>% dm::dm_zoom_to(obstacles))
    label = "OBSTACLES EN MATIERE D'ACCES A L'EDUCATION",
    new_name = "obstacles", #db_clean
    varnames_select = c("idx_q08a","q08a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q08a="Numéro sequentiel",
      q08a = "Obstacle en matière d'accès à l'éducation",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q08a = "{str_to_upper(q08a)}"
    )
  ),
  besoins=list(# glimpse(db_raw %>% dm::dm_zoom_to(besoins))
    label = "BESOINS PRIORITAIRES POUR L'EDUCATION",
    new_name = "besoins_educ", #db_clean
    varnames_select = c("idx_q09a","q09a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q09a="Numéro sequentiel",
      q09a = "Besoin prioritaire en matière d'éducation",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q09a = "{str_to_upper(q09a)}"
    )
  ),
  besoins_iee=list(# glimpse(db_raw %>% dm::dm_zoom_to(besoins_iee))
    label = "BESOINS EN INTERVENTIONS EiE (Education en situation d'urgence)",
    new_name = "besoins_iee", #db_clean
    varnames_select = c("idx_q10a","q10a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q10a="Numéro sequentiel",
      q10a = "Besoin en intervention EiE",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q10a = "{str_to_upper(q10a)}"
    )
  ),
  actions_humanitaires=list(# glimpse(db_raw %>% dm::dm_zoom_to(actions_humanitaires))
    label = "REPONSES HUMANITAIRES AFIN DE MINIMISER LES TEMPS D'INTERRUPTION SCOLAIRE",
    new_name = "actions_humanitaires", #db_clean
    varnames_select = c("idx_q11a","q11a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q11a="Numéro sequentiel",
      q11a = "Action humanitaire afin de minimiser les temps d'interruption scolaire",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q11a = "{str_to_upper(q11a)}"
    )
  ),
  motifs_abandon_scolaire=list(# glimpse(db_raw %>% dm::dm_zoom_to(motifs_abandon_scolaire))
    label = "PRINCIPAUX MOTIFS DE L'ABANDON SCOLAIRE",
    new_name = "motifs_abandon_scolaire", #db_clean
    varnames_select = c("idx_q12a","q12a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q12a="Numéro sequentiel",
      q12a = "Motif de l'abandon scolaire",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q12a = "{str_to_upper(q12a)}"
    )
  ),
  causes_redoublement=list(# glimpse(db_raw %>% dm::dm_zoom_to(causes_redoublement))
    label = "PRINCIPALES CAUSES DE REDOUBLEMENT",
    new_name = "causes_redoublement", #db_clean
    varnames_select = c("idx_q13a","q13a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q13a="Numéro sequentiel",
      q13a = "Cause de redoublement",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q13a = "{str_to_upper(q13a)}"
    )
  ),
  protection=list(# glimpse(db_raw %>% dm::dm_zoom_to(protection))
    label = "PROBLEMES DE PROTECTION DES ENFANTS A L'ECOLE OU SUR LE CHEMIN DE L'ECOLE",
    new_name = "pb_protection_enft", #db_clean
    varnames_select = c("idx_q14a","q14a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q14a="Numéro sequentiel",
      q14a = "Problème de protection des enfants à l’école ou sur le chemin de l’école",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q14a = "{str_to_upper(q14a)}"
    )
  ),
  activites_copa_coges=list(# glimpse(db_raw %>% dm::dm_zoom_to(activites_copa_coges))
    label = "PRINCIPALES ACTIVITES DES COPA/COGES",
    new_name = "activites_copa_coges", #db_clean
    varnames_select = c("idx_q15a","q15a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q15a="Numéro sequentiel",
      q15a = "Activité des COPA/COGES",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q15a = "{str_to_upper(q15a)}"
    )
  ),
  elearning_intervention=list(# glimpse(db_raw %>% dm::dm_zoom_to(elearning_intervention))
    label = "PRINCIPALES INTERVENTIONS EN FAVEUR DE L'ENSEIGNEMENT A DISTANCE",
    new_name = "actions_humanitaires", #db_clean
    varnames_select = c("idx_q16a","q16a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q16a="Numéro sequentiel",
      q16a = "Intervention en faveur de l’enseignement à distance",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q16a = "{str_to_upper(q16a)}"
    )
  ),
  pb_genre=list(# glimpse(db_raw %>% dm::dm_zoom_to(pb_genre))
    label = "PROBLEMES DES FILLES ET DES FEMMES A L'ECOLE",
    new_name = "actions_humanitaires", #db_clean
    varnames_select = c("idx_q17a","q17a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q17a="Numéro sequentiel",
      q17a = "Problème des filles et des femmes à l'école",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q17a = "{str_to_upper(q17a)}"
    )
  ),
  handicap=list(# glimpse(db_raw %>% dm::dm_zoom_to(handicap))
    label = "DEFIS EN MATIERE D'EDUCATION DES ENFANTS EN SITUATION DE HANDICAP",
    new_name = "diff_enft_handicap", #db_clean
    varnames_select = c("idx_q18a","q18a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q18a="Numéro sequentiel",
      q18a = "Défis en matière d'éducation des enfants en situation de handicap",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q18a = "{str_to_upper(q18a)}"
    )
  )
  
)

# Traitement semi-automatique des donnees
for(dbname in dbnames_list){
  infos <- dbnames_params[[dbname]]
  suppressMessages(
    db <- db_raw[[dbname]] %>% 
      janitor::clean_names(case="none") %>% 
      select(all_of(infos$varnames_select)) %>% 
      # (1) traitement sepecifiques des donnees avec la logique dplyr
      mutate(
        across(
          names(infos$treatment_list),
          ~ str_glue(infos$treatment_list[cur_column()]))
      ) %>%
      # Conversion en type de donnees standard afin que set_variable_label
      # fonctionne correctement
      type_convert() %>% 
      # (2) Etiquettage 
      labelled::set_variable_labels(.labels = infos$varnames_set_labs)
  )
  # stockage de la  base de donnees traitees
  db_clean[[infos$new_name]] <- db
}

# Liste des identifiants 
idlist <- db_clean$main %>% 
  filter(validation_status!=param$STATUT_NOT_APPROVED) %>% 
  pull(index)
# On ne conserve que les donnnees approuvees
db_clean <- db_clean %>% map(
  function(.x){
    if('parent_index' %in% names(.x)){
      .x %>% filter(parent_index %in% idlist)
    }else{
      .x
    } 
  }
)

# (3) Export des donnees au format excel

file_name <- with(param$dbname,name[variable==database_name])
file_id <- with(param$dbname,order_id[variable==database_name])

# (3.1) Export des donnees brutes
dico <- list()
for(dbname in names(db_raw)){
  db <- db_raw[[dbname]] %>% 
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>% 
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=db_raw %>% as.list(),
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)#,colWidths="auto"

# (3.2) Export des donnees dites nettoyees
# Construction d'un dataset readme pour informer sur la nature de chaque onglet
db_readme <- tibble(database="main",description="BASE DE DONNEES PRINCIPALES") %>% 
  bind_rows(
    dbnames_params %>% names() %>% 
      # set_names() %>% 
      map_dfr( ~ dbnames_params[[.x]] %>% as_tibble() %>% select(new_name,label) %>% distinct()) %>% 
      rename(database=new_name,description=label)
  )

dico <- list()
#Ajout du dictionnaire Globale... celui per
dico$readme <- db_readme
# Extraction des dictionnaires associees a chaque table relationnelle
for(dbname in names(db_clean)){
  infos <- dbnames_params[[dbname]]
  db <- db_clean[[dbname]] %>% 
    # labelled::set_variable_labels(.labels = infos$varnames_set_labs) %>%
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>% 
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DICO.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=c(readme=list(db_readme),db_clean %>% as.list() %>% map(~ to_factor(.x))),
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DATA.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE,colWidths="auto"
)

rm(db,db_raw,db_clean,dbnames_list,dbnames_params,infos,dico,database_name,
   file_name,file_id,idlist,db_readme,dbname)


#*********************************************************************************
# Traitement des données ENSEIGNANT [quali]
#*********************************************************************************
database_name <- "enseignant"
db_raw <- databases[[database_name]]  %>% # variable de stockage des donnees brutes
  #renomme les nom trop long
  dm::dm_rename_tbl(consequences_pro=consequences_professionnelle)

# # Identification des noms trop long pour les noms d'onglet des feuilles excels
# [pas plus de 25 caracteres]
# names(db_raw) %>% purrr::set_names() %>% 
#   imap( ~ tibble(name=.y,size=nchar(.x))) %>% 
#   bind_rows() %>% filter(size>=25)


db_clean <- list() # variable de stockage des resultats des traitements

# (1) Traitement de la principale structure
# glimpse(db_raw %>% dm::dm_zoom_to(main))
db <- db_raw$main %>% 
  select(-any_of(param$varname_todelete)) %>% 
  select(-c(enum_name,nom_agent,sprintf("attachment%d",1:3),
            key01,key02,key_note,besoins_count,causes_redoublement_count,
            appui_enseignant_count,q09a_count,q10a_count,q17a_count)) %>% 
  janitor::clean_names(case="none") %>% 
  rename(form_id=xform_id_string,comment=attachment0) %>% 
  mutate(
    start = lubridate::as_datetime(start),
    end = lubridate::as_datetime(end),
    today=lubridate::as_date(today),
    submission_time= lubridate::as_datetime(submission_time),
    validation_status=to_character(validation_status),
    poste_travail = str_to_upper(poste_travail),
    telephone = ifelse(telephone=='000','',telephone),
    enum_id = parse_number(enum_id),
    ecole0 = as.character(ecole_sample0) %>% tidyr::replace_na(""),
    ecole1 = as.character(ecole_sample1) %>% tidyr::replace_na(""),
    schoolID = ifelse(readr::parse_number(ecole_sample0_estremplace)==1,as.character(ecole_sample1),
                      as.character(ecole_sample0)),
    id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID)),
    ecole_sample0 = to_character(ecole_sample0),
    ecole_sample1 = to_character(ecole_sample1)
    # province1_lab = ifelse(province_1==1,"ITURI",''),
    # province2_lab = ifelse(province_2==1,"KASAI-CENTRAL",''),
    # province3_lab = ifelse(province_3==1,"KASAI-ORIENTAL",''),
    # province4_lab = ifelse(province_4==1,"NORD-KIVU",''),
    # province5_lab = ifelse(province_5==1,"SUD-KIVU",''),
    # province6_lab = ifelse(province_6==1,"TANGANYIKA",''),
    # province = str_trim(str_c(province1_lab,province2_lab,province3_lab,
    #                           province4_lab,province5_lab,province6_lab,
    #                           sep=" ",collapse = NULL))
  ) %>% 
  # select(-any_of(sprintf("province%d_lab",1:6))) %>% 
  # On ne conserve que les données correspondant à la période d’enquête
  filter(start>=param$survey_startdate,end<=param$survey_enddate) %>% 
  # Correction de la structure des donnees afin de faciliter l'export des resultats
  correcting.dataStructure() %>% 
  set_variable_labels(
    start = "Date d'ouverture du questionnaire",
    end = "Date de clôture du questionnaire",
    today = "Jour de collecte",
    hhid = "Identifiant observation",
    comment = "Compte rendu de l'entretien",
    poste_travail = "Poste de travail",
    telephone = "Numéro de téléphone de la source d’informations",
    enum_id = "Identifiant agent enquêteur",
    form_id = "Identifiant formulaire",
    index = "Clé primaire de la table",
    id = "Numéro séquentielle",
    uuid="Numéro de série",
    submission_time="Date de soumission",
    validation_status="Statut de validation du questionnaire",
    version="Idenfiant de la version du formulaire",
    territoire = "Territoires couverts",
    ecole0="Nom de l’établissement issu de l'échantillon primaire",
    ecole_sample0="Nom de l’établissement issu de l'échantillon primaire",
    ecole1="Nom de l’établissement issu de l'échantillon des remplaçants",
    ecole_sample1="Nom de l’établissement issu de l'échantillon des remplaçants",
    schoolID ="Identifant applicatif de l'école enquêté",
    id_ecole="Identifant de l'école enquêté"#,
    # province = "Provinces couvertes",
    # province_1="Province couverte :: ITURI",
    # province_2="Province couverte :: KASAI-CENTRAL",
    # province_3="Province couverte :: KASAI-ORIENTAL",
    # province_4="Province couverte :: NORD-KIVU",
    # province_5="Province couverte :: SUD-KIVU",
    # province_6="Province couverte :: TANGANYIKA"
  )
# Sauvegarde le traitement dans la base de resultat qui sera export
db_clean$main <- db 

# (2) Traitement en boucle des bases de donnees  issus des boucles repeat
dbnames_list <- names(db_raw)[-1]
dbnames_params <- list(
  besoins=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(besoins))
    label = "BESOINS PRIORITAIRES POUR L'EDUCATION",
    new_name = "besoins_educ", #db_clean
    varnames_select = c("idx_q09a","q09a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q09a="Numéro sequentiel",
      q09a = "Besoin prioritaire en matière d'éducation",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q09a = "{str_to_upper(q09a)}"
    )
  ),
  causes_redoublement=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(causes_redoublement))
    label = "PRINCIPALES CAUSES DE REDOUBLEMENT",
    new_name = "causes_redoublement", #db_clean
    varnames_select = c("idx_q10a","q10a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q10a="Numéro sequentiel",
      q10a = "Cause de redoublement",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q10a = "{str_to_upper(q10a)}"
    )
  ),
  appui_enseignant=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(appui_enseignant))
    label = "APPUIS ESSENTIELS A UN TRAVAIL DE QUALITE DES ENSEIGNANTS",
    new_name = "appui_enseignant", #db_clean
    varnames_select = c("idx_q17a","q17a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q17a="Numéro sequentiel",
      q17a = "Appui nécessaire pour un travail de qualité des enseignants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q17a = "{str_to_upper(q17a)}"
    )
  ),
  consequence_perso=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(consequence_perso))
    label = "CONSEQUENCES DES CRISES DANS LA VIE PRIVEE DES ENSEIGNANTS",
    new_name = "consequence_perso", #db_clean
    varnames_select = c("idx_q05a","q05a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q05a="Numéro sequentiel",
      q05a = "Conséquences des crises dans la vie privée des enseignants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q05a = "{str_to_upper(q05a)}"
    )
  ),
  consequences_pro=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(consequences_pro))
    label = "CONSEQUENCES DES CRISES DANS LA VIE PROFESSIONNELLE DES ENSEIGNANTS",
    new_name = "consequences_pro", #db_clean
    varnames_select = c("idx_q05b","q05b","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q05b="Numéro sequentiel",
      q05b = "Conséquences des crises dans la vie professionnelle des enseignants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q05b = "{str_to_upper(q05b)}"
    )
  ),
  renforcement_capacite=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(renforcement_capacite))
    label = "LISTES DES FORMATIONS RECUES CES 2 DERNIERES ANNEES",
    new_name = "renforcement_capacite", #db_clean
    varnames_select = c("idx_q18a","q18a","q18b","q18c",
                        "parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q18a="Numéro sequentiel",
      q18a = "Formation reçues ces 2 dernières années",
      q18b = "Domaine de la formation",
      q18c = "Organisation(s) responsable(s) de la formation",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q18a = "{str_to_upper(q18a)}",
      q18b = "{str_to_upper(q18b)}",
      q18c = "{str_to_upper(q18c)}"
    )
  ),
  handicap=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(handicap))
    label = "PRINCIPALES DIFFICULTES AVEC LES ENFANTS EN SITUATION DE HANDICAP",
    new_name = "diff_enft_handicap", #db_clean
    varnames_select = c("idx_q22b","q22b","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q22b="Numéro sequentiel",
      q22b = "Difficulté rencontrée pour enseigner/intégrer les élèves en situation de handicap",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q22b = "{str_to_upper(q22b)}"
    )
  ),
  institutions=list(#names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(institutions))
    label = "LISTES DES INSTITUTIONS POUR LE SIGNALEMENT D'ABUS OU DE VIOLENCE SUR DES ENFANTS",
    new_name = "institutions", #db_clean
    varnames_select = c("idx_q12a","q12a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q12a="Numéro sequentiel",
      q12a = "Institution de signalement d'abus ou de violence sur des enfants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q12a = "{str_to_upper(q12a)}"
    )
  ),
  diff_enseignement=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(diff_enseignement))
    label = "DIFFICULTES RENCONTREES POUR UN ENSEIGNEMENT DE QUALITE",
    new_name = "diff_enseignement", #db_clean
    varnames_select = c("idx_q13a","q13a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q13a="Numéro sequentiel",
      q13a = "Difficulté rencontrée pour un enseignement de qualité",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q13a = "{str_to_upper(q13a)}"
    )
  ),
  elearning=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(elearning))
    label = "ACTIVITES D’ENSEIGNEMENT A DISTANCE",
    new_name = "elearning", #db_clean
    varnames_select = c("idx_q23a","q23a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q23a="Numéro sequentiel",
      q23a = "Activité d’enseignement à distance",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q23a = "{str_to_upper(q23a)}"
    )
  ),
  motif_absence_prof=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(motif_absence_prof))
    label = "LISTE DES MOTIFS D'ABSENCE DES ENSEIGNANTS",
    new_name = "motif_absence_prof", #db_clean
    varnames_select = c("idx_q14a","q14a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q14a="Numéro sequentiel",
      q14a = "Motif d'absence des enseignants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q14a = "{str_to_upper(q14a)}"
    )
  ),
  contenu_pro_genre=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(contenu_pro_genre))
    label = "LISTE DE MATERIELS PEDAGOGIQUE PRO-GENRE",
    new_name = "contenu_pro_genre", #db_clean
    varnames_select = c("idx_q21a","q21a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q21a="Numéro sequentiel",
      q21a = "Matériel pédagogique pro-genre ",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q21a = "{str_to_upper(q21a)}"
    )
  ),
  abus=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(abus))
    label = "LISTE DES SITUATIONS D'ABUS/VIOLENCE ENVERS LES ENFANTS",
    new_name = "abus", #db_clean
    varnames_select = c("idx_q11a","q11a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q11a="Numéro sequentiel",
      q11a = "Situation d’abus/violence envers les enfants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q11a = "{str_to_upper(q11a)}"
    )
  ),
  difficultes=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(difficultes))
    label = "LISTE DES DIFFICULTES DES ENSEIGNANTS LORSQUE L'ECOLE FONCTIONNE COMME ABRIS LE SOIR ET ECOLE LA JOURNEE",
    new_name = "diff_enseignant", #db_clean
    varnames_select = c("idx_q06a","q06a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q06a="Numéro sequentiel",
      q06a = "Difficulté rencontrée lorsque l'école fonctionne comme abris le soir et école en journée",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q06a = "{str_to_upper(q06a)}"
    )
  ),
  enft_pdi_defis=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(enft_pdi_defis))
    label = "LISTE DES DEFIS DES ENFANTS DEPLACES INTERNES AU SEIN DE L'ECOLE",
    new_name = "enft_pdi_defis", #db_clean
    varnames_select = c("idx_q08a","q08a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q08a="Numéro sequentiel",
      q08a = "Défi auquel est confronté les enfants PDI au sein de l'école",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q08a = "{str_to_upper(q08a)}"
    )
  ),
  formation_specifique=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(formation_specifique))
    label = "LISTE DES FORMATIONS RECUES POUR TRAVAILLER AVEC LES ENFANTS EN SITUATION DE HANDICAP",
    new_name = "formation_handicap", #db_clean
    varnames_select = c("idx_q19a","q19a","q19b","q19c",
                        "parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q19a="Numéro sequentiel",
      q19a = "Formation pour travailler avec les enfants en situation de handicap",
      q19b = "Domaine de la formation",
      q19c = "Organisation(s) responsable(s) de la formation",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q19a = "{str_to_upper(q19a)}",
      q19b = "{str_to_upper(q19b)}",
      q19c = "{str_to_upper(q19c)}"
    )
  ),
  appui_psychologique=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(appui_psychologique))
    label = "LISTE DES APPUIS PSYCHOLOGIQUE RECUES",
    new_name = "appui_psychologique", #db_clean
    varnames_select = c("idx_q20a","q20a","q20b","q20c",
                        "parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q20a="Numéro sequentiel",
      q20a = "Assistance psychologique",
      q20b = "Domaine de l'assistance",
      q20c = "Organisation(s) responsable(s) de l'assistance",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q20a = "{str_to_upper(q20a)}",
      q20b = "{str_to_upper(q20b)}",
      q20c = "{str_to_upper(q20c)}"
    )
  )
  
)

# Traitement semi-automatique des donnees
for(dbname in dbnames_list){
  infos <- dbnames_params[[dbname]]
  suppressMessages(
    db <- db_raw[[dbname]] %>% 
      janitor::clean_names(case="none") %>% 
      select(all_of(infos$varnames_select)) %>% 
      # (1) traitement sepecifiques des donnees avec la logique dplyr
      mutate(
        across(
          names(infos$treatment_list),
          ~ str_glue(infos$treatment_list[cur_column()]))
      ) %>%
      # Conversion en type de donnees standard afin que set_variable_label
      # fonctionne correctement
      type_convert() %>%
      # (2) Etiquettage 
      labelled::set_variable_labels(.labels = infos$varnames_set_labs)
  )
  # stockage de la  base de donnees traitees
  db_clean[[infos$new_name]] <- db
}

# Liste des identifiants 
idlist <- db_clean$main %>% 
  filter(validation_status!=param$STATUT_NOT_APPROVED) %>% 
  pull(index)
# On ne conserve que les donnnees approuvees
db_clean <- db_clean %>% map(
  function(.x){
    if('parent_index' %in% names(.x)){
      .x %>% filter(parent_index %in% idlist)
    }else{
      .x
    } 
  }
)

# (3) Export des donnees au format excel
file_name <- with(param$dbname,name[variable==database_name])
file_id <- with(param$dbname,order_id[variable==database_name])

# (3.1) Export des donnees brutes
dico <- list()
for(dbname in names(db_raw)){
  db <- db_raw[[dbname]] %>% 
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>% 
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=db_raw %>% as.list(),
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)#,colWidths="auto"

# (3.2) Export des donnees dites nettoyees
# Construction d'un dataset readme pour informer sur la nature de chaque onglet
db_readme <- tibble(database="main",description="BASE DE DONNEES PRINCIPALES") %>% 
  bind_rows(
    dbnames_params %>% names() %>% 
      # set_names() %>% 
      map_dfr( ~ dbnames_params[[.x]][c("new_name","label")] %>% as_tibble() %>% distinct()) %>% 
      rename(database=new_name,description=label)
  )

dico <- list()
#Ajout du dictionnaire Globale... celui per
dico$readme <- db_readme
# Extraction des dictionnaires associees a chaque table relationnelle
for(dbname in names(db_clean)){
  infos <- dbnames_params[[dbname]]
  db <- db_clean[[dbname]] %>% 
    # labelled::set_variable_labels(.labels = infos$varnames_set_labs) %>%
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>% 
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DICO.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=c(readme=list(db_readme),db_clean %>% as.list() %>% map(~ to_factor(.x))),
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DATA.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE,colWidths="auto"
)

rm(db,db_raw,db_clean,dbnames_list,dbnames_params,infos,dico,database_name,
   file_name,file_id,idlist,db_readme,dbname)


#*********************************************************************************
# Traitement des données ENFANTS 6-9 ANS SCOLARISES [quali]
#*********************************************************************************
database_name <- "es_0609"
db_raw <- databases[[database_name]]  #%>% # variable de stockage des donnees brutes
#renomme les nom trop long
# dm::dm_rename_tbl(consequences_pro=consequences_professionnelle)

# # Identification des noms trop long pour les noms d'onglet des feuilles excels
# [pas plus de 25 caracteres]
# names(db_raw) %>% purrr::set_names() %>%
#   imap( ~ tibble(name=.y,size=nchar(.x))) %>%
#   bind_rows() %>% filter(size>=25)


db_clean <- list() # variable de stockage des resultats des traitements

# (1) Traitement de la principale structure
# glimpse(db_raw %>% dm::dm_zoom_to(main))
db <- db_raw$main %>% 
  select(-any_of(param$varname_todelete)) %>% 
  select(-c(enum_name,nom_agent,sprintf("attachment%d",1:3),
            key01,key02,key_note,p_count_warning,q01a_count,q01b_count,
            defis_count,idees_count,propositions_count,aspects_positifs_count,
            aspects_negatifs_count)) %>% 
  janitor::clean_names(case="none") %>% 
  rename(form_id=xform_id_string,comment=attachment0) %>% 
  mutate(
    start = lubridate::as_datetime(start),
    end = lubridate::as_datetime(end),
    today=lubridate::as_date(today),
    submission_time= lubridate::as_datetime(submission_time),
    validation_status=to_character(validation_status),
    # poste_travail = str_to_upper(poste_travail),
    # telephone = ifelse(telephone=='000','',telephone),
    enum_id = parse_number(enum_id),
    ecole0 = as.character(ecole_sample0) %>% tidyr::replace_na(""),
    ecole1 = as.character(ecole_sample1) %>% tidyr::replace_na(""),
    schoolID = ifelse(readr::parse_number(ecole_sample0_estremplace)==1,as.character(ecole_sample1),
                      as.character(ecole_sample0)),
    id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID)),
    ecole_sample0 = to_character(ecole_sample0),
    ecole_sample1 = to_character(ecole_sample1)
    # province1_lab = ifelse(province_1==1,"ITURI",''),
    # province2_lab = ifelse(province_2==1,"KASAI-CENTRAL",''),
    # province3_lab = ifelse(province_3==1,"KASAI-ORIENTAL",''),
    # province4_lab = ifelse(province_4==1,"NORD-KIVU",''),
    # province5_lab = ifelse(province_5==1,"SUD-KIVU",''),
    # province6_lab = ifelse(province_6==1,"TANGANYIKA",''),
    # province = str_trim(str_c(province1_lab,province2_lab,province3_lab,
    #                           province4_lab,province5_lab,province6_lab,
    #                           sep=" ",collapse = NULL))
  ) %>% 
  # select(-any_of(sprintf("province%d_lab",1:6))) %>% 
  # On ne conserve que les données correspondant à la période d’enquête
  filter(start>=param$survey_startdate,end<=param$survey_enddate) %>% 
  # Correction de la structure des donnees afin de faciliter l'export des resultats
  correcting.dataStructure() %>% 
  set_variable_labels(
    start = "Date d'ouverture du questionnaire",
    end = "Date de clôture du questionnaire",
    today = "Jour de collecte",
    hhid = "Identifiant observation",
    comment = "Compte rendu de l'entretien",
    # poste_travail = "Poste de travail",
    # telephone = "Numéro de téléphone de la source d’informations",
    enum_id = "Identifiant agent enquêteur",
    form_id = "Identifiant formulaire",
    index = "Clé primaire de la table",
    id = "Numéro séquentielle",
    uuid="Numéro de série",
    submission_time="Date de soumission",
    validation_status="Statut de validation du questionnaire",
    version="Idenfiant de la version du formulaire",
    territoire = "Territoires couverts",
    ecole0="Nom de l’établissement issu de l'échantillon primaire",
    ecole_sample0="Nom de l’établissement issu de l'échantillon primaire",
    ecole1="Nom de l’établissement issu de l'échantillon des remplaçants",
    ecole_sample1="Nom de l’établissement issu de l'échantillon des remplaçants",
    schoolID ="Identifant applicatif de l'école enquêté",
    id_ecole="Identifant de l'école enquêté"#,
    # province = "Provinces couvertes",
    # province_1="Province couverte :: ITURI",
    # province_2="Province couverte :: KASAI-CENTRAL",
    # province_3="Province couverte :: KASAI-ORIENTAL",
    # province_4="Province couverte :: NORD-KIVU",
    # province_5="Province couverte :: SUD-KIVU",
    # province_6="Province couverte :: TANGANYIKA"
  )
# Sauvegarde le traitement dans la base de resultat qui sera export
db_clean$main <- db 

# (2) Traitement en boucle des bases de donnees  issus des boucles repeat
dbnames_list <- names(db_raw)[-1]
dbnames_params <- list(
  aspects_positifs=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(aspects_positifs))
    label = "LISTES DES ASPECTS POSITIFS DE L'ECOLE",
    new_name = "aspects_positifs", #db_clean
    varnames_select = c("idx_q01a","q01a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q01a="Numéro sequentiel",
      q01a = "Aspectfs positifs",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q01a = "{str_to_upper(q01a)}"
    )
  ),
  aspects_negatifs=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(aspects_negatifs))
    label = "LISTES DES ASPECTS NEGATIFS DE L'ECOLE",
    new_name = "aspects_negatifs", #db_clean
    varnames_select = c("idx_q01b","q01b","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q01b="Numéro sequentiel",
      q01b = "Aspectfs négatifs",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q01b = "{str_to_upper(q01b)}"
    )
  ),
  defis=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(defis))
    label = "DIFFICULTES DES ENFANTS A L'ECOLE",
    new_name = "diff_ecole", #db_clean
    varnames_select = c("idx_q02a","q02a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q02a="Numéro sequentiel",
      q02a = "Défi auquel les enfants font face à l’école",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q02a = "{str_to_upper(q02a)}"
    )
  ),
  idees=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(idees))
    label = "IDEES D'AMELIORATION DE L'ECOLE",
    new_name = "idees", #db_clean
    varnames_select = c("idx_q03a","q03a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q03a="Numéro sequentiel",
      q03a = "Idée d'amélioration de l'école",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q03a = "{str_to_upper(q03a)}"
    )
  ),
  propositions=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(propositions))
    label = "CARACTERISTIQUES DE L'ECOLE DE REVE",
    new_name = "propositions", #db_clean
    varnames_select = c("idx_q04a","q04a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q04a="Numéro sequentiel",
      q04a = "Caractéristique de l'école de rêve",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q04a = "{str_to_upper(q04a)}"
    )
  )
)

# Traitement semi-automatique des donnees
for(dbname in dbnames_list){
  infos <- dbnames_params[[dbname]]
  suppressMessages(
  db <- db_raw[[dbname]] %>% 
    janitor::clean_names(case="none") %>% 
    select(all_of(infos$varnames_select)) %>% 
    # (1) traitement sepecifiques des donnees avec la logique dplyr
    mutate(
      across(
        names(infos$treatment_list),
        ~ str_glue(infos$treatment_list[cur_column()]))
    ) %>%
    # Conversion en type de donnees standard afin que set_variable_label
    # fonctionne correctement
    type_convert() %>%
    # (2) Etiquettage 
    labelled::set_variable_labels(.labels = infos$varnames_set_labs)
  )
  # stockage de la  base de donnees traitees
  db_clean[[infos$new_name]] <- db
}

# Liste des identifiants 
idlist <- db_clean$main %>% 
  filter(validation_status!=param$STATUT_NOT_APPROVED) %>% 
  pull(index)
# On ne conserve que les donnnees approuvees
db_clean <- db_clean %>% map(
  function(.x){
    if('parent_index' %in% names(.x)){
      .x %>% filter(parent_index %in% idlist)
    }else{
      .x
    } 
  }
)

# (3) Export des donnees au format excel
file_name <- with(param$dbname,name[variable==database_name])
file_id <- with(param$dbname,order_id[variable==database_name])

# (3.1) Export des donnees brutes
dico <- list()
for(dbname in names(db_raw)){
  db <- db_raw[[dbname]] %>% 
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>% 
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=db_raw %>% as.list(),
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)#,colWidths="auto"

# (3.2) Export des donnees dites nettoyees
# Construction d'un dataset readme pour informer sur la nature de chaque onglet
db_readme <- tibble(database="main",description="BASE DE DONNEES PRINCIPALES") %>% 
  bind_rows(
    dbnames_params %>% names() %>% 
      # set_names() %>% 
      map_dfr( ~ dbnames_params[[.x]][c("new_name","label")] %>% as_tibble() %>% distinct()) %>% 
      rename(database=new_name,description=label)
  )

dico <- list()
#Ajout du dictionnaire Globale... celui per
dico$readme <- db_readme
# Extraction des dictionnaires associees a chaque table relationnelle
for(dbname in names(db_clean)){
  infos <- dbnames_params[[dbname]]
  db <- db_clean[[dbname]] %>% 
    # labelled::set_variable_labels(.labels = infos$varnames_set_labs) %>%
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>% 
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DICO.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=c(readme=list(db_readme),db_clean %>% as.list() %>% map(~ to_factor(.x))),
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DATA.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE,colWidths="auto"
)

rm(db,db_raw,db_clean,dbnames_list,dbnames_params,infos,dico,database_name,
   file_name,file_id,idlist,db_readme,dbname)


#*********************************************************************************
# Traitement des données ENFANTS 6-9 ANS NON SCOLARISES [quali]
#*********************************************************************************
database_name <- "ens_0609"
db_raw <- databases[[database_name]]  #%>% # variable de stockage des donnees brutes
#renomme les nom trop long
# dm::dm_rename_tbl(consequences_pro=consequences_professionnelle)

# # Identification des noms trop long pour les noms d'onglet des feuilles excels
# [pas plus de 25 caracteres]
# names(db_raw) %>% purrr::set_names() %>%
#   imap( ~ tibble(name=.y,size=nchar(.x))) %>%
#   bind_rows() %>% filter(size>=25)


db_clean <- list() # variable de stockage des resultats des traitements

# (1) Traitement de la principale structure
# glimpse(db_raw %>% dm::dm_zoom_to(main))
db <- db_raw$main %>% 
  select(-any_of(param$varname_todelete)) %>% 
  select(-c(enum_name,nom_agent,sprintf("attachment%d",1:3),
            key01,key02,key_note,p_count_warning,q01a_count,
            q02a_count,q03a_count,propositions_count,motifs_count,
            actions_count,activites_count)) %>% 
  janitor::clean_names(case="none") %>% 
  rename(form_id=xform_id_string,comment=attachment0) %>% 
  mutate(
    start = lubridate::as_datetime(start),
    end = lubridate::as_datetime(end),
    today=lubridate::as_date(today),
    submission_time= lubridate::as_datetime(submission_time),
    validation_status=to_character(validation_status),
    # poste_travail = str_to_upper(poste_travail),
    # telephone = ifelse(telephone=='000','',telephone),
    enum_id = parse_number(enum_id),
    ecole0 = as.character(ecole_sample0) %>% tidyr::replace_na(""),
    ecole1 = as.character(ecole_sample1) %>% tidyr::replace_na(""),
    schoolID = ifelse(readr::parse_number(ecole_sample0_estremplace)==1,as.character(ecole_sample1),
                      as.character(ecole_sample0)),
    id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID)),
    ecole_sample0 = to_character(ecole_sample0),
    ecole_sample1 = to_character(ecole_sample1)
    # province1_lab = ifelse(province_1==1,"ITURI",''),
    # province2_lab = ifelse(province_2==1,"KASAI-CENTRAL",''),
    # province3_lab = ifelse(province_3==1,"KASAI-ORIENTAL",''),
    # province4_lab = ifelse(province_4==1,"NORD-KIVU",''),
    # province5_lab = ifelse(province_5==1,"SUD-KIVU",''),
    # province6_lab = ifelse(province_6==1,"TANGANYIKA",''),
    # province = str_trim(str_c(province1_lab,province2_lab,province3_lab,
    #                           province4_lab,province5_lab,province6_lab,
    #                           sep=" ",collapse = NULL))
  ) %>% 
  # select(-any_of(sprintf("province%d_lab",1:6))) %>% 
  # On ne conserve que les données correspondant à la période d’enquête
  filter(start>=param$survey_startdate,end<=param$survey_enddate) %>% 
  # Correction de la structure des donnees afin de faciliter l'export des resultats
  correcting.dataStructure() %>% 
  set_variable_labels(
    start = "Date d'ouverture du questionnaire",
    end = "Date de clôture du questionnaire",
    today = "Jour de collecte",
    hhid = "Identifiant observation",
    comment = "Compte rendu de l'entretien",
    # poste_travail = "Poste de travail",
    # telephone = "Numéro de téléphone de la source d’informations",
    enum_id = "Identifiant agent enquêteur",
    form_id = "Identifiant formulaire",
    index = "Clé primaire de la table",
    id = "Numéro séquentielle",
    uuid="Numéro de série",
    submission_time="Date de soumission",
    validation_status="Statut de validation du questionnaire",
    version="Idenfiant de la version du formulaire",
    territoire = "Territoires couverts",
    ecole0="Nom de l’établissement issu de l'échantillon primaire",
    ecole_sample0="Nom de l’établissement issu de l'échantillon primaire",
    ecole1="Nom de l’établissement issu de l'échantillon des remplaçants",
    ecole_sample1="Nom de l’établissement issu de l'échantillon des remplaçants",
    schoolID ="Identifant applicatif de l'école enquêté",
    id_ecole="Identifant de l'école enquêté"#,
    # province = "Provinces couvertes",
    # province_1="Province couverte :: ITURI",
    # province_2="Province couverte :: KASAI-CENTRAL",
    # province_3="Province couverte :: KASAI-ORIENTAL",
    # province_4="Province couverte :: NORD-KIVU",
    # province_5="Province couverte :: SUD-KIVU",
    # province_6="Province couverte :: TANGANYIKA"
  )
# Sauvegarde le traitement dans la base de resultat qui sera export
db_clean$main <- db 

# (2) Traitement en boucle des bases de donnees  issus des boucles repeat
dbnames_list <- names(db_raw)[-1]
dbnames_params <- list(
  activites=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(activites))
    label = "LISTE DES ACTIVITES QUOTIDIENNES",
    new_name = "activites_quotidiennes", #db_clean
    varnames_select = c("idx_q01a","q01a","q01b","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q01a="Numéro sequentiel",
      q01a = "Libellé de l'activté",
      q01b = "Activité à risque pour la protection de l'enfant",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q01a = "{str_to_upper(q01a)}"
    )
  ),
  motifs=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(motifs))
    label = "LISTE DES MOTIFS DE LA NON FREQUENTATION SCOLAIRE",
    new_name = "motifs_non_scolarisation", #db_clean
    varnames_select = c("idx_q02a","q02a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q02a="Numéro sequentiel",
      q02a = "Motif de la non fréquentation scolaire",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q02a = "{str_to_upper(q02a)}"
    )
  ),
  actions=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(actions))
    label = "LISTE DES INTERVENTIONS NECESSAIRES POUR LA SCOLARISATION DES ENFANTS",
    new_name = "interventions_necessaires", #db_clean
    varnames_select = c("idx_q03a","q03a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q03a="Numéro sequentiel",
      q03a = "Intervention nécessaire pour scolariser les enfants de la communauté",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q03a = "{str_to_upper(q03a)}"
    )
  ),
  propositions=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(propositions))
    label = "CARACTERISTIQUES DE L'ECOLE DE REVE",
    new_name = "propositions", #db_clean
    varnames_select = c("idx_q04a","q04a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q04a="Numéro sequentiel",
      q04a = "Caractéristique de l'école de rêve",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q04a = "{str_to_upper(q04a)}"
    )
  )
)

# Traitement semi-automatique des donnees
for(dbname in dbnames_list){
  infos <- dbnames_params[[dbname]]
  suppressMessages(
    db <- db_raw[[dbname]] %>% 
      janitor::clean_names(case="none") %>% 
      select(all_of(infos$varnames_select)) %>% 
      # (1) traitement sepecifiques des donnees avec la logique dplyr
      mutate(
        across(
          names(infos$treatment_list),
          ~ str_glue(infos$treatment_list[cur_column()]))
      ) %>%
      # Conversion en type de donnees standard afin que set_variable_label
      # fonctionne correctement
      type_convert() %>%
      # (2) Etiquettage 
      labelled::set_variable_labels(.labels = infos$varnames_set_labs)
  )
  # stockage de la  base de donnees traitees
  db_clean[[infos$new_name]] <- db
}

# Liste des identifiants 
idlist <- db_clean$main %>% 
  filter(validation_status!=param$STATUT_NOT_APPROVED) %>% 
  pull(index)
# On ne conserve que les donnnees approuvees
db_clean <- db_clean %>% map(
  function(.x){
    if('parent_index' %in% names(.x)){
      .x %>% filter(parent_index %in% idlist)
    }else{
      .x
    } 
  }
)

# (3) Export des donnees au format excel
file_name <- with(param$dbname,name[variable==database_name])
file_id <- with(param$dbname,order_id[variable==database_name])

# (3.1) Export des donnees brutes
dico <- list()
for(dbname in names(db_raw)){
  db <- db_raw[[dbname]] %>% 
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>% 
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=db_raw %>% as.list(),
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)#,colWidths="auto"

# (3.2) Export des donnees dites nettoyees
# Construction d'un dataset readme pour informer sur la nature de chaque onglet
db_readme <- tibble(database="main",description="BASE DE DONNEES PRINCIPALES") %>% 
  bind_rows(
    dbnames_params %>% names() %>% 
      # set_names() %>% 
      map_dfr( ~ dbnames_params[[.x]][c("new_name","label")] %>% as_tibble() %>% distinct()) %>% 
      rename(database=new_name,description=label)
  )

dico <- list()
#Ajout du dictionnaire Globale... celui per
dico$readme <- db_readme
# Extraction des dictionnaires associees a chaque table relationnelle
for(dbname in names(db_clean)){
  infos <- dbnames_params[[dbname]]
  db <- db_clean[[dbname]] %>% 
    # labelled::set_variable_labels(.labels = infos$varnames_set_labs) %>%
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>% 
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DICO.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=c(readme=list(db_readme),db_clean %>% as.list() %>% map(~ to_factor(.x))),
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DATA.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE,colWidths="auto"
)

rm(db,db_raw,db_clean,dbnames_list,dbnames_params,infos,dico,database_name,
   file_name,file_id,idlist,db_readme,dbname)


#*********************************************************************************
# Traitement des données FGD ENFANTS NON-SCOLARISES 13-16 ANS [quali]
#*********************************************************************************
database_name <- "ens_1316" # names(databases)
db_raw <- databases[[database_name]]  #%>% # variable de stockage des donnees brutes
#renomme les nom trop long
# dm::dm_rename_tbl(consequences_pro=consequences_professionnelle)

# # Identification des noms trop long pour les noms d'onglet des feuilles excels
# [pas plus de 25 caracteres]
# names(db_raw) %>% purrr::set_names() %>%
#   imap( ~ tibble(name=.y,size=nchar(.x))) %>%
#   bind_rows() %>% filter(size>=25)


db_clean <- list() # variable de stockage des resultats des traitements

# (1) Traitement de la principale structure
# glimpse(db_raw %>% dm::dm_zoom_to(main))
db <- db_raw$main %>% 
  select(-any_of(param$varname_todelete)) %>% 
  select(-c(enum_name,nom_agent,sprintf("attachment%d",1:3),
            key01,key02,key_note,p_count_warning,q01a_count,
            q02a_count,actions_count,vulnerables_count,raisons_count)) %>% 
  janitor::clean_names(case="none") %>% 
  rename(form_id=xform_id_string,comment=attachment0) %>% 
  mutate(
    start = lubridate::as_datetime(start),
    end = lubridate::as_datetime(end),
    today=lubridate::as_date(today),
    submission_time= lubridate::as_datetime(submission_time),
    validation_status=to_character(validation_status),
    # poste_travail = str_to_upper(poste_travail),
    # telephone = ifelse(telephone=='000','',telephone),
    enum_id = parse_number(enum_id),
    ecole0 = as.character(ecole_sample0) %>% tidyr::replace_na(""),
    ecole1 = as.character(ecole_sample1) %>% tidyr::replace_na(""),
    schoolID = ifelse(readr::parse_number(ecole_sample0_estremplace)==1,as.character(ecole_sample1),
                      as.character(ecole_sample0)),
    id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID)),
    ecole_sample0 = to_character(ecole_sample0),
    ecole_sample1 = to_character(ecole_sample1)
    # province1_lab = ifelse(province_1==1,"ITURI",''),
    # province2_lab = ifelse(province_2==1,"KASAI-CENTRAL",''),
    # province3_lab = ifelse(province_3==1,"KASAI-ORIENTAL",''),
    # province4_lab = ifelse(province_4==1,"NORD-KIVU",''),
    # province5_lab = ifelse(province_5==1,"SUD-KIVU",''),
    # province6_lab = ifelse(province_6==1,"TANGANYIKA",''),
    # province = str_trim(str_c(province1_lab,province2_lab,province3_lab,
    #                           province4_lab,province5_lab,province6_lab,
    #                           sep=" ",collapse = NULL))
  ) %>% 
  # select(-any_of(sprintf("province%d_lab",1:6))) %>% 
  # On ne conserve que les données correspondant à la période d’enquête
  filter(start>=param$survey_startdate,end<=param$survey_enddate) %>% 
  # Correction de la structure des donnees afin de faciliter l'export des resultats
  correcting.dataStructure() %>% 
  set_variable_labels(
    start = "Date d'ouverture du questionnaire",
    end = "Date de clôture du questionnaire",
    today = "Jour de collecte",
    hhid = "Identifiant observation",
    comment = "Compte rendu de l'entretien",
    # poste_travail = "Poste de travail",
    # telephone = "Numéro de téléphone de la source d’informations",
    enum_id = "Identifiant agent enquêteur",
    form_id = "Identifiant formulaire",
    index = "Clé primaire de la table",
    id = "Numéro séquentielle",
    uuid="Numéro de série",
    submission_time="Date de soumission",
    validation_status="Statut de validation du questionnaire",
    version="Idenfiant de la version du formulaire",
    territoire = "Territoires couverts",
    ecole0="Nom de l’établissement issu de l'échantillon primaire",
    ecole_sample0="Nom de l’établissement issu de l'échantillon primaire",
    ecole1="Nom de l’établissement issu de l'échantillon des remplaçants",
    ecole_sample1="Nom de l’établissement issu de l'échantillon des remplaçants",
    schoolID ="Identifant applicatif de l'école enquêté",
    id_ecole="Identifant de l'école enquêté"#,
    # province = "Provinces couvertes",
    # province_1="Province couverte :: ITURI",
    # province_2="Province couverte :: KASAI-CENTRAL",
    # province_3="Province couverte :: KASAI-ORIENTAL",
    # province_4="Province couverte :: NORD-KIVU",
    # province_5="Province couverte :: SUD-KIVU",
    # province_6="Province couverte :: TANGANYIKA"
  )
# Sauvegarde le traitement dans la base de resultat qui sera export
db_clean$main <- db 

# (2) Traitement en boucle des bases de donnees  issus des boucles repeat
dbnames_list <- names(db_raw)[-1]
dbnames_params <- list(
  vulnerables=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(vulnerables))
    label = "LISTE DES CATEGORIES D'ENFANTS VULNERABLES",
    new_name = "liste_vulnerabilites", #db_clean
    varnames_select = c("idx_q01a","q01a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q01a="Numéro sequentiel",
      q01a = "Catégorie d'enfants vulnérables",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q01a = "{str_to_upper(q01a)}"
    )
  ),
  raisons=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(raisons))
    label = "LISTE DES MOTIFS DE LA NON FREQUENTATION SCOLAIRE",
    new_name = "motifs_non_scolarisation", #db_clean
    varnames_select = c("idx_q02a","q02a",sprintf("q03_%d",c(0:2,96)),
                        "q03_other","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q02a="Numéro sequentiel",
      q02a = "Motif de la non fréquentation scolaire",
      q03_0 = "Motif non valable pour les vulnérables identifiés",
      q03_1 = "Motif plus import pour les garçons",
      q03_2 = "Motif plus important pour les filles",
      q03_96 = "Motif plus important pour autre groupe de vulnérables",
      q03_other = "Autre groupe de vulnérable",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q02a = "{str_to_upper(q02a)}",
      q03_other = "{str_to_upper(replace_na(q03_other,''))}"
    )
  ),
  actions=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(actions))
    label = "LISTE DES INTERVENTIONS NECESSAIRES POUR LA SCOLARISATION DES ENFANTS",
    new_name = "interventions_necessaires", #db_clean
    varnames_select = c("idx_q04a","q04a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q04a="Numéro sequentiel",
      q04a = "Intervention nécessaire pour scolariser les enfants de la communauté",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q04a = "{str_to_upper(q04a)}"
    )
  )
)

# Traitement semi-automatique des donnees
for(dbname in dbnames_list){
  infos <- dbnames_params[[dbname]]
  suppressMessages(
    db <- db_raw[[dbname]] %>% 
      janitor::clean_names(case="none") %>% 
      select(all_of(infos$varnames_select)) %>% 
      # (1) traitement sepecifiques des donnees avec la logique dplyr
      mutate(
        across(
          names(infos$treatment_list),
          ~ str_glue(infos$treatment_list[cur_column()]))
      ) %>% 
      # Conversion en type de donnees standard afin que set_variable_label
      # fonctionne correctement
      type_convert() %>% 
      # (2) Etiquettage 
      labelled::set_variable_labels(.labels = infos$varnames_set_labs)
  )
  # stockage de la  base de donnees traitees
  db_clean[[infos$new_name]] <- db
}

# Liste des identifiants 
idlist <- db_clean$main %>% 
  filter(validation_status!=param$STATUT_NOT_APPROVED) %>% 
  pull(index)
# On ne conserve que les donnnees approuvees
db_clean <- db_clean %>% map(
  function(.x){
    if('parent_index' %in% names(.x)){
      .x %>% filter(parent_index %in% idlist)
    }else{
      .x
    } 
  }
)

# (3) Export des donnees au format excel
file_name <- with(param$dbname,name[variable==database_name])
file_id <- with(param$dbname,order_id[variable==database_name])

# (3.1) Export des donnees brutes
dico <- list()
for(dbname in names(db_raw)){
  db <- db_raw[[dbname]] %>% 
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>% 
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=db_raw %>% as.list(),
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)#,colWidths="auto"

# (3.2) Export des donnees dites nettoyees
# Construction d'un dataset readme pour informer sur la nature de chaque onglet
db_readme <- tibble(database="main",description="BASE DE DONNEES PRINCIPALES") %>% 
  bind_rows(
    dbnames_params %>% names() %>% 
      # set_names() %>% 
      map_dfr( ~ dbnames_params[[.x]][c("new_name","label")] %>% as_tibble() %>% distinct()) %>% 
      rename(database=new_name,description=label)
  )

# Test : db_clean[-1] %>%  map( ~ generate_dictionary(.x))
dico <- c(
  list(readme=db_readme),
  db_clean %>%  map(
    function(.x){
      generate_dictionary(.x) %>% 
        labelled::lookfor_to_long_format() %>% 
        mutate(
          col_type = case_when(
            col_type=='dttm' ~ "date",
            col_type=='chr+lbl' ~ "dbl+lbl",
            col_type=='lgl' ~ "chr",
            .default = col_type
          ),
          values = ifelse(
            !is.na(value_labels),
            str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
            levels
          ),
          labels=ifelse(
            !is.na(value_labels),
            str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
            value_labels
          )
        )
    }
  )
)

# Dico export
rio::export(
  x=dico,
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DICO.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=c(readme=list(db_readme),db_clean %>% as.list() %>% map(~ to_factor(.x))),
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DATA.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE,colWidths="auto"
)

rm(db,db_raw,db_clean,dbnames_list,dbnames_params,infos,dico,database_name,
   file_name,file_id,idlist,db_readme,dbname)


#*********************************************************************************
# Traitement des données FGD ENFANTS SCOLARISES 13-16 ANS [quali]
#*********************************************************************************
database_name <- "es_1316" # names(databases)
db_raw <- databases[[database_name]]  #%>% # variable de stockage des donnees brutes
#renomme les nom trop long
# dm::dm_rename_tbl(consequences_pro=consequences_professionnelle)

# # Identification des noms trop long pour les noms d'onglet des feuilles excels
# [pas plus de 25 caracteres]
# names(db_raw) %>% purrr::set_names() %>%
#   imap( ~ tibble(name=.y,size=nchar(.x))) %>%
#   bind_rows() %>% filter(size>=25)

db_clean <- list() # variable de stockage des resultats des traitements

# (1) Traitement de la principale structure
# glimpse(db_raw %>% dm::dm_zoom_to(main))
db <- db_raw$main %>% 
  select(-any_of(param$varname_todelete)) %>% 
  select(-c(enum_name,nom_agent,sprintf("attachment%d",1:3),
            key01,key02,key_note,p_count_warning,defis_count,
            protection_count,defis_filles_count,suggestion_count)) %>% 
  janitor::clean_names(case="none") %>% 
  rename(form_id=xform_id_string,comment=attachment0,q08b_3=q32b_3) %>% 
  mutate(
    start = lubridate::as_datetime(start),
    end = lubridate::as_datetime(end),
    today=lubridate::as_date(today),
    submission_time= lubridate::as_datetime(submission_time),
    validation_status=to_character(validation_status),
    # poste_travail = str_to_upper(poste_travail),
    # telephone = ifelse(telephone=='000','',telephone),
    enum_id = parse_number(enum_id),
    ecole0 = as.character(ecole_sample0) %>% tidyr::replace_na(""),
    ecole1 = as.character(ecole_sample1) %>% tidyr::replace_na(""),
    schoolID = ifelse(readr::parse_number(ecole_sample0_estremplace)==1,as.character(ecole_sample1),
                      as.character(ecole_sample0)),
    id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID)),
    ecole_sample0 = to_character(ecole_sample0),
    ecole_sample1 = to_character(ecole_sample1)
    # province1_lab = ifelse(province_1==1,"ITURI",''),
    # province2_lab = ifelse(province_2==1,"KASAI-CENTRAL",''),
    # province3_lab = ifelse(province_3==1,"KASAI-ORIENTAL",''),
    # province4_lab = ifelse(province_4==1,"NORD-KIVU",''),
    # province5_lab = ifelse(province_5==1,"SUD-KIVU",''),
    # province6_lab = ifelse(province_6==1,"TANGANYIKA",''),
    # province = str_trim(str_c(province1_lab,province2_lab,province3_lab,
    #                           province4_lab,province5_lab,province6_lab,
    #                           sep=" ",collapse = NULL))
  ) %>% 
  # select(-any_of(sprintf("province%d_lab",1:6))) %>% 
  # On ne conserve que les données correspondant à la période d’enquête
  filter(start>=param$survey_startdate,end<=param$survey_enddate) %>% 
  # Correction de la structure des donnees afin de faciliter l'export des resultats
  correcting.dataStructure() %>% 
  # set_variable_labels(
  #   .labels = var_label(db_raw$main) %>% janitor::clean_names(case="none") %>% unlist() ,
  #   .strict = FALSE
  # ) %>% 
  set_variable_labels(
    start = "Date d'ouverture du questionnaire",
    end = "Date de clôture du questionnaire",
    today = "Jour de collecte",
    hhid = "Identifiant observation",
    comment = "Compte rendu de l'entretien",
    # poste_travail = "Poste de travail",
    # telephone = "Numéro de téléphone de la source d’informations",
    enum_id = "Identifiant agent enquêteur",
    form_id = "Identifiant formulaire",
    index = "Clé primaire de la table",
    id = "Numéro séquentielle",
    uuid="Numéro de série",
    submission_time="Date de soumission",
    validation_status="Statut de validation du questionnaire",
    version="Idenfiant de la version du formulaire",
    territoire = "Territoires couverts",
    ecole0="Nom de l’établissement issu de l'échantillon primaire",
    ecole_sample0="Nom de l’établissement issu de l'échantillon primaire",
    ecole1="Nom de l’établissement issu de l'échantillon de remplacement",
    ecole_sample1="Nom de l’établissement issu de l'échantillon de remplacement",
    schoolID ="Identifant applicatif de l'école enquêté",
    id_ecole="Identifant de l'école enquêté"#,
    # province = "Provinces couvertes",
    # province_1="Province couverte :: ITURI",
    # province_2="Province couverte :: KASAI-CENTRAL",
    # province_3="Province couverte :: KASAI-ORIENTAL",
    # province_4="Province couverte :: NORD-KIVU",
    # province_5="Province couverte :: SUD-KIVU",
    # province_6="Province couverte :: TANGANYIKA"
  )
# Sauvegarde le traitement dans la base de resultat qui sera export
db_clean$main <- db 

# (2) Traitement en boucle des bases de donnees  issus des boucles repeat
dbnames_list <- names(db_raw)[-1]
dbnames_params <- list(
  defis=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(defis))
    label = "LISTES DES DIFFICULTES EN MATIERE D'EDUCATION",
    new_name = "diff_educ", #db_clean
    varnames_select = c("idx_q01a","q01a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q01a="Numéro sequentiel",
      q01a = "Difficulté rencontrée en matière d'éducation",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q01a = "{str_to_upper(q01a)}"
    )
  ),
  recreation=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(recreation))
    label = "LISTE DES MOTIFS ASSOCIES AU NON RECOURS DE LA COUR COMME ESPACE DE JEUX",
    new_name = "no_use_playground", #db_clean
    varnames_select = c("idx_q02c","q02c","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q02c="Numéro sequentiel",
      q02c = "Motif du non recours à la cour comme espace de jeux",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q02c = "{str_to_upper(q02c)}"
    )
  ),
  latrine=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(latrine))
    label = "LISTE DES MOTIFS EN LIEN AVEC LA NON-UTILISATION DES LATRINES PAR LES ENFANTS",
    new_name = "no_use_latrine", #db_clean
    varnames_select = c("idx_q03c","q03c","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q03c="Numéro sequentiel",
      q03c = "Motif de la non-utilisation des latrines par les enfants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q03c = "{str_to_upper(q03c)}"
    )
  ),
  vulnerables=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(vulnerables))
    label = "LISTE DES CATEGORIES D'ENFANTS VULNERABLES ABSENTS DE L'ECOLE ET LES PRINCIPAUX MOTIFS D'ABSENCE",
    new_name = "motifs_absence", #db_clean
    varnames_select = c("idx_q04a","q04a","q04b1","q04b2","q04b3",
                        "parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q04a="Numéro sequentiel",
      q04a = "Groupe d'enfants souvent absent à l'école",
      q04b1 = "Premier motif d'absence",
      q04b2 = "Deuxième motif d'absence",
      q04b3 = "Troisième motif d'absence",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q04a = "{str_to_upper(q04a)}",
      q04b1 = "{str_to_upper(replace_na(q04b1,''))}",
      q04b2 = "{str_to_upper(replace_na(q04b2,''))}",
      q04b3 = "{str_to_upper(replace_na(q04b3,''))}"
    )
  ),
  protection_pb=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(protection_pb))
    label = "LISTE DES PROBLEMES DE PROTECTION A L'ECOLE OU SUR LE CHEMIN DE L'ECOLE",
    new_name = "probleme_protection", #db_clean
    varnames_select = c("idx_q05a","q05a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q05a="Numéro sequentiel",
      q05a = "Problème de protection à l'école ou sur le chemin de l'école",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q05a = "{str_to_upper(q05a)}"
    )
  ),
  gender_pb=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(gender_pb))
    label = "LISTES DES MOTIFS POUR LESQUELS LES GARCONS ONT PLUS LE DROIT D'ETRE SCOLARISE QUE LES FILLES",
    new_name = "gender_problems", #db_clean
    varnames_select = c("idx_q06a","q06a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q06a="Numéro sequentiel",
      q06a = "Motif pour scolariser davantage les garçons que les filles",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q06a = "{str_to_upper(q06a)}"
    )
  ),
  defis_filles=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(defis_filles))
    label = "LISTE DES DIFFICULTES RENCONTREES PAR LES FILLES A L'ECOLE",
    new_name = "defis_filles", #db_clean
    varnames_select = c("idx_q07a","q07a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q07a="Numéro sequentiel",
      q07a = "Difficulté rencontrées par les filles à l'école",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q07a = "{str_to_upper(q07a)}"
    )
  ),
  suggestion=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(suggestion))
    label = "LISTE DES SUGGESTIONS POUR AMELIORER LA SITUATION DE L'EDUCATION OU DE L'ECOLE",
    new_name = "suggestion_amelioration", #db_clean
    varnames_select = c("idx_q08a","q08a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q08a="Numéro sequentiel",
      q08a = "Suggestions d'amélioration de la situation de l'éducation à l'école",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q08a = "{str_to_upper(q08a)}"
    )
  )
)

# Traitement semi-automatique des donnees
for(dbname in dbnames_list){
  infos <- dbnames_params[[dbname]]
  suppressMessages(
    db <- db_raw[[dbname]] %>% 
      janitor::clean_names(case="none") %>% 
      select(all_of(infos$varnames_select)) %>% 
      # (1) traitement sepecifiques des donnees avec la logique dplyr
      mutate(
        across(
          names(infos$treatment_list),
          ~ str_glue(infos$treatment_list[cur_column()]))
      ) %>% 
      # Conversion en type de donnees standard afin que set_variable_label
      # fonctionne correctement
      type_convert() %>% 
      # (2) Etiquettage 
      labelled::set_variable_labels(.labels = infos$varnames_set_labs)
  )
  # stockage de la  base de donnees traitees
  db_clean[[infos$new_name]] <- db
}

# Liste des identifiants 
idlist <- db_clean$main %>% 
  filter(validation_status!=param$STATUT_NOT_APPROVED) %>% 
  pull(index)
# On ne conserve que les donnnees approuvees
db_clean <- db_clean %>% map(
  function(.x){
    if('parent_index' %in% names(.x)){
      .x %>% filter(parent_index %in% idlist)
    }else{
      .x
    } 
  }
)

# (3) Export des donnees au format excel
file_name <- with(param$dbname,name[variable==database_name])
file_id <- with(param$dbname,order_id[variable==database_name])

# (3.1) Export des donnees brutes
dico <- list()
for(dbname in names(db_raw)){
  db <- db_raw[[dbname]] %>% 
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>% 
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=db_raw %>% as.list(),
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)#,colWidths="auto"

# (3.2) Export des donnees dites nettoyees
# Construction d'un dataset readme pour informer sur la nature de chaque onglet
db_readme <- tibble(database="main",description="BASE DE DONNEES PRINCIPALES") %>% 
  bind_rows(
    dbnames_params %>% names() %>% 
      # set_names() %>% 
      map_dfr( ~ dbnames_params[[.x]][c("new_name","label")] %>% as_tibble() %>% distinct()) %>% 
      rename(database=new_name,description=label)
  )

dico <- list()
#Ajout du dictionnaire Globale... celui per
dico$readme <- db_readme
# Extraction des dictionnaires associees a chaque table relationnelle
for(dbname in names(db_clean)){
  infos <- dbnames_params[[dbname]]
  db <- db_clean[[dbname]] %>% 
    # labelled::set_variable_labels(.labels = infos$varnames_set_labs) %>% [delete all]
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>%
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DICO.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=c(readme=list(db_readme),db_clean %>% as.list() %>% map(~ to_factor(.x))),
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DATA.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE,colWidths="auto"
)

# {MOUTE}
rm(db,db_raw,db_clean,dbnames_list,dbnames_params,infos,dico,database_name,
   file_name,file_id,idlist,db_readme,dbname)

#*********************************************************************************
# Traitement des données FGD PARENTS D’ÉLEVES ET MEMBRES APE/COGES [quali]
#*********************************************************************************
database_name <- "parent_es" # names(databases)
db_raw <- databases[[database_name]]  #%>% # variable de stockage des donnees brutes
#renomme les nom trop long
# dm::dm_rename_tbl(consequences_pro=consequences_professionnelle)

# # Identification des noms trop long pour les noms d'onglet des feuilles excels
# [pas plus de 25 caracteres]
# names(db_raw) %>% purrr::set_names() %>%
#   imap( ~ tibble(name=.y,size=nchar(.x))) %>%
#   bind_rows() %>% filter(size>=25)

db_clean <- list() # variable de stockage des resultats des traitements

# (1) Traitement de la principale structure
# glimpse(db_raw %>% dm::dm_zoom_to(main))
db <- db_raw$main %>% 
  select(-any_of(param$varname_todelete)) %>% 
  select(-c(enum_name,nom_agent,sprintf("attachment%d",1:3),
            key01,key02,key_note,p_count_warning,difficultes_count,
            propositions_count)) %>% 
  janitor::clean_names(case="none") %>% 
  rename(form_id=xform_id_string,comment=attachment0) %>% 
  mutate(
    start = lubridate::as_datetime(start),
    end = lubridate::as_datetime(end),
    today=lubridate::as_date(today),
    submission_time= lubridate::as_datetime(submission_time),
    validation_status=to_character(validation_status),
    # poste_travail = str_to_upper(poste_travail),
    # telephone = ifelse(telephone=='000','',telephone),
    enum_id = parse_number(enum_id),
    ecole0 = as.character(ecole_sample0) %>% tidyr::replace_na(""),
    ecole1 = as.character(ecole_sample1) %>% tidyr::replace_na(""),
    schoolID = ifelse(readr::parse_number(ecole_sample0_estremplace)==1,as.character(ecole_sample1),
                      as.character(ecole_sample0)),
    id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID)),
    ecole_sample0 = to_character(ecole_sample0),
    ecole_sample1 = to_character(ecole_sample1)
    # province1_lab = ifelse(province_1==1,"ITURI",''),
    # province2_lab = ifelse(province_2==1,"KASAI-CENTRAL",''),
    # province3_lab = ifelse(province_3==1,"KASAI-ORIENTAL",''),
    # province4_lab = ifelse(province_4==1,"NORD-KIVU",''),
    # province5_lab = ifelse(province_5==1,"SUD-KIVU",''),
    # province6_lab = ifelse(province_6==1,"TANGANYIKA",''),
    # province = str_trim(str_c(province1_lab,province2_lab,province3_lab,
    #                           province4_lab,province5_lab,province6_lab,
    #                           sep=" ",collapse = NULL))
  ) %>% 
  # select(-any_of(sprintf("province%d_lab",1:6))) %>% 
  # On ne conserve que les données correspondant à la période d’enquête
  filter(start>=param$survey_startdate,end<=param$survey_enddate) %>% 
  # Correction de la structure des donnees afin de faciliter l'export des resultats
  correcting.dataStructure() %>% 
  # set_variable_labels(
  #   .labels = var_label(db_raw$main) %>% janitor::clean_names(case="none") %>% unlist() ,
  #   .strict = FALSE
  # ) %>% 
  set_variable_labels(
    start = "Date d'ouverture du questionnaire",
    end = "Date de clôture du questionnaire",
    today = "Jour de collecte",
    hhid = "Identifiant observation",
    comment = "Compte rendu de l'entretien",
    # poste_travail = "Poste de travail",
    # telephone = "Numéro de téléphone de la source d’informations",
    enum_id = "Identifiant agent enquêteur",
    form_id = "Identifiant formulaire",
    index = "Clé primaire de la table",
    id = "Numéro séquentielle",
    uuid="Numéro de série",
    submission_time="Date de soumission",
    validation_status="Statut de validation du questionnaire",
    version="Idenfiant de la version du formulaire",
    territoire = "Territoires couverts",
    ecole0="Nom de l’établissement issu de l'échantillon primaire",
    ecole_sample0="Nom de l’établissement issu de l'échantillon primaire",
    ecole1="Nom de l’établissement issu de l'échantillon de remplacement",
    ecole_sample1="Nom de l’établissement issu de l'échantillon de remplacement",
    schoolID ="Identifant applicatif de l'école enquêté",
    id_ecole="Identifant de l'école enquêté"#,
    # province = "Provinces couvertes",
    # province_1="Province couverte :: ITURI",
    # province_2="Province couverte :: KASAI-CENTRAL",
    # province_3="Province couverte :: KASAI-ORIENTAL",
    # province_4="Province couverte :: NORD-KIVU",
    # province_5="Province couverte :: SUD-KIVU",
    # province_6="Province couverte :: TANGANYIKA"
  )
# Sauvegarde le traitement dans la base de resultat qui sera export
db_clean$main <- db 

# (2) Traitement en boucle des bases de donnees  issus des boucles repeat
dbnames_list <- names(db_raw)[-1]
dbnames_params <- list(
  evt_list=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(evt_list))
    label = "LISTE DES EVENEMENTS AYANT AFFECTES LE FONCTIONNEMENT DE L'ECOLE",
    new_name = "evt_list", #db_clean
    varnames_select = c("idx_q01a","q01a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q01a="Numéro sequentiel",
      q01a = "Evénement ayant affecté le fonctionnement de l'école",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q01a = "{str_to_upper(q01a)}"
    )
  ),
  evt_consequences=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(evt_consequences))
    label = "LISTES DES CONSEQUENCES ASSOCIES AUX DIFFERENTS EVENEMTS",
    new_name = "evt_consequences", #db_clean
    varnames_select = c("idx_q01b","q01b","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q01b="Numéro sequentiel",
      q01b = "Conséquence associée",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q01b = "{str_to_upper(q01b)}"
    )
  ),
  evt_impact=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(evt_impact))
    label = "LISTE DES IMPACTS ASSOCIES AUX DIFFERENTS EVENEMENTS",
    new_name = "evt_impact", #db_clean
    varnames_select = c("idx_q01c","q01c","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q01c="Numéro sequentiel",
      q01c = "Impact associé",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q01c = "{str_to_upper(q01c)}"
    )
  ),
  difficultes=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(difficultes))
    label = "LISTE DES DIFFICULTES RENCONTREES PAR LES ENFANTS A L'ECOLE",
    new_name = "difficultes", #db_clean
    varnames_select = c("idx_q02a","q02a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q02a="Numéro sequentiel",
      q02a = "Difficulté des enfants à l'école",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q02a = "{str_to_upper(q02a)}"
    )
  ),
  implication_parents=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(implication_parents))
    label = "LISTE DES DIFFERENTES IMPLICATIONS DES PARENTS DANS LE RECRUTEMENT DES ENSEIGNANTS",
    new_name = "implication_parents", #db_clean
    varnames_select = c("idx_q03a","q03a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q03a="Numéro sequentiel",
      q03a = "Implication des parents dans le recrutement des enseignants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q03a = "{str_to_upper(q03a)}"
    )
  ),
  action_communaute=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(action_communaute))
    label = "LISTE DES ACTIONS MENEES PAR LA COMMUNAUTE",
    new_name = "action_communaute", #db_clean
    varnames_select = c("idx_q04a","q04a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q04a="Numéro sequentiel",
      q04a = "Action des membres de la communauté en faveur des écoles",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q04a = "{str_to_upper(q04a)}"
    )
  ),
  decisions_copa=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(decisions_copa))
    label = "LISTE DES DECISIONS DE GESTION SCOLAIRE EN LIEN AVEC COPA/COGES",
    new_name = "decisions_copa", #db_clean
    varnames_select = c("idx_q05a","q05a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q05a="Numéro sequentiel",
      q05a = "Decisions de gestion scolaire où COPA/COGES est impliqué",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q05a = "{str_to_upper(q05a)}"
    )
  ),
  pb_protection_fille=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(pb_protection_fille))
    label = "LISTE DES PROBLEMES DE PROTECTION POUR LES FILLES A L'ECOLE OU SUR LE CHEMIN DE L'ECOLE",
    new_name = "pb_protection_fille", #db_clean
    varnames_select = c("idx_q06b","q06b","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q06b="Numéro sequentiel",
      q06b = "Problème de protection pour les filles à l'école ou sur le chemin de l'école",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q06b = "{str_to_upper(q06b)}"
    )
  ),
  pb_protection_garcon=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(pb_protection_garcon))
    label = "LISTE DES PROBLEMES DE PROTECTION POUR LES GARCONS A L'ECOLE OU SUR LE CHEMIN DE L'ECOLE",
    new_name = "pb_protection_garcon", #db_clean
    varnames_select = c("idx_q06d","q06d","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q06d="Numéro sequentiel",
      q06d = "Problème de protection pour les garçons à l'école ou sur le chemin de l'école",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q06d = "{str_to_upper(q06d)}"
    )
  ),
  actions_faites=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(actions_faites))
    label = "LISTE DES ACTIONS EN FAVEUR DE LA SECURITE DES ENFANTS",
    new_name = "mesures_secutite_enft", #db_clean
    varnames_select = c("idx_q07a","q07a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q07a="Numéro sequentiel",
      q07a = "Mesure de sécurité pour les enfants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q07a = "{str_to_upper(q07a)}"
    )
  ),
  actions_a_faire=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(actions_a_faire))
    label = "LISTES DES EFFORTS SUPPLEMENTAIRES POUR LA SECURITE DES ENFANTS",
    new_name = "actions_a_faire", #db_clean
    varnames_select = c("idx_q07b","q07b","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q07b="Numéro sequentiel",
      q07b = "Action supplémentaire à conduire pour la sécurité des enfants à l'école et sur le chemin de l'école",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q07b = "{str_to_upper(q07b)}"
    )
  ),
  cat_enft_abuse=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(cat_enft_abuse))
    label = "CATEGORIES DES ENFANTS SUSCEPTIBLES D'ETRE VICTIME D'ABUS",
    new_name = "cat_enft_abuse", #db_clean
    varnames_select = c("idx_q08b","q08b","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q08b="Numéro sequentiel",
      q08b = "Type d'enfants souvent victimes d'abus ou d'harcélement",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q08b = "{str_to_upper(q08b)}"
    )
  ),
  lieu_signalisation_abus=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(lieu_signalisation_abus))
    label = "LISTE DES LIEUX DE SIGNALISATION DES CAS D'ABUS OU DE VIOLENCE SUR LES ENFANTS",
    new_name = "lieu_signalisation_abus", #db_clean
    varnames_select = c("idx_q09b","q09b","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q09b="Numéro sequentiel",
      q09b = "Lieu de signalisation des cas d'abus ou de violence sur les enfants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q09b = "{str_to_upper(q09b)}"
    )
  ),
  handicap_difficultes=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(handicap_difficultes))
    label = "LISTE DES DIFFICULTES RENCONTREES PAR LES ENFANTS EN SITUATION DE HANDICAP",
    new_name = "handicap_difficultes", #db_clean
    varnames_select = c("idx_q10b","q10b","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q10b="Numéro sequentiel",
      q10b = "Difficulté rencontrée par les enfants en situation de handicap",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q10b = "{str_to_upper(q10b)}"
    )
  ),
  propositions=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(propositions))
    label = "LISTE DE PROPOSITIONS D'AMELIORATION DE L'EDUCATION AU SEIN DE L'ECOLE",
    new_name = "proposition_amelioration", #db_clean
    varnames_select = c("idx_q11a","q11a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q11a="Numéro sequentiel",
      q11a = "Proposition d'amélioration de l'éducation au sein de l'école",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q11a = "{str_to_upper(q11a)}"
    )
  )
)

# Traitement semi-automatique des donnees
for(dbname in dbnames_list){
  infos <- dbnames_params[[dbname]]
  suppressMessages(
    db <- db_raw[[dbname]] %>% 
      janitor::clean_names(case="none") %>% 
      select(all_of(infos$varnames_select)) %>% 
      # (1) traitement sepecifiques des donnees avec la logique dplyr
      mutate(
        across(
          names(infos$treatment_list),
          ~ str_glue(infos$treatment_list[cur_column()]))
      ) %>% 
      # Conversion en type de donnees standard afin que set_variable_label
      # fonctionne correctement
      type_convert() %>% 
      # (2) Etiquettage 
      labelled::set_variable_labels(.labels = infos$varnames_set_labs)
  )
  # stockage de la  base de donnees traitees
  db_clean[[infos$new_name]] <- db
}

# Liste des identifiants 
idlist <- db_clean$main %>% 
  filter(validation_status!=param$STATUT_NOT_APPROVED) %>% 
  pull(index)
# On ne conserve que les donnnees approuvees
db_clean <- db_clean %>% map(
  function(.x){
    if('parent_index' %in% names(.x)){
      .x %>% filter(parent_index %in% idlist)
    }else{
      .x
    } 
  }
)

# (3) Export des donnees au format excel
file_name <- with(param$dbname,
                  str_replace_all(name[variable==database_name],
                                  pattern = "'|/",replacement = " "))
file_id <- with(param$dbname,order_id[variable==database_name])

# (3.1) Export des donnees brutes
dico <- list()
for(dbname in names(db_raw)){
  db <- db_raw[[dbname]] %>% 
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>% 
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=db_raw %>% as.list(),
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)#,colWidths="auto"

# (3.2) Export des donnees dites nettoyees
# Construction d'un dataset readme pour informer sur la nature de chaque onglet
db_readme <- tibble(database="main",description="BASE DE DONNEES PRINCIPALES") %>% 
  bind_rows(
    dbnames_params %>% names() %>% 
      # set_names() %>% 
      map_dfr( ~ dbnames_params[[.x]][c("new_name","label")] %>% as_tibble() %>% distinct()) %>% 
      rename(database=new_name,description=label)
  )

dico <- list()
#Ajout du dictionnaire Globale... celui per
dico$readme <- db_readme
# Extraction des dictionnaires associees a chaque table relationnelle
for(dbname in names(db_clean)){
  infos <- dbnames_params[[dbname]]
  db <- db_clean[[dbname]] %>% 
    # labelled::set_variable_labels(.labels = infos$varnames_set_labs) %>% [delete all]
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>%
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DICO.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=c(readme=list(db_readme),db_clean %>% as.list() %>% map(~ to_factor(.x))),
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DATA.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE,colWidths="auto"
)

rm(db,db_raw,db_clean,dbnames_list,dbnames_params,infos,dico,database_name,
   file_name,file_id,idlist,db_readme,dbname)


#*********************************************************************************
# Traitement des données  PARENTS D'ENFANTS NON SCOLARISES [quali]
#*********************************************************************************
database_name <- "parent_ens" # names(databases)
db_raw <- databases[[database_name]]  #%>% # variable de stockage des donnees brutes
#renomme les nom trop long
# dm::dm_rename_tbl(consequences_pro=consequences_professionnelle)

# # Identification des noms trop long pour les noms d'onglet des feuilles excels
# [pas plus de 25 caracteres]
# names(db_raw) %>% purrr::set_names() %>%
#   imap( ~ tibble(name=.y,size=nchar(.x))) %>%
#   bind_rows() %>% filter(size>=25)

db_clean <- list() # variable de stockage des resultats des traitements

# (1) Traitement de la principale structure
# glimpse(db_raw %>% dm::dm_zoom_to(main))
db <- db_raw$main %>% 
  select(-any_of(param$varname_todelete)) %>% 
  select(-c(enum_name,nom_agent,sprintf("attachment%d",1:3),
            key01,key02,key_note,p_count_warning,q01a_count,
            q02a_count,q04a_count,raisons_nonfrequentation_count,
            actions_count,activites_count)) %>% 
  janitor::clean_names(case="none") %>% 
  rename(form_id=xform_id_string,comment=attachment0) %>% 
  mutate(
    start = lubridate::as_datetime(start),
    end = lubridate::as_datetime(end),
    today=lubridate::as_date(today),
    submission_time= lubridate::as_datetime(submission_time),
    validation_status=to_character(validation_status),
    # poste_travail = str_to_upper(poste_travail),
    # telephone = ifelse(telephone=='000','',telephone),
    enum_id = parse_number(enum_id),
    ecole0 = as.character(ecole_sample0) %>% tidyr::replace_na(""),
    ecole1 = as.character(ecole_sample1) %>% tidyr::replace_na(""),
    schoolID = ifelse(readr::parse_number(ecole_sample0_estremplace)==1,as.character(ecole_sample1),
                      as.character(ecole_sample0)),
    id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID)),
    ecole_sample0 = to_character(ecole_sample0),
    ecole_sample1 = to_character(ecole_sample1)
    # province1_lab = ifelse(province_1==1,"ITURI",''),
    # province2_lab = ifelse(province_2==1,"KASAI-CENTRAL",''),
    # province3_lab = ifelse(province_3==1,"KASAI-ORIENTAL",''),
    # province4_lab = ifelse(province_4==1,"NORD-KIVU",''),
    # province5_lab = ifelse(province_5==1,"SUD-KIVU",''),
    # province6_lab = ifelse(province_6==1,"TANGANYIKA",''),
    # province = str_trim(str_c(province1_lab,province2_lab,province3_lab,
    #                           province4_lab,province5_lab,province6_lab,
    #                           sep=" ",collapse = NULL))
  ) %>% 
  # select(-any_of(sprintf("province%d_lab",1:6))) %>% 
  # On ne conserve que les données correspondant à la période d’enquête
  filter(start>=param$survey_startdate,end<=param$survey_enddate) %>% 
  # Correction de la structure des donnees afin de faciliter l'export des resultats
  correcting.dataStructure() %>% 
  # set_variable_labels(
  #   .labels = var_label(db_raw$main) %>% janitor::clean_names(case="none") %>% unlist() ,
  #   .strict = FALSE
  # ) %>% 
  set_variable_labels(
    start = "Date d'ouverture du questionnaire",
    end = "Date de clôture du questionnaire",
    today = "Jour de collecte",
    hhid = "Identifiant observation",
    comment = "Compte rendu de l'entretien",
    # poste_travail = "Poste de travail",
    # telephone = "Numéro de téléphone de la source d’informations",
    enum_id = "Identifiant agent enquêteur",
    form_id = "Identifiant formulaire",
    index = "Clé primaire de la table",
    id = "Numéro séquentielle",
    uuid="Numéro de série",
    submission_time="Date de soumission",
    validation_status="Statut de validation du questionnaire",
    version="Idenfiant de la version du formulaire",
    territoire = "Territoires couverts",
    ecole0="Nom de l’établissement issu de l'échantillon primaire",
    ecole_sample0="Nom de l’établissement issu de l'échantillon primaire",
    ecole1="Nom de l’établissement issu de l'échantillon de remplacement",
    ecole_sample1="Nom de l’établissement issu de l'échantillon de remplacement",
    schoolID ="Identifant applicatif de l'école enquêté",
    id_ecole="Identifant de l'école enquêté"#,
    # province = "Provinces couvertes",
    # province_1="Province couverte :: ITURI",
    # province_2="Province couverte :: KASAI-CENTRAL",
    # province_3="Province couverte :: KASAI-ORIENTAL",
    # province_4="Province couverte :: NORD-KIVU",
    # province_5="Province couverte :: SUD-KIVU",
    # province_6="Province couverte :: TANGANYIKA"
  )
# Sauvegarde le traitement dans la base de resultat qui sera export
db_clean$main <- db 

# (2) Traitement en boucle des bases de donnees  issus des boucles repeat
dbnames_list <- names(db_raw)[-1]
dbnames_params <- list(
  raisons_nonfrequentation=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(raisons_nonfrequentation))
    label = "LISTES DES MOTIFS DE LA NON FREQUENTATION SCOLAIRE DES ENFANTS",
    new_name = "raisons_nonfrequentation", #db_clean
    varnames_select = c("idx_q01a","q01a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q01a="Numéro sequentiel",
      q01a = "Motif de la non fréquentation scolaire des enfants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q01a = "{str_to_upper(q01a)}"
    )
  ),
  actions=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(actions))
    label = "Interventions en faveur de la scolarisation des enfants",
    new_name = "actions", #db_clean
    varnames_select = c("idx_q02a","q02a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q02a="Numéro sequentiel",
      q02a = "Intervention pour la scolarisation des enfants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q02a = "{str_to_upper(q02a)}"
    )
  ),
  initiatives=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(initiatives))
    label = "LISTE DES INITIATIVES EN FAVEUR DES ACTIVITES EDUCATIVES",
    new_name = "initiatives", #db_clean
    varnames_select = c("idx_q03a","q03a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q03a="Numéro sequentiel",
      q03a = "Initiative en faveur des activités éducatives",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q03a = "{str_to_upper(q03a)}"
    )
  ),
  activites=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(activites))
    label = "LISTE DES ACTIVITES QUOTIDIENNES",
    new_name = "activites", #db_clean
    varnames_select = c("idx_q04a","q04a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q04a="Numéro sequentiel",
      q04a = "Activité quotidienne",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q04a = "{str_to_upper(q04a)}"
    )
  ),
  protection=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(protection))
    label = "LISTE DES PROBLEMES D'INSECURITE",
    new_name = "pb_protection", #db_clean
    varnames_select = c("idx_q05a","q05a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q05a="Numéro sequentiel",
      q05a = "Problème d'insécurité",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q05a = "{str_to_upper(q05a)}"
    )
  ),
  institution=list(# names(db_raw); glimpse(db_raw %>% dm::dm_zoom_to(institution))
    label = "LISTE DES INSTITUTIONS EN CAS D'ABUS OU DE VIOLENCE ENVERS DES ENFANTS",
    new_name = "institution", #db_clean
    varnames_select = c("idx_q06a","q06a","parent_table_name","parent_index","index"),
    varnames_set_labs = c(
      idx_q06a="Numéro sequentiel",
      q06a = "Institution en cas d'abus ou de violence envers des enfants",
      parent_table_name = "Nom de la table parent",
      parent_index="Clé primaire de la table parent",
      index ="Clé primaire de la table"
    ),
    treatment_list = c(
      q06a = "{str_to_upper(q06a)}"
    )
  )
)

# Traitement semi-automatique des donnees
for(dbname in dbnames_list){
  infos <- dbnames_params[[dbname]]
  suppressMessages(
    db <- db_raw[[dbname]] %>% 
      janitor::clean_names(case="none") %>% 
      select(all_of(infos$varnames_select)) %>% 
      # (1) traitement sepecifiques des donnees avec la logique dplyr
      mutate(
        across(
          names(infos$treatment_list),
          ~ str_glue(infos$treatment_list[cur_column()]))
      ) %>% 
      # Conversion en type de donnees standard afin que set_variable_label
      # fonctionne correctement
      type_convert() %>% 
      # (2) Etiquettage 
      labelled::set_variable_labels(.labels = infos$varnames_set_labs)
  )
  # stockage de la  base de donnees traitees
  db_clean[[infos$new_name]] <- db
}

# Liste des identifiants 
idlist <- db_clean$main %>% 
  filter(validation_status!=param$STATUT_NOT_APPROVED) %>% 
  pull(index)
# On ne conserve que les donnnees approuvees
db_clean <- db_clean %>% map(
  function(.x){
    if('parent_index' %in% names(.x)){
      .x %>% filter(parent_index %in% idlist)
    }else{
      .x
    } 
  }
)

# (3) Export des donnees au format excel
file_name <- with(param$dbname,
                  str_replace_all(name[variable==database_name],
                                  pattern = "'|/",replacement = " "))
file_id <- with(param$dbname,order_id[variable==database_name])

# (3.1) Export des donnees brutes
dico <- list()
for(dbname in names(db_raw)){
  db <- db_raw[[dbname]] %>% 
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>% 
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=db_raw %>% as.list(),
  file=sprintf("deliverable/01_raw/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)#,colWidths="auto"

# (3.2) Export des donnees dites nettoyees
# Construction d'un dataset readme pour informer sur la nature de chaque onglet
db_readme <- tibble(database="main",description="BASE DE DONNEES PRINCIPALES") %>% 
  bind_rows(
    dbnames_params %>% names() %>% 
      # set_names() %>% 
      map_dfr( ~ dbnames_params[[.x]][c("new_name","label")] %>% as_tibble() %>% distinct()) %>% 
      rename(database=new_name,description=label)
  )

dico <- list()
#Ajout du dictionnaire Globale... celui per
dico$readme <- db_readme
# Extraction des dictionnaires associees a chaque table relationnelle
for(dbname in names(db_clean)){
  infos <- dbnames_params[[dbname]]
  db <- db_clean[[dbname]] %>% 
    # labelled::set_variable_labels(.labels = infos$varnames_set_labs) %>% [delete all]
    labelled::generate_dictionary(details = "basic") %>%
    labelled::lookfor_to_long_format() %>%
    mutate(
      col_type = case_when(
        col_type=='dttm' ~ "date",
        col_type=='chr+lbl' ~ "dbl+lbl",
        col_type=='lgl' ~ "chr",
        .default = col_type
      ),
      values = ifelse(
        !is.na(value_labels),
        str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
        levels
      ),
      labels=ifelse(
        !is.na(value_labels),
        str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
        value_labels
      )
    ) 
  dico[[dbname]] <- db
}

# Dico export
rio::export(
  x=dico,
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DICO.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DICO.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE#,colWidths="auto"
)

# Data export
rio::export(
  x=c(readme=list(db_readme),db_clean %>% as.list() %>% map(~ to_factor(.x))),
  # file=sprintf("deliverable/02_clean/quali/%02d_%s_DATA.xlsx",file_id,file_name),
  file=sprintf("deliverable/02_clean/quali/%s_DATA.xlsx",file_name),
  asTable = TRUE,creator="Charles Mouté",firstRow=TRUE,keepNA=FALSE,colWidths="auto"
)

rm(db,db_raw,db_clean,dbnames_list,dbnames_params,infos,dico,database_name,
   file_name,file_id,idlist,db_readme,dbname)

datasets <- list()

#*********************************************************************************
# Traitement des données QUESTIONNAIRE ECOLE [quanti]
#*********************************************************************************

# formid <- param$dbname$uid[2]

#..............................................................................
# Export et traitement des donnees que l'on considere brute
#..............................................................................
# db_raw <- kobo_data(kobo_asset(formid),progress=TRUE) %>%
db_raw <- databases$ecole %>%
  select(-c(deviceid:survey_year,nom_agent,key_note,consent_msg,q102_warning, 
            crise_01:crise_04,q201_warning,key01,key02,`_uuid`,rootUuid,deprecatedID,`_status`)) %>% 
  rename(
    nom_agent=fullname,id=`_id`,version=`__version__`,form_id=`_xform_id_string`,#status=`_status`,
    submission_time=`_submission_time`,validation_status=`_validation_status`,submitted_by=`_submitted_by`
  ) %>% 
  mutate(
    start = lubridate::as_datetime(start),
    end = lubridate::as_datetime(end),
    today=lubridate::as_date(today),
    submission_time= lubridate::as_datetime(submission_time),
    # On se rassure de ne pas aovoir des couacs lors de l'export au format stata
    # ou autre
    ecole0 = as.character(ecole_sample0) %>% tidyr::replace_na(""),
    ecole1 = as.character(ecole_sample1) %>% tidyr::replace_na(""),
    schoolID = ifelse(readr::parse_number(ecole_sample0_estremplace)==1,as.character(ecole_sample1),
                      as.character(ecole_sample0)),
    id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID)),
    ecole_sample0 = to_character(ecole_sample0),
    ecole_sample1 = to_character(ecole_sample1),
    validation_status=to_character(validation_status),
    consent_comment=to_character(consent_comment) # type logical qui devrait être chaine de caractere
  ) %>% 
  # On ne conserve que les données correspondant à la période d’enquête
  filter(start>=param$survey_startdate,end<=param$survey_enddate) %>% 
  # Correction de la structure des donnees afin de faciliter l'export des resultats
  correcting.dataStructure() %>% 
  # Complétion des noms des variables
  set_variable_labels(
    q102="Nombre de garçons et de filles inscrits dans toutes les classes de l'école",
    q103="Nombre de garçons et de filles présents à l'école ajourd'hui",
    q104_1_count="Nombre de crises survenues entre septembre 2021 et mars 2022 dans la localité",
    q201="Nombre d'enseignants travaillant actuellement dans l'école",
    q202="Nombre d'enseignants qui sont presents et enseignent actuellement à l'école",
    q203="Nombre d'enseignants qui devraient être présents et enseignés dans l'école aujourd'hui",
    q204="Nombre d'enseignants affectés par les situations de crises intervenues de septembre 2021 à Mars 2022 et ne peuvent plus travailler maintenant",
    q207="Nombre d'enseignants qualifiés",
    q208="Nombre d'enseignants non qualitfiés ayant bénéficiés de jours de formations",
    q216="Nombre d'enseignants payés par le Ministère de l’Enseignement Secondaire, Technique et Professionnel",
    q217="Nombre d'enseignants payés par des ONG et autres",
    id="Identifiant Kobo de l'observation",
    version="Version du formulaire",
    form_id="Identifiant Kobo du formulaire",
    submission_time="Date de soumission",
    validation_status="Statut de validation",
    submitted_by="Pseudonyme de l'auteur de la soumission",
    ecole0="Nom de l’établissement issu de l'échantillon primaire",
    ecole1="Nom de l’établissement issu de l'échantillon de remplacement",
    schoolID ="Identifant applicatif de l'école enquêté",
    id_ecole="Identifant de l'école enquêté",
    consent_qst_texte="Principale question posée lors du consentement",
    consent_comment="Commentaire formulée lors du consentement"
  )

# Extraction du dictionnaire des donnees associe aux donnees brutes
dico_raw <- db_raw %>%
  labelled::generate_dictionary(details = "basic") %>%
  labelled::lookfor_to_long_format() %>% 
  mutate(
    col_type = case_when(
      col_type=='dttm' ~ "date",
      col_type=='chr+lbl' ~ "dbl+lbl",
      col_type=='lgl' ~ "chr",
      .default = col_type
    ),
    values = ifelse(
      !is.na(value_labels),
      str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
      levels
    ),
    labels=ifelse(
      !is.na(value_labels),
      str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
      value_labels
    )
  )

# Export des donnees brutes ajustées
rio::export(db_raw,"deliverable/01_raw/quanti/02_Ecoles_DATA.dta","dta")
rio::export(db_raw,"deliverable/01_raw/quanti/02_Ecoles_DATA.xlsx","xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="data",firstRow=TRUE,
            keepNA=FALSE)
rio::export(dico_raw,"deliverable/01_raw/quanti/02_Ecoles_DICO.xlsx","xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)#,colWidths="auto"

# # Extraction des donnees a traiter manuellement
# Le code ci-dessous s'execute une seule fois....
# getData.toBeProcessed(db_raw,
#                       instructions=import("./input/TREATMENT_INSTRUCTIONS.xlsx",setclass="tibble",sheet="school")) %>% 
#   rio::export("./output/JENA_ECOLE_INCONSISTENCES.xlsx",asTable = TRUE,
#          creator="Charles Mouté",sheetName="data",firstRow=TRUE,keepNA=FALSE)

#..............................................................................
# Apurement des donnees ecoles
#..............................................................................

# Ajustement de la structure des donnees
db_clean <-  db_raw %>% 
  # Suppression des variables jugees inutiles
  select(-c(
    nom_directeur,source_nom,source_telephone,enum_name
  )) %>% 
  filter(
    # On supprime les donnees non approuvees ainsi que les observations en lien
    # avec un refus de participation
    validation_status!=param$STATUT_NOT_APPROVED,
    consent_value==1
  ) %>% 
  # suppression automatique des doublons sur les variables d'identification
  # On travaille sur les données les plus récentes, pour éviter de supprimer des 
  # données importantes ..
  processing.duplicateData() %>% 
  processing.duplicateData(var_id="id_ecole") %>% 
  mutate(
    enum_id = as.numeric(enum_id),
    # Ajout des variables oubliées ou ajoutés après traitement manuel des données
    q209 = q209_1 + q209_2,
    q109_0=0,q110_1_11=0,q119_1_5=0,q119_1_6=0,
    q119_3_1=NA_integer_,q119_3_2=NA_integer_,q119_3_3=NA_integer_,q119_3_4=NA_integer_,q119_3_5=NA_integer_,
    q160_2a_0=NA_integer_,q160_2a_UNICEF=NA_integer_,q160_2a_SAVETHECHILDREN=NA_integer_,
    q160_2b_0=NA_integer_,q160_2b_UNICEF=NA_integer_,q160_2b_SAVETHECHILDREN=NA_integer_,
    q160_2c_0=NA_integer_,q160_2c_UNICEF=NA_integer_,q160_2c_SAVETHECHILDREN=NA_integer_,
    q160_2d_0=NA_integer_,q160_2d_UNICEF=NA_integer_,q160_2d_SAVETHECHILDREN=NA_integer_
  ) %>% 
  # Complétion des noms des variables
  set_variable_labels(
    enum_id="Identifiant agent de collecte",
    q102="Nombre de garçons et de filles inscrits dans toutes les classes de l'école",
    q103="Nombre de garçons et de filles présents à l'école ajourd'hui",
    q104_1_count="Nombre de crises survenues entre septembre 2021 et mars 2022 dans la localité",
    q109_0="1.09. Des enfants de groupes vulnérables ou à risque (tels que ci-dessous définis) fréquentent-ils cette école?::Aucun",
    q110_1_11="1.10.1. Votre école fait-elle des efforts pour aider les élèves à accéder, participer et/ou rester à l’école?::Sensibilisation des parents et/ou des enfants sur l'importance de l'éducation et la fréquentation scolaire",
    q119_1_5="1.19.1. Quels sont les principaux dangers naturels dans la communauté/zone?::Feux de brousse",
    q119_1_6="1.19.1. Quels sont les principaux dangers naturels dans la communauté/zone?::Eruption volcanique",
    q119_3a="1.19.3A. Citez au plus 3 mesures mises en place à l’école pour tenir compte des principaux dangers naturels dans la communauté/zone (1er mesure) ?",
    q119_3b="1.19.3B. Citez au plus 3 mesures mises en place à l’école pour tenir compte des principaux dangers naturels dans la communauté/zone (2e mesure) ?",
    q119_3c="1.19.3C. Citez au plus 3 mesures mises en place à l’école pour tenir compte des principaux dangers naturels dans la communauté/zone (3e mesure) ?",
    q119_3_1="Mesures pour tenir compte des principaux dangers naturels : Reboisement des espaces/Semis d'arbres",
    q119_3_2="Mesures pour tenir compte des principaux dangers naturels : Construction de canaux de canalisations des eaux et/ou d'autres obstacles utiles de même nature (barrière, enclos, clôture …)",
    q119_3_3="Mesures pour tenir compte des principaux dangers naturels : Réhabilitation/Renforcement des infrastructures existantes",
    q119_3_4="Mesures pour tenir compte des principaux dangers naturels : Construction de nouvelles infrastructures",
    q119_3_5="Mesures pour tenir compte des principaux dangers naturels : Sensibilisation sur les catastrophes et/ou les comportements à adopter en cas de catastrophes",
    q160_2a_0="1.60.2A. Quelles sont les agences (ONG/Agences onusiennes, Organisation société civile) qui sont venues ici pour vous parler de la situation de l’éducation ou des besoins de cette école depuis le début des crises dues à des causes naturelles de septembre 2021 à Mars 2022 ?::AUCUNE",
    q160_2a_UNICEF="1.60.2A. Quelles sont les agences (ONG/Agences onusiennes, Organisation société civile) qui sont venues ici pour vous parler de la situation de l’éducation ou des besoins de cette école depuis le début des crises dues à des causes naturelles de septembre 2021 à Mars 2022 ?::UNICEF",
    q160_2a_SAVETHECHILDREN="1.60.2A. Quelles sont les agences (ONG/Agences onusiennes, Organisation société civile) qui sont venues ici pour vous parler de la situation de l’éducation ou des besoins de cette école depuis le début des crises dues à des causes naturelles de septembre 2021 à Mars 2022 ?:: SAVE THE CHILDREN",
    q160_2b_0="1.60.2B. Quelles sont les agences (ONG/Agences onusiennes, Organisation société civile) qui sont venues ici pour vous parler de la situation de l’éducation ou des besoins de cette école depuis le début des crises dues à des conflits armés ou communautaires de septembre 2021 à Mars 2022 ? :: AUCUNE",
    q160_2b_UNICEF="1.60.2B. Quelles sont les agences (ONG/Agences onusiennes, Organisation société civile) qui sont venues ici pour vous parler de la situation de l’éducation ou des besoins de cette école depuis le début des crises dues à des conflits armés ou communautaires de septembre 2021 à Mars 2022 ? :: UNICEF",
    q160_2b_SAVETHECHILDREN="1.60.2B. Quelles sont les agences (ONG/Agences onusiennes, Organisation société civile) qui sont venues ici pour vous parler de la situation de l’éducation ou des besoins de cette école depuis le début des crises dues à des conflits armés ou communautaires de septembre 2021 à Mars 2022 ? ::  THE SAVE CHILDREN",
    q160_2c_0="1.60.2C. Quelles sont les agences (ONG/Agences onusiennes, Organisation société civile) qui sont venues ici pour vous parler de la situation de l’éducation ou des besoins de cette école depuis le début des crises sanitaires de septembre 2021 à Mars 2022 ? :: AUCUNE",
    q160_2c_UNICEF="1.60.2C. Quelles sont les agences (ONG/Agences onusiennes, Organisation société civile) qui sont venues ici pour vous parler de la situation de l’éducation ou des besoins de cette école depuis le début des crises sanitaires de septembre 2021 à Mars 2022 ? :: UNICEF",
    q160_2c_SAVETHECHILDREN="1.60.2C. Quelles sont les agences (ONG/Agences onusiennes, Organisation société civile) qui sont venues ici pour vous parler de la situation de l’éducation ou des besoins de cette école depuis le début des crises sanitaires de septembre 2021 à Mars 2022 ? :: SAVE THE CHILDREN",
    q160_2d_0="1.60.2D. Quelles sont les agences (ONG/Agences onusiennes, Organisation société civile) qui sont venues ici pour vous parler de la situation de l’éducation ou des besoins de cette école depuis le début des crises administratives/sociales de septembre 2021 à Mars 2022 ? :: AUCUNE",
    q160_2d_UNICEF="1.60.2D. Quelles sont les agences (ONG/Agences onusiennes, Organisation société civile) qui sont venues ici pour vous parler de la situation de l’éducation ou des besoins de cette école depuis le début des crises administratives/sociales de septembre 2021 à Mars 2022 ? :: UNICEF",
    q160_2d_SAVETHECHILDREN="1.60.2D. Quelles sont les agences (ONG/Agences onusiennes, Organisation société civile) qui sont venues ici pour vous parler de la situation de l’éducation ou des besoins de cette école depuis le début des crises administratives/sociales de septembre 2021 à Mars 2022 ? :: SAVE THE CHILDREN",
    q201="Nombre d'enseignants travaillant actuellement dans l'école",
    q202="Nombre d'enseignants qui sont presents et enseignent actuellement à l'école",
    q203="Nombre d'enseignants qui devraient être présents et enseignés dans l'école aujourd'hui",
    q204="Nombre d'enseignants affectés par les situations de crises intervenues de septembre 2021 à Mars 2022 et ne peuvent plus travailler maintenant",
    q207="Nombre d'enseignants qualifiés",
    q208="Nombre d'enseignants non qualitfiés ayant bénéficiés de jours de formations",
    q209="Nombre d'enseignants officiellement enregistrés auprés du Gouvernement",
    q216="Nombre d'enseignants payés par le Ministère de l’Enseignement Secondaire, Technique et Professionnel",
    q217="Nombre d'enseignants payés par des ONG et autres",
    id="Identifiant Kobo de l'observation",
    version="Version du formulaire",
    form_id="Identifiant Kobo du formulaire",
    submission_time="Date de soumission",
    validation_status="Statut de validation",
    submitted_by="Pseudonyme de l'auteur de la soumission"
  ) %>% 
  # set_value_labels(
  #   
  # ) %>% 
  add_value_labels(
    regime_gestion=c("Ecoles conventionnees"=8,"Ecoles conventionnees Adventiste"=9),
    source_poste=c("Préfet des études"=6,"Responsable Association des Parents d'élèves"=7)
  )


#..................................................................................
# [On est ici dans le traitement], on doit finalise les imputations manuelles
# dans le fichier et ainsi que la construction de la fonction processing.data
#..................................................................................


#  # Traitement semi-automatique des modalités autres ainsi que des variables textuelles
# et d'autres incoherences identifiees
# Ajout de quelques variables manquantes

db_clean <- db_clean %>% 
  processing.data(
    xls_processing=import("./input/JENA_ECOLE_INCONSISTENCES_TREATED.xlsx",setclass="tibble",sheet="data")
  ) %>% ungroup()
  

# Pour export des donnees pour reporting
datasets$db_clean <- db_clean

# Extraction du dictionnaire de données associés aux données dites propres
dico_clean <- db_clean %>%
  labelled::generate_dictionary(details = "basic") %>%
  labelled::lookfor_to_long_format() %>% 
  mutate(
    col_type = case_when(
      col_type=='dttm' ~ "date",
      col_type=='chr+lbl' ~ "dbl+lbl",
      col_type=='lgl' ~ "chr",
      .default = col_type
    ),
    values = ifelse(
      !is.na(value_labels),
      str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
      levels
    ),
    labels=ifelse(
      !is.na(value_labels),
      str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
      value_labels
    )
  )

# Export des données brutes corrigees
rio::export(db_clean,"deliverable/02_clean/quanti/02_Ecoles_DATA.dta")
rio::export(db_clean %>% to_factor(),"deliverable/02_clean/quanti/02_Ecoles_DATA.xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)
rio::export(dico_clean,"deliverable/02_clean/quanti/02_Ecoles_DICO.xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)#,colWidths="auto"

#..............................................................................
# Production de la base d'analyse => selection des variables d'interet
#..............................................................................

choice_q206_5 <- c("Le CRS a fermé","INSATISFACTION DANS LE PAYEMENT DE SALAIRE",
                   "DES ENSEIGNANTS NON MECANISER PAR L’ETAT N I",
                   "PANDEMIE CORONA VIRUS","INSATISFACTION DANS LE PAYEMENT DE SALAIRE ET LA MATERNITE",
                   ". E CRS a fermé")
choice_q206_6 <- c("ILS INTEGRENT DANS DES GROUPES ARMEES","TRAUMATISME LIE AUX CRISE DE CONFLIT ARME",
                   "EXPLOITATION DES ENFA T DANS LES MINE","ÉCOLE  TRES PROCHE DU CAMPS MILITAIRE  À 100 METTRES")

choice_q302_0 <- c("AUCUN","AUCUNE MESURE","NON","AUCUNE MESURE  N’A ÉTÉ PRISE","Aucun",
                   "Aucune","none","Aucune mesure","Aucune mesure à été prise",
                   "Pas de mesure","Au mesure","AUCUNE","MANQUE  DE MOYENS",
                   "MANQUE  D,APPUIE","RIEN","NONE","AUCUNE MESURE PRISE","AUCUN MESURE",
                   "AUCUN \n","AUCUN GROUPE","Aucune mesure prise","Aucune mesure n'a été prise",
                   "AUCUNE  MESURE  PRISE","RAS","AUCUNE INITIATIVE","PAS DES MESURES",
                   "Aucune  mesure prise","Aucun groupe","AUCUNE MESURE À LEURS FAVEURS")

choice_mesure_1 <- c("ILS ONT ETE GRATUITEMENT")
choice_mesure_2 <- c("ENSEIGNEMENTS A DISTANCE","Alternance")
choice_mesure_3 <- c("L’APPUIE AVEC DES KITS SANITAIRES","LE PORT DE CACHE NEZ")

db_analysis <- db_clean %>% 
  mutate(
    motif = ifelse(!is.na(autre_ecole_motif),autre_ecole_motif,pourquoi_remplacement),
    motif = str_replace_all(motif,"^(ECOLE NON FONCTIONNEL)$","ECOLE NON FONCTIONNELLE"),
    date_reunion = ymd(q163),date_collecte=ymd(today),duree=as.numeric(date_collecte-date_reunion),
    q208=as.numeric(q208),
    across(
      all_of(c(sprintf("q206_2a_%d",1:6),sprintf("q206_2b_%d",1:6))),
      ~ replace_na(.x,0)
    ),
    q206_2a_5 = ifelse(q206_2a_other %in% choice_q206_5,1,q206_2a_5),
    q206_2b_5 = ifelse(q206_2b_other %in% choice_q206_5,1,q206_2b_5),
    q206_2a_6 = ifelse(q206_2a_other %in% choice_q206_6,1,q206_2a_6),
    q206_2b_6 = ifelse(q206_2b_other %in% choice_q206_6,1,q206_2b_6),
    q206_2_1=as.numeric(q206_2a_1|q206_2b_1),
    q206_2_2=as.numeric(q206_2a_2|q206_2b_2),
    q206_2_3=as.numeric(q206_2a_3|q206_2b_3),
    q206_2_4=as.numeric(q206_2a_4|q206_2b_4),
    q206_2_5=as.numeric(q206_2a_5|q206_2b_5),
    q206_2_6=as.numeric(q206_2a_6|q206_2b_6),
    au_moins_une_mesure = ifelse(q302_96==0,1,as.numeric(!q302_other%in% choice_q302_0)),
    mesure_1 = ifelse(is.na(q302a_1) & is.na(q302b_1) & is.na(q302c_1) & is.na(q302d_1) & is.na(q302e_1) & 
                        is.na(q302f_1) & is.na(q302g_1) & is.na(q302h_1) & is.na(q302i_1) , NA_integer_,
                      as.numeric(
                        replace_na(q302a_1,0) |replace_na(q302b_1,0) |replace_na(q302c_1,0) |replace_na(q302d_1,0) |
                          replace_na(q302e_1,0) |replace_na(q302f_1,0) |replace_na(q302g_1,0) |replace_na(q302h_1,0) |
                          replace_na(q302i_1,0)
                      )),
    mesure_1 = ifelse(q302_other %in% choice_mesure_1,1,mesure_1),
    mesure_2 = ifelse(is.na(q302a_2) & is.na(q302b_2) & is.na(q302c_2) & is.na(q302d_2) & is.na(q302e_2) & 
                        is.na(q302f_2) & is.na(q302g_2) & is.na(q302h_2) & is.na(q302i_2) , NA_integer_,
                      as.numeric(
                        replace_na(q302a_2,0) |replace_na(q302b_2,0) |replace_na(q302c_2,0) |replace_na(q302d_2,0) |
                          replace_na(q302e_2,0) |replace_na(q302f_2,0) |replace_na(q302g_2,0) |replace_na(q302h_2,0) |
                          replace_na(q302i_2,0)
                      )),
    mesure_2 = ifelse(q302_other %in% choice_mesure_2,1,mesure_2),
    mesure_3 = ifelse(is.na(q302a_3) & is.na(q302b_3) & is.na(q302c_3) & is.na(q302d_3) & is.na(q302e_3) & 
                        is.na(q302f_3) & is.na(q302g_3) & is.na(q302h_3) & is.na(q302i_3) , NA_integer_,
                      as.numeric(
                        replace_na(q302a_3,0) |replace_na(q302b_3,0) |replace_na(q302c_3,0) |replace_na(q302d_3,0) |
                          replace_na(q302e_3,0) |replace_na(q302f_3,0) |replace_na(q302g_3,0) |replace_na(q302h_3,0) |
                          replace_na(q302i_3,0)
                      )),
    mesure_3 = ifelse(q302_other %in% choice_mesure_3,1,mesure_3),
    mesure_4 = ifelse(is.na(q302a_4) & is.na(q302b_4) & is.na(q302c_4) & is.na(q302d_4) & is.na(q302e_4) & 
                        is.na(q302f_4) & is.na(q302g_4) & is.na(q302h_4) & is.na(q302i_4) , NA_integer_,
                      as.numeric(
                        replace_na(q302a_4,0) |replace_na(q302b_4,0) |replace_na(q302c_4,0) |replace_na(q302d_4,0) |
                          replace_na(q302e_4,0) |replace_na(q302f_4,0) |replace_na(q302g_4,0) |replace_na(q302h_4,0) |
                          replace_na(q302i_4,0)
                      )),
    across(
      all_of(sprintf("mesure_%d",1:4)), ~ replace_na(.x,0)
    )
  ) %>% 
  replace_na(replace=list(
    q208_1=0,q208_2=0,q216_1=0,q216_2=0
  )) %>% 
  set_value_labels(
    q119_2=c("Des mesures ont été prises"=1,"Aucune mesure n'a été prise"=0)
  ) %>% 
  set_variable_labels(
    province = "Province",
    milres="Milieu d’implantation de l’établissement",
    type_etablissement ="Type d’établissement",
    secteur="Secteur d’enseignement",
    motif="Principale raison de la non-fonctionnalité de l'école",
    date_reunion = "Date de la derniere reunion",
    duree = "Durée ecoulée en jour depuis la dernière réunion",
    q119_1_1 ="Principaux dangers naturels:Inondations",
    q119_1_2 ="Principaux dangers naturels:Tremblements de terre",
    q119_1_3 ="Principaux dangers naturels:Typhons/cyclones/ouragans",
    q119_1_4 ="Principaux dangers naturels:Glissements de terrain",
    q119_1_5 ="Principaux dangers naturels:Feux de brousse",
    q119_1_6 ="Principaux dangers naturels:Eruption volcanique",
    q208 ="Enseignants non qualifiés ayant bénéficié d'au moins dix jours complets de formation à l’enseignement",
    q208_1 = "Enseignants hommes non qualifiés ayant bénéficié d'au moins dix jours complets de formation à l’enseignement",
    q208_2 = "Enseignants femmes non qualifiés ayant bénéficié d'au moins dix jours complets de formation à l’enseignement",
    q216 ="Nombre d'enseignants travaillant dans l’école et payés par le Ministère de l’Éducation",
    q216_1 = "Nombre d'enseignants hommes travaillant dans l’école et payés par le Ministère de l’Éducation",
    q216_2 = "Nombre d'enseignants femmes travaillant dans l’école et payés par le Ministère de l’Éducation",
    q206_2_1="Raisons de la dimunition du nombre d'enseignants : Blessé",
    q206_2_2="Raisons de la dimunition du nombre d'enseignants : Mort",
    q206_2_3="Raisons de la dimunition du nombre d'enseignants : Disparu",
    q206_2_4="Raisons de la dimunition du nombre d'enseignants : Déplacé",
    q206_2_5="Raisons de la dimunition du nombre d'enseignants : Pas payé",
    q206_2_6="Raisons de la dimunition du nombre d'enseignants : Insécurité",
    q206_2a_1="Raisons de la dimunition du nombre d'enseignants hommes : Blessé",
    q206_2a_2="Raisons de la dimunition du nombre d'enseignants hommes : Mort",
    q206_2a_3="Raisons de la dimunition du nombre d'enseignants hommes : Disparu",
    q206_2a_4="Raisons de la dimunition du nombre d'enseignants hommes : Déplacé",
    q206_2a_5="Raisons de la dimunition du nombre d'enseignants hommes : Pas payé",
    q206_2a_6="Raisons de la dimunition du nombre d'enseignants hommes : Insécurité",
    q206_2b_1="Raisons de la dimunition du nombre d'enseignants femmes : Blessé",
    q206_2b_2="Raisons de la dimunition du nombre d'enseignants femmes : Mort",
    q206_2b_3="Raisons de la dimunition du nombre d'enseignants femmes : Disparu",
    q206_2b_4="Raisons de la dimunition du nombre d'enseignants femmes : Déplacé",
    q206_2b_5="Raisons de la dimunition du nombre d'enseignants femmes : Pas payé",
    q206_2b_6="Raisons de la dimunition du nombre d'enseignants femmes : Insécurité",
    mesure_1="Soutien financier supplémentaire aux apprenants du groupe",
    mesure_2="Un effort particulier pour améliorer l’accès à l’infrastructure pour les apprenants",
    mesure_3="Appareils subventionnés pour l’accès",
    mesure_4="Matériel d’apprentissage sur mesure pour le groupe"
  )
  
dico_analysis <- db_analysis %>%
  labelled::generate_dictionary(details = "basic") %>%
  labelled::lookfor_to_long_format() %>% 
  mutate(
    col_type = case_when(
      col_type=='dttm' ~ "date",
      col_type=='chr+lbl' ~ "dbl+lbl",
      col_type=='lgl' ~ "chr",
      .default = col_type
    ),
    values = ifelse(
      !is.na(value_labels),
      str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
      levels
    ),
    labels=ifelse(
      !is.na(value_labels),
      str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
      value_labels
    )
  )

# Export des donnees brutes corrigees
rio::export(db_analysis,"deliverable/03_analysis/quanti/02_Ecoles_DATA.dta")
rio::export(db_analysis %>% to_factor() ,"deliverable/03_analysis/quanti/02_Ecoles_DATA.xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)#,colWidths="auto"
rio::export(dico_analysis,"deliverable/03_analysis/quanti/02_Ecoles_DICO.xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)#,colWidths="auto"

# Nettoyage de l'environnememt
rm(db_clean,db_raw,db_analysis,dico_clean,dico_raw,dico_analysis,
   choice_q206_5,choice_q206_6,choice_q302_0,choice_mesure_1,choice_mesure_2,
   choice_mesure_3)


#*********************************************************************************
# Traitement des données du QUESTIONNAIRE INFORMATEUR CLE [quanti]
#*********************************************************************************

#..............................................................................
# (1) Traitement de la base de donnees principale
#..............................................................................
db_raw <- databases$informateur[["main"]] %>%
  select(-c(deviceid:survey_year,enum_name:fullname,key_note,consent_msg,
            crise_01:crise_04,key01,key02,`_uuid`,`_status`,`_submitted_by`,
            q110_count,q113_count,q124_count,q301_count)) %>% 
  rename(
    id=`_id`,version=`__version__`,form_id=`_xform_id_string`,#status=`_status`,
    submission_time=`_submission_time`,validation_status=`_validation_status`,#,submitted_by=`_submitted_by`
    index=`_index`
  ) %>% 
  mutate(
    start = lubridate::as_datetime(start),
    end = lubridate::as_datetime(end),
    today=lubridate::as_date(today),
    submission_time= lubridate::as_datetime(submission_time),
    # On se rassure de ne pas aovoir des couacs lors de l'export au format stata
    # ou autre
    ecole0 = as.character(ecole_sample0) %>% tidyr::replace_na(""),
    ecole1 = as.character(ecole_sample1) %>% tidyr::replace_na(""),
    schoolID = ifelse(readr::parse_number(ecole_sample0_estremplace)==1,as.character(ecole_sample1),
                      as.character(ecole_sample0)),
    id_ecole=stringr::str_to_upper(ifelse(schoolID=="R-000",nom_ecole,schoolID)),
    ecole_sample0 = to_character(ecole_sample0),
    ecole_sample1 = to_character(ecole_sample1),
    validation_status=to_character(validation_status),
    consent_comment=to_character(consent_comment) # type logical qui devrait être chaine de caractere
  ) %>% 
  # On ne conserve que les données correspondant à la période d’enquête
  filter(start>=param$survey_startdate,end<=param$survey_enddate) %>% 
  # Correction de la structure des donnees afin de faciliter l'export des resultats
  correcting.dataStructure() %>% 
  # Complétion des noms des variables
  set_variable_labels(
    hhid = "Identifant applicatif",
    id="Identifiant Kobo de l'observation",
    version="Version du formulaire",
    form_id="Identifiant Kobo du formulaire",
    submission_time="Date de soumission",
    validation_status="Statut de validation",
    # submitted_by="Pseudonyme de l'auteur de la soumission",
    nom_ecole_sample0 = "Identifiant de l'école issu de l'échantillon primaire",
    nom_ecole_sample1 = "Identifiant de l'école issu de l'échantillon de remplacement",
    nom_ecole = "Nom de l'école enquêté",
    ecole0="Nom de l’établissement issu de l'échantillon primaire",
    ecole1="Nom de l’établissement issu de l'échantillon de remplacement",
    schoolID ="Identifant applicatif de l'école enquêté",
    id_ecole="Identifant de l'école enquêté",
    consent_qst_texte="Principale question posée lors du consentement",
    consent_comment="Commentaire formulée lors du consentement",
    q102_count = "Nombre de crises survenues entre septembre 2021 et mars 2022"
  )

#POUR REPORTING DES DONNEES 
datasets$db_informer_raw <- db_raw

# Extraction du dictionnaire des donnees associe aux donnees brutes
dico_raw <- db_raw %>%
  labelled::generate_dictionary(details = "basic") %>%
  labelled::lookfor_to_long_format() %>% 
  mutate(
    col_type = case_when(
      col_type=='dttm' ~ "date",
      col_type=='chr+lbl' ~ "dbl+lbl",
      col_type=='lgl' ~ "chr",
      .default = col_type
    ),
    values = ifelse(
      !is.na(value_labels),
      str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
      levels
    ),
    labels=ifelse(
      !is.na(value_labels),
      str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
      value_labels
    )
  )

# Export des donnees brutes ajustées
rio::export(db_raw,"deliverable/01_raw/quanti/01_Informateur_DATA.dta","stata")
rio::export(db_raw,"deliverable/01_raw/quanti/01_Informateur_DATA.xlsx","xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="data",firstRow=TRUE,
            keepNA=FALSE)
rio::export(dico_raw,"deliverable/01_raw/quanti/01_Informateur_DICO.xlsx","xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)#,colWidths="auto"

# ## Extraction des donnees a traiter manuellement
# ## Le code ci-dessous s'execute une seule fois....
# getData.toBeProcessed(db_raw,
#                       instructions=import("./input/INFORMATEUR_TREATMENT_INSTRUCTIONS.xlsx",
#                                           setclass="tibble",sheet="school")) %>%
#   rio::export("./output/JENA_INFORMATEUR_INCONSISTENCES.xlsx",asTable = TRUE,
#               creator="Charles Mouté",sheetName="data",firstRow=TRUE,keepNA=FALSE)

#..............................................................................
# Apurement des donnees 
#..............................................................................

# Ajustement de la structure des donnees
db_clean <-  db_raw %>% 
  # Suppression des variables jugees inutiles/sensibles
  select(-c(
    nom_directeur,source_nom,source_telephone
  )) %>% 
  filter(
    # On supprime les donnees non approuvees ainsi que les observations en lien
    # avec un refus de participation
    validation_status!=param$STATUT_NOT_APPROVED,
    consent_value==1
  ) %>% 
  # suppression automatique des doublons sur les variables d'identification
  # On travaille sur les données les plus récentes, pour éviter de supprimer des 
  # données importantes ..
  processing.duplicateData() %>% 
  processing.duplicateData(var_id="id_ecole") %>% 
  mutate(
    enum_id = as.numeric(enum_id)
  ) %>% 
  # Complétion des noms des variables
  set_variable_labels(
    enum_id="Identifiant agent de collecte"
  ) # Tratement incomplet, vu l'urgence les modalites autres n'ont pas exemple pas ete traitées


#..................................................................................
# [On est ici dans le traitement], on doit finalise les imputations manuelles
# dans le fichier et ainsi que la construction de la fonction processing.data
#..................................................................................


#  # Traitement semi-automatique des modalités autres ainsi que des variables textuelles
# et d'autres incoherences identifiees
# Ajout de quelques variables manquantes
db_clean <- db_clean %>% 
  processing.data(
    xls_processing=import("./input/JENA_INFORMATEUR_INCONSISTENCES_TREATED.xlsx",
                          col_types="text",setclass="tibble",sheet="data")
  ) %>% ungroup()


# Extraction du dictionnaire de données associés aux données dites propres
dico_clean <- db_clean %>%
  labelled::generate_dictionary(details = "basic") %>%
  labelled::lookfor_to_long_format() %>% 
  mutate(
    col_type = case_when(
      col_type=='dttm' ~ "date",
      col_type=='chr+lbl' ~ "dbl+lbl",
      col_type=='lgl' ~ "chr",
      .default = col_type
    ),
    values = ifelse(
      !is.na(value_labels),
      str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
      levels
    ),
    labels=ifelse(
      !is.na(value_labels),
      str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
      value_labels
    )
  )

# Export des données brutes corrigees
rio::export(db_clean,"deliverable/02_clean/quanti/01_Informateur_DATA.dta")
rio::export(db_clean %>% to_factor(),"deliverable/02_clean/quanti/01_Informateur_DATA.xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)
rio::export(dico_clean,"deliverable/02_clean/quanti/01_Informateur_DICO.xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)#,colWidths="auto"

#..............................................................................
# Production de la base d'analyse => selection des variables d'interet
#..............................................................................
# Dans le plan de tabulation cette base n'a pas vraiment ete exploité [...]
db_analysis <- db_clean
dico_analysis <- dico_clean

# Export des donnees brutes corrigees
rio::export(db_analysis,"deliverable/03_analysis/quanti/01_Informateur_DATA.dta")
rio::export(db_analysis %>% to_factor() ,"deliverable/03_analysis/quanti/01_Informateur_DATA.xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)#,colWidths="auto"
rio::export(dico_analysis,"deliverable/03_analysis/quanti/01_Informateur_DICO.xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)#,colWidths="auto"

# Nettoyage de l'environnememt
rm(db_clean,db_raw,db_analysis,dico_clean,dico_raw,dico_analysis)


#..............................................................................
# (2) Traitement de la base de donnees sur l'assistance aux personnes handicapées
#..............................................................................

db_raw <- databases$informateur[["aide_pour_ph"]] %>%
  select(-c(q302,`_validation_status`,crisis_idx)) %>% 
  rename(index=`_index`,parent_index=`_parent_index`,parent_table_name=`_parent_table_name`) %>% 
  set_variable_labels(
    # crisis_idx="Numéro séquentielle",
    crisis_id="Code associé à la crise",
    nom_crise="Intitulé de la crise",
    q302_1="Assistance :: Surveillance de la protection des enfants",
    q302_2="Assistance :: Alimentation à l’école",
    q302_3="Assistance :: Fourniture de matériel sanitaire pour les filles",
    q302_4="Assistance :: Fourniture d’uniformes ou de vêtements",
    q302_5="Assistance :: Bourses ou exonération des frais de scolarité",
    q302_6="Assistance :: Emploi du temps flexible",
    q302_7="Assistance :: Classes séparées pour les apprenants plus âgés",
    q302_8="Assistance :: Suivi de l’abandon scolaire",
    q302_9="Assistance :: Cours « de rattrapage » ou accélérés",
    q302_10="Assistance :: Services de garderie pour les enfants",
    q302_96="Assistance :: Autre (préciser)",
    q302_other="Autre assistance reçue",
    parent_table_name = "Nom de la table parent",
    parent_index="Clé primaire de la table parent",
    index ="Clé primaire de la table"
  )

dico_raw <- db_raw %>%
  labelled::generate_dictionary(details = "basic") %>%
  labelled::lookfor_to_long_format() %>% 
  mutate(
    col_type = case_when(
      col_type=='dttm' ~ "date",
      col_type=='chr+lbl' ~ "dbl+lbl",
      col_type=='lgl' ~ "chr",
      .default = col_type
    ),
    values = ifelse(
      !is.na(value_labels),
      str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
      levels
    ),
    labels=ifelse(
      !is.na(value_labels),
      str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
      value_labels
    )
  )

#POUR REPORTING DES DONNEES 
datasets$db_informer_assist_raw <- db_raw

# Export des donnees brutes
rio::export(db_raw,"deliverable/01_raw/quanti/01_AIDE_POUR_PH_DATA.dta")
rio::export(db_raw %>% to_factor() ,"deliverable/01_raw/quanti/01_AIDE_POUR_PH_DATA.xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)#,colWidths="auto"
rio::export(dico_raw,"deliverable/01_raw/quanti/01_AIDE_POUR_PH_DICO.xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)#,colWidths="auto"


# Export des donnees brutes corrigees
db_clean <- db_raw
dico_clean <- dico_raw
rio::export(db_clean,"deliverable/02_clean/quanti/01_AIDE_POUR_PH_DATA.dta")
rio::export(db_clean %>% to_factor() ,"deliverable/02_clean/quanti/01_AIDE_POUR_PH_DATA.xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)#,colWidths="auto"
rio::export(dico_clean,"deliverable/02_clean/quanti/01_AIDE_POUR_PH_DICO.xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)#,colWidths="auto"

# Traitement des donnees avant export
# db_informer_assist_raw ; db_informer_raw
choice_i302_1 <- c("Accompagner leurs enfants à l’école")
choice_i302_2 <- c("Cantine scolaire")
choice_i302_3 <- c("LAVABOS,  CASH NEZ")
choice_i302_5 <- c("Pris en charge, assistance educationnel",
                   "BENEFICIER DE LA GRATUITE PRONEE PAR LE GOUVERNEMENT")

db_analysis <- db_clean %>% 
  mutate(
    q302_1 = ifelse(q302_1 %in% choice_i302_1,1,q302_1),
    q302_2 = ifelse(q302_2 %in% choice_i302_2,1,q302_2),
    q302_3 = ifelse(q302_3 %in% choice_i302_3,1,q302_3),
    q302_5 = ifelse(q302_5 %in% choice_i302_5,1,q302_5)
  ) %>% 
  set_variable_labels(
    q302_1="Surveillance de la protection des enfants",
    q302_2="Alimentation à l’école",
    q302_3="Fourniture de matériel sanitaire pour les filles",
    q302_5="Bourses ou exonération des frais de scolarité"
  )

dico_analysis <- db_raw %>%
  labelled::generate_dictionary(details = "basic") %>%
  labelled::lookfor_to_long_format() %>% 
  mutate(
    col_type = case_when(
      col_type=='dttm' ~ "date",
      col_type=='chr+lbl' ~ "dbl+lbl",
      col_type=='lgl' ~ "chr",
      .default = col_type
    ),
    values = ifelse(
      !is.na(value_labels),
      str_trim(str_replace_all(str_extract(value_labels,"^(\\[(.)+\\])"),pattern = "\\[|\\]","")),
      levels
    ),
    labels=ifelse(
      !is.na(value_labels),
      str_trim(str_replace(value_labels,"(\\[(.)+\\])","")),
      value_labels
    )
  )

# Export des donnees pour l'analyse
rio::export(db_raw,"deliverable/03_analysis/quanti/01_AIDE_POUR_PH_DATA.dta")
rio::export(db_raw %>% to_factor() ,"deliverable/03_analysis/quanti/01_AIDE_POUR_PH_DATA.xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)#,colWidths="auto"
rio::export(dico_raw,"deliverable/03_analysis/quanti/01_AIDE_POUR_PH_DICO.xlsx",
            asTable = TRUE,creator="Charles Mouté",sheetName="dico",firstRow=TRUE,
            keepNA=FALSE)#,colWidths="auto"

rm(db_clean,db_raw,db_analysis,dico_clean,dico_raw,dico_analysis,
   choice_i302_1,choice_i302_2,choice_i302_3,choice_i302_5)


# Sauvegarde de l'environnement de travail
save.image()

# Export des données pour le reporting
export(datasets,file = "datasets.rda")

