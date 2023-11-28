# Monnitoring de la performance de l'enquête et des agents
# @Charles Mouté


# ...............................................................................
# Chargement des librairies utilitaires
#...............................................................................

# library(openxlsx)
# library(tidyverse)

# Import des fonctions utilitaires ainsi que des donnnees
rm(list=ls())
source("JENA_library.R")

#...............................................................................
# Export de quelques resultats d'interets
#...............................................................................

horaire <- Sys.time()
horodatage <- strftime(horaire, "%y%m%d%H%M%S")
jena_monitoring_date <- strftime(horaire, "%Y-%m-%d")
print_byprovince <- FALSE

path_output <- "output/deliverable/"

# Export des eventuelles incoherences sur les ecoles de remplacement
db <- bind_rows(get.replacementSchoolsInSample() %>% mutate(replacement="InSample"),
                get.replacementSchoolsOutSample() %>% mutate(replacement="OutSample")) %>% 
  mutate(monitoring_date=jena_monitoring_date,today=as.character(today)) %>% 
  rename(date_collecte=today) %>% labelled::to_factor()
if(!is.null(db) & nrow(db)>0)
  export.dataset(db,sprintf("%sJENA_REMPLACEMENT_%s.xlsx",path_output,horodatage),
                 by_province=print_byprovince)

# Export des questionnaires ecoles sans equivalence parmi les questionnaires
# informateurs cles et inversement
db <- get.schoolWithoutEquivalentInformants() %>% 
  mutate(monitoring_date=jena_monitoring_date,today=as.character(today)) %>% 
  rename(date_collecte=today) %>% labelled::to_factor() %>% 
  mutate(checked=0,observation="")
if(!is.null(db) & nrow(db)>0)
  export.dataset(db,sprintf("%sJENA_QUANTI_NON_EQUIVALENT_%s.xlsx",path_output,horodatage),
                 by_province=print_byprovince)

# Export des questionnaires qui n'ont pas de statut. Cela vaut exclusivement pour les questionnaires
# des superviseurs et ceux des agents quali comme quati
db <- get.unapprovedForms()  %>% 
  mutate(monitoring_date=jena_monitoring_date,today=as.character(today)) %>% 
  rename(date_collecte=today) %>% labelled::to_factor() %>% 
  mutate(checked=0,observation="")
if(!is.null(db) & nrow(db)>0)
  export.dataset(db,sprintf("%sJENA_FORMULAIRE_A_EXAMINER_%s.xlsx",path_output,horodatage),
                 by_province=print_byprovince)

# Export des doublons d'entretien ou de formulaire
db <- get.duplicatedData() %>% 
  mutate(monitoring_date=jena_monitoring_date,today=as.character(today)) %>% 
  rename(date_collecte=today) %>% labelled::to_factor() %>% 
  mutate(checked=0,observation="")
if(!is.null(db) & nrow(db)>0) 
  export.dataset(db,sprintf("%sJENA_DOUBLON_INTERVIEW_%s.xlsx",path_output,horodatage),
                 by_province=print_byprovince)

# Export des numeros de telephone en double
db <- check.duplicatedPhonenumber() %>% 
  mutate(monitoring_date=jena_monitoring_date,today=as.character(today)) %>% 
  rename(date_collecte=today) %>% labelled::to_factor() %>% 
  mutate(checked=0,observation="")
if(!is.null(db) & nrow(db)>0) 
  export.dataset(db,sprintf("%sJENA_DOUBLON_NUMERO_TELEPHONE_%s.xlsx",path_output,horodatage),
                 by_province=print_byprovince)

# Export des numeros de telephone invalides
db <- check.undefinedPhonenumber() %>% 
  mutate(monitoring_date=jena_monitoring_date,today=as.character(today)) %>% 
  rename(date_collecte=today) %>% labelled::to_factor() %>% 
  mutate(checked=0,observation="")
if(!is.null(db) & nrow(db)>0) 
  export.dataset(db,sprintf("%sJENA_NUMERO_TELEPHONE_INVALIDE_%s.xlsx",path_output,horodatage),
                 by_province=print_byprovince)

# # Export des formulaires d'anlyse qui n'ont pas de formulaire de synthèse et inversement.
# db <- get.formsWithoutSummary() %>% 
#   mutate(monitoring_date=jena_monitoring_date,today=as.character(today)) %>% 
#   rename(date_collecte=today) %>% labelled::to_factor() %>% 
#   mutate(checked=0,observation="")
# if(!is.null(db) & nrow(db)>0) 
#   export.dataset(db,sprintf("%sJENA_QUALI_SANS_SYNTHESE_%s.xlsx",path_output,horodatage),
#                  by_province=print_byprovince)


# Export de tous les informations sur les pieces jointes des formulaires quali
db <- get.infosOnAttachements() %>% 
  mutate(monitoring_date=jena_monitoring_date,today=as.character(today)) %>% 
  # select(-Audio,-Image) %>% 
  rename(date_collecte=today) %>% labelled::to_factor() %>% 
  mutate(checked=0,observation="")
if(!is.null(db) & nrow(db)>0) 
  export.dataset(db,sprintf("%sJENA_QUALI_PIECES_JOINTES_%s.xlsx",path_output,horodatage),
                 by_province=print_byprovince)


# Export de toutes les données afin de calculer les indicateurs d'interet
db <- get.dataForMonitoring() %>% 
  mutate(monitoring_date=jena_monitoring_date,today=as.character(today)) %>% 
  select(-form_id,-schoolID,-caseid,-ecole_sample0_estremplace) %>% 
  rename(date_collecte=today) %>% labelled::to_factor()
if(!is.null(db) & nrow(db)>0){
  export.dataset(db,sprintf("%sJENA_DATA_%s.xlsx",path_output,horodatage))
  # SAUVEGARDE DANS LE DOSSIER DE BASE POUR FACILITER LA MISE A JOUR MANUEL DU
  # DASHBOARD STATIQUE
  rio::export(db,"JENA_DATA.xlsx",asTable = TRUE,creator="Charles Mouté",
              sheetName="data",keepNA=FALSE,colWidths="auto")
} 

#Export des indicateurs de comparaisons des donnees conformement aux differents
#Plan d'echantillon
result <- comparaison.expectedData_collectedData(jena_monitoring_date)
export.dataset(result[["monitor_quanti"]],
               sprintf("%sJENA_MONITOR_QUANTI_%s.xlsx",path_output,horodatage))
export.dataset(result[["monitor_quali"]],
               sprintf("%sJENA_MONITOR_QUALI_%s.xlsx",path_output,horodatage))
export.dataset(result[["monitor_quali_school"]],
               sprintf("%sJENA_MONITOR_QUALI_ECOLE_%s.xlsx",path_output,horodatage))
export.dataset(result[["monitor_quali_manager"]],
               sprintf("%sJENA_MONITOR_QUALI_MANAGER_%s.xlsx",path_output,horodatage))


# Mise a jour du tableau de bord excel
# Pour une raison inconnu le report automatique des informations abime le tableau de bord
# on copiera manuellement les données dans l'onglet data du TableauDeBord Excel
# update.jenaDashboard(jena_monitoring_date)

# Nettoyage de l'environnement de travail
rm(db,result)
