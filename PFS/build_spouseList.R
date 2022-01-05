# Production de la liste des conjoints
# Charles Mouté <charlesmoute@gmail.com>
# version : 210814-0945


#...........................................................................................
# Chargement des librairies utilitaires
#...........................................................................................
library(readxl)
library(tidyverse)


# Preparation du fichier
#Old_path : Archive/case_management/04 - Enquete/Baseline Sample_HH List_2021-08-06.xlsx
db_baseline  <- read_excel(path = "data_reporting/Baseline Sample_HH List_2021-08-06.xlsx",
                           sheet = 3)
db_baseline <- db_baseline[,c(13,14,15,33,43,46,58,70,82,94,106)]
varlist <- c("hhid","lastname","firstname","hhh_name","hhh_spouseCount","spouse1","spouse2","spouse3",
             "spouse4","spouse5","spouse6")
#spouse0=hhh_name = household head name
names(db_baseline) <- varlist
unique(db_baseline$spouse6) # pas de 6eme epouse
unique(db_baseline$spouse5) # pas de 5eme epouse
unique(db_baseline$spouse4) # Ok


db_baseline$'0' <- sprintf("Chef de ménage: %s",db_baseline$hhh_name)
db_baseline$'1' <- sprintf("Conjoint 1: %s",db_baseline$spouse1)
db_baseline$'2' <- sprintf("Conjoint 2: %s",db_baseline$spouse2)
db_baseline$'3' <- sprintf("Conjoint 3: %s",db_baseline$spouse3)
db_baseline$'4' <- sprintf("Conjoint 4: %s",db_baseline$spouse4)
db_baseline$'5' <- "Aucune des personnes ci-dessus / autre membre du ménage"
db_baseline <- db_baseline %>% select(-spouse6) 
db_baseline$name <- sprintf("%s %s",str_to_upper(db_baseline$lastname), 
                            str_to_upper(db_baseline$firstname))

#Old_path : Archive/case_management/04 - Enquete/Replacement2Households_2021-09-09.xlsx
db_extension01 <- read_excel(path = "data_reporting/Replacement2Households_2021-09-09.xlsx",
                             sheet = 1)

# Dans le cas de cette rallonge de donnees, les couples sont monogames...
# aussi nous ne retenons que la colonne "spouse_m0_1"
db_extension01 <- db_extension01 [,c("HH_ID","Nomdubénéficiaire","Prénomdubénéficiaire","hhh_m0",
                                     "hhh_spcount","spouse_m0_1")]
names(db_extension01) <- varlist[1:6]
db_extension01$'0' <- sprintf("Chef de ménage: %s",db_extension01$hhh_name)
db_extension01$'1' <- sprintf("Conjoint 1: %s",db_extension01$spouse1)
db_extension01$'5' <- "Aucune des personnes ci-dessus / autre membre du ménage"
db_extension01$name <- sprintf("%s %s",str_to_upper(db_extension01$lastname),
                               str_to_upper(db_extension01$firstname))
db <- bind_rows(db_baseline,db_extension01)
  
# dbase <- db_baseline %>% 
#   select(one_of(names(db_baseline)[c(1:5,11:16)])) %>%
#   pivot_longer(cols=6:11,names_to = "id_spouse",values_to = "spouse")
dbase <- db %>% 
  select(one_of(names(db)[c(1:5,11:16)])) %>%
  pivot_longer(cols=6:11,names_to = "id_spouse",values_to = "spouse")

dbase <- dbase [which(dbase$id_spouse<=dbase$hhh_spouseCount | dbase$id_spouse==5),]
dbase <- dbase %>% select(-hhh_spouseCount)

dbase$hhid <- as.character(dbase$hhid)
dbase$lastname <- as.character(dbase$lastname)
dbase$firstname <- as.character(dbase$firstname)
dbase$hhh_name<- as.character(dbase$hhh_name)
dbase$id_spouse <- as.numeric(dbase$id_spouse)
dbase$spouse <- as.character(dbase$spouse)
dbase <- dbase %>% select(id_spouse,spouse,lastname,firstname,hhh_name,hhid) # hhid identifiant menage PFS

# db_cases <- read_excel(path = "Archive/case_management/04 - Enquete/cases_2108140540.xlsx",sheet = 1) %>%
#   select(caseid,hhid)

# OldPath : Archive/case_management/04 - Enquete/cases_2109110325.xlsx
db_cases0 <- read_excel(path = "data_reporting/cases_2109110325.xlsx",
                       sheet = 1) %>% select(caseid,hhid)
db_cases1 <- read_excel(path = "data_reporting/cases_2109110325.xlsx",
                       sheet = 3) %>% select(caseid,hhid)
db_cases <- bind_rows(db_cases0,db_cases1)
length(which(duplicated(db_cases[c(1,3,1,4),])))

dbase <- merge(dbase,db_cases,by="hhid",all.x = TRUE)
dbase <- dbase %>% select(id_spouse,spouse,lastname,firstname,hhh_name,caseid)

dbase <- dbase %>% replace_na(list(lastname="",firstname="",hhh_name="",spouse=""))
dbase$rowid <- sprintf("%s%d",dbase$caseid,dbase$id_spouse)
dbase$spousename <- dbase$spouse
dbase$spousename <- str_replace_all(dbase$spousename,pattern = "Chef de ménage: ",replacement = "")
dbase$spousename <- str_replace_all(dbase$spousename,pattern = "Conjoint [1-5]: ",replacement = "")

write_csv(x=dbase %>% select(id_spouse,spouse,spousename,caseid,rowid),file="spouses.csv")
write_csv(dbase %>% select(caseid,lastname,firstname,hhh_name) %>% distinct(),"beneficiary.csv")
  
  
  