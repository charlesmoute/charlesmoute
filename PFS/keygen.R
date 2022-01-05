# Generation automatique des identifiants
# Charles Mouté <charlesmoute@gmail.com>
# version : 210502-0930


#...........................................................................................
# Chargement des librairies utilitaires
#...........................................................................................
library(tidyverse)

# dput(sort(unique(db$region)))
# dput(sort(unique(db$division)))
# dput(sort(unique(db$town)))

db <- read_csv2("data_reporting/beneficiaire.csv",
                col_types = list(region = col_factor(levels = c("ADAMAOUA", "EST", "EXTREME-NORD", "NORD")),
                                 division = col_factor(levels = c("LOGONE-ET-CHARI", "LOM-ET-DJEREM", "MAYO-REY", "MBERE")),
                                 council = col_factor(levels = c("BETARE-OYA", "GAROUA-BOULAI", "LOGONE-BIRNI", "MEIGANGA", 
                                                                 "TOUBORO")),
                                 town = col_factor(levels = c("ABO BOUTILA", "ALAMADA", "AMBAKANG", "Ambre Blama Cherif", 
                                                              "ARDO GADDO", "ASSOURA", "AVIATION", "BABONGO", "BABOUA", "BADAM", 
                                                              "BAGOU", "Bague", "BAINA I", "BAINA II", "BAKARI", "BANGUEL", 
                                                              "BAOUDI", "BARDE", "BARDE NDOKAYO", "BARKI", "BEGORO", "BEKA GUIWAN", 
                                                              "BEMBARANG", "BETHANIE", "BIBOKO", "BILTAO", "BINDIBA", "BINDIKI", 
                                                              "BITOU", "BLAKORO", "BODOMO ISSA", "BOFORD", "BOGDIBO", "BOGUERA", 
                                                              "BONGO", "BOULI", "BOUNOU", "BOUTOU", "BOUWA", "Choiram Mbladock", 
                                                              "Dabanga", "DABOLOE", "DABOULE", "DAINA", "DAKERE", "DAMBOURA", 
                                                              "DANA", "DANG PATOU", "DANKALI", "Darsalah", "Dava", "DIBI HOSSERE", 
                                                              "Didad 1", "Dilga Dabanga", "Dilga Mousgoum (blakoro)", "DIP", 
                                                              "DIR PETEL", "DJAKKA", "DJALINGO", "DJALLO", "DJAORO DOUA", "DJINDANG", 
                                                              "DOBEZO", "Dokdori (Ndamrou)", "DOKOLIM", "DOLE", "DOLE DANG PATOU", 
                                                              "DOLE TAMTANA", "DOUA YEL", "DOUAFO", "Dougouni", "DOYO", "Fadje", 
                                                              "FLAYE", "GAI RAY", "GAINDARA", "GAINDIKI", "GALA BOCOM", "Gambarou Mbomo", 
                                                              "GAMKOMBOL", "GANDONG", "GARGA LIMBONA", "Gaulama Borno", "GAZI", 
                                                              "GBABIO", "GBAFOUCK HOSSERE", "GBAGUETE LAWAN", "GBAGUETE SALAO", 
                                                              "GBANAM", "GBANBIRI", "GBATA NORD", "GBATAA", "GBATEN", "GBAWAR", 
                                                              "GBAYA", "GBEBANA", "Gbem", "GOMBO KORO", "Gomma", "Goudjouia", 
                                                              "GOUNTE", "GUIGO", "GUIGUI", "GUMBELA", "Halague", "Halaka", 
                                                              "HELBAO", "Hinale (Moukak)", "Honkol (+Ngaoupata)", "Houli Magnaco", 
                                                              "Houloumsa (Mariam +doudoum)", "Hounangare", "ILA", "Kabe", "Kabo 3", 
                                                              "KADES", "KAITA MIDAL", "Kalakafra", "Kalkoussam (+bague 1,2,3 et Beche)", 
                                                              "Kama", "KAMBO KASSI", "KAMZAM", "KAOU", "KARANG PANDJAMA", "Kare", 
                                                              "Kare Dorsala", "KARREFOUR MBOULA", "KASSA NGAOUNDERE", "KAWTAL 1", 
                                                              "Kidam", "KISSI", "KOMBO GOUDRON", "KOMBO VILLAGE", "KOMBOL", 
                                                              "KOMBOUL", "KONDE", "KONGOLO", "KONGOLO 2", "KOUBAO", "KOUMANE", 
                                                              "KPAMA", "KPOK MBONGA", "Labado", "LAKA PETEL", "LARA   KOUSSINI", 
                                                              "LARA NDAH", "LIGUIM", "LOBO", "LOKOTI", "LORTANG", "LOUMOU DOLE", 
                                                              "MABELE 1", "MABELE 2", "MADEPO", "Madina", "Maham", "Mahana (canton d'helbrique)", 
                                                              "MALI", "MAMA WASSANDE", "MAN   RIGARA", "Manawadji", "MARARABA", 
                                                              "MAROUM", "MAYO KOLOM", "MAYO ZAKI", "MBAH BINGO", "MBAI MBOUM", 
                                                              "MBALDI", "MBALE", "MBALI", "Mbardi", "MBASSI", "MBERE", "MBIDAN", 
                                                              "MBIDERE", "MBIGANG", "MBIKOUNI", "MBITOM", "MBITOM I", "MBITOM II", 
                                                              "MBONDO", "MBONGA", "MBORGUENE", "MBOULAI", "Mbourgouma", "MBOUSSA", 
                                                              "Mbre", "MIKILA", "MIKILA DOOZUI", "MISSION CATHOLIQUE", "MOI NAM", 
                                                              "MOMBAL", "MONAY", "Moukak Ounan Ngare", "Mrena", "NANDONGUE", 
                                                              "NANGONDA", "Ndaga", "NDANGA GANDIMA", "NDERA", "NDOCK", "NDONGUE", 
                                                              "NDOUKBAI", "NDOUROU", "NDOYONG", "NGAM", "NGANGO", "NGANHI", 
                                                              "NGARA NGOH", "NGAZI", "NGAZI TINA", "NGOLORI", "NGUEO TAO", 
                                                              "Nkologue", "Ntaka", "OUDOU LAI", "OURO   SALAMA", "OURO   SONLEY", 
                                                              "OURO DALA", "PONT LOM", "QUARTIER HAOUSSA", "SABAL  SUD", "SABGA", 
                                                              "SABONGARI GBABOE", "SABONGARI MBELA", "SARAMBI", "SARANG", "SELAL MBOUSSIRI", 
                                                              "SODENOU", "SOH", "SOKORTA", "Sororo 1", "Taboui", "TAPARE", 
                                                              "TAPARE SALAO", "TAPAWA MBALE", "TAPI", "TETE D'ELEPHANT", "TIBANGA", 
                                                              "TIKE ALAT", "TIKIRI", "Tilde Logone", "TOUGOUN", "Tourou", "Tshi", 
                                                              "WAKASSAOU", "WAKASSO", "WALDE", "WANTAMO", "WASSA", "WERTE", 
                                                              "YAFOUNOU", "YANDEA", "YELWA", "YENDE", "YOKOSIRE", "ZAMBOI", 
                                                              "Zaraf", "Zawiya Blama Bichara", "ZOEGUENE", "ZOUKOMBO")),
                                 hhid = col_double(),
                                 hoh_name = col_character(),
                                 id_region = col_number(),
                                 id_division = col_number(),
                                 id_council = col_number(),
                                 id_town = col_number(),
                                 id_supervisor = col_number(),
                                 supervisor = col_character(),
                                 id_agent = col_number(),
                                 agent = col_character()))

db$rowindex <- c(1:dim(db)[1])

tmp <- str_to_upper(db$town,locale="fr")
db$town <- factor(tmp,levels = sort(unique(tmp)))
tmp <- str_to_upper(db$hoh_name,locale="fr")
db$hoh_name <- factor(tmp,levels = sort(unique(tmp)))

# Creation des identifiants..
db$id_region <- as.numeric(db$region)
db$id_division <- as.numeric(db$division)
db$id_council <- as.numeric(db$council)
db$id_town <- as.numeric(db$town)

# Affectation aleatoire de 20 superviseurs
tmp <- db %>% group_by(id_town) %>% summarise(eff=n())
set.seed(37861)
tmp$id_supervisor <- sample(1:20,size = dim(tmp)[1],replace = T) 
tmp$supervisor <- sprintf("Superviseur %02d",tmp$id_supervisor)

db <- merge(db %>% select(-id_supervisor,-supervisor),
            tmp %>% select(id_town,id_supervisor,supervisor),
            by="id_town",all.x = TRUE,sort = FALSE)

# Affectation aleatoire des agents en fonction du nombre de menage couvert par chaque superviseur
tmp <- db %>% group_by(id_supervisor) %>% summarise(eff=n())
tmp$codeagent_start <- tmp$id_supervisor*3-2
tmp$codeagent_end <- tmp$id_supervisor*3

df <- NULL
for(i in 1:dim(tmp)[1]){
    info <- tmp %>% slice(i)
    tmp2 <- db %>% 
      filter(id_supervisor==info$id_supervisor) %>%
      select(id_supervisor,rowindex)
    set.seed(37861)
    tmp2$id_agent <- sample(c(info$codeagent_start:info$codeagent_end),
                            size=info$eff,replace=TRUE)
    tmp2$agent <- sprintf("Agent %02d.%02d",tmp2$id_supervisor,tmp2$id_agent)
    df <- rbind(df,tmp2 %>% select(-id_supervisor))
}

db <- merge(db %>% select(-id_agent,-agent),
            df,by=c("rowindex"),all.x = TRUE, sort = FALSE)

# Les idenfiants produit par l'INS ne sont pas unique... on utilisera notre rowindex
db <- db %>%
  mutate(hhid=parse_number(sprintf("%d%d%d%03d%05d",id_region,id_division,id_council,id_town,rowindex))) %>%
  arrange(id_region,id_division,id_council,id_town,rowindex) %>%
  select(id_region,region,id_division,division,id_council,council,id_town,town,id_supervisor,supervisor,
         id_agent,agent,hhid,hoh_name,rowindex)

# Suppression de variables utilitaires
rm(tmp,tmp2,df,info,i)

# Sauvegarde des donnees
write_csv(db %>% select(-rowindex),"beneficiaire_2105021245.csv")
write_csv(db %>% select(id_region,region) %>% distinct() %>% arrange(region),
          "regions.csv")
write_csv(db %>% select(id_division,division) %>% distinct() %>% arrange(division),
          "divisions.csv")
write_csv(db %>% select(id_council,council) %>% distinct() %>% arrange(council),
          "councils.csv")
write_csv(db %>% select(id_town,town) %>% distinct() %>% arrange(town),
          "towns.csv")
write_csv(db %>% select(id_supervisor,supervisor) %>% distinct() %>% arrange(supervisor),
          "supervisors.csv")
write_csv(db %>% select(id_agent,agent,id_supervisor) %>% distinct() %>% arrange(agent) ,"agents.csv")

save.image("keygen_2105021245.RData")
