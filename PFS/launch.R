# Script d'execution des programmes de
# gestion de données

# PFS
# Août 2021
# @version : 210826-1635
# @author  : Charles M. <charlesmoute@gmail.com>

#...........................................................................................................
# Configuration de l'espace de travail
#...........................................................................................................

source("init.R")

#----------------------------------------------------------------------------------------------->
# Execution des scripts de gestion des données/reporting
#----------------------------------------------------------------------------------------------->

# 1er reporting : 07/09/2021 => couvrant la periode du 12/08/2021 au 06/09/2021
# 2e reporting :  14/09/2021 => couvrant la periode du 07/09/2021 au 13/09/2021
# 3e reporting :  21/09/2021 => couvrant la periode du 14/09/2021 au 27/09/2021

# Production du rapport d'activite d'ensemble
write.pfsInfos(startdate=param$startdate,enddate=param$enddate)

# Production de la liste des doublons
write.duplicateCase(startdate=param$startdate,enddate=param$enddate)

# Production du tableau des effectifs des questionnaires par commune chez les
# les hommes et les femmes en distinguant des questionnaires soumis,les doublons
# les conpolets, les inéligibles/incomplets et en rapportant le taux de couverture
# write.statDocs(startdate=param$startdate,enddate=param$enddate,supervisorList = "MOUTE CHARLES")
write.statDocs(startdate=param$startdate,enddate=param$enddate)


# Production des rapports d'evaluations par village et sur une periode indiquee
write.townInfos(townList= str_sort(unique(c(df[["id_town"]],dh[["id_town"]]))),
                startdate=param$startdate,enddate=param$enddate,graph_onescreen=FALSE)

# Production des rapports d'evaluations par agent et sur une periode indique
write.agentInfos(str_sort(unique(c(df[["agentname"]],dh[["agentname"]]))),
                 startdate=param$startdate,enddate=param$enddate,graph_onescreen=FALSE)

# Production des rapports d'evaluations par superviseur et sur une periode indique
write.supervisorInfos(str_sort(unique(staff[["supervisor_name"]])),
                      startdate=param$startdate,enddate=param$enddate,
                      graph_onescreen=FALSE)

# Sauvegarde de l'environnement de travai   
save.image(sprintf("PFS_%s.RData",format(Sys.time(),"%Y%m%d_%H%M"))); save.image()

#----------------------------------------------------------------------------------------------->
# Scripts ponctuels : Comparaisons d'observations
#----------------------------------------------------------------------------------------------->

# Comparaison de formulaires soupconnes d'etre des doublons
# df= base de donnees des femmes .. dh= base de donnees
tmp <- df %>% filter(caseid=="F-3130603292")
# Si plus de deux doublons pour un même caseid, modifier les indices 1 & 2 par les
# ceux à comparer..
result <- compareDF::compare_df(tmp[1,],tmp[2,],"caseid",keep_unchanged_cols=FALSE)
View(result)
# result[["comparison_df"]][["key"]]
# View(result$comparison_df)
rm(tmp,result)

#----------------------------------------------------------------------------------------------->
# Scripts ponctuels : Extraction des doublons pour quelques superviseurs..
#----------------------------------------------------------------------------------------------->
extract.duplicateCases(supervisorlist = c("ASSOMO MONEMOTO ARSÈNE"),
                       filename="listeDoublons_ASSOMO.xlsx")
