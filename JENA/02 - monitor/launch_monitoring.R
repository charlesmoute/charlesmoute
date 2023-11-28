# Lancement des activités de monitoring
# charles.moute@gmail.com

# ...............................................................................
# Chargement des librairies utilitaires
#...............................................................................

# Import des packages utiles
library(openxlsx) # Pour exporter les donnees au format excel
library(robotoolbox) #pour telecharger les donnees depuis le serveur kobo
# library(tidyverse)
library(tidyr)
library(readr)
library(dplyr) #pour le traitement des donnees

# Import des fonctions utilitaires ainsi que des donnnees
rm(list=ls())
source("JENA_downloadData.R") # Telecharge les donnees, les traite et les mets a jour
# source("JENA_library.R") # Chargement les donnees mises a jour et les librairies
source("JENA_monitoring.R") #lance la production des donnees 

# source("launch_monitoring.R")