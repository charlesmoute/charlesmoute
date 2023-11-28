
# Export des fichiers au format cvs
library(tidyverse)

readxl::read_excel("03_quanti/dataset/cases.xlsx",sheet = "cases") %>% 
  write_csv("cases.csv",na="")
