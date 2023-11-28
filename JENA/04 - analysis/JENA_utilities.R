
# Fonctions utilitaires pour le traitement des donnees
# charles.moute@gmail.com


# Correction de la strucure des donnees exporter depuis Kobo
# En effet, certains etiquette ne s'affiche pas correctement et le type
# logical génére des incoherences à l'exportation que ce soit en csv ou
# sur stata
correcting.dataStructure <- function(database){
  
  result <- NULL
  if(!is.null(database)){
    
    # Generation d'un dictionnaire complet des donnees
    dico <- labelled::generate_dictionary(database,details = "full")
    
    # Traitement des erreurs d'importation de kobo_data
    # on devrait avoir pour la plupart des cas double+lbl & non chr+lbl
    
    db <- database
    varnames <- dico$variable[str_which(dico$col_type,"chr\\+lbl")]
    for(varname in varnames){
      # cat ("[ Traitement de ",varname," ]\n")
      etiq_raw <- with(dico,value_labels[which(variable==varname)]) %>% unlist()
      names(etiq_raw) <- str_replace_all(names(etiq_raw),sprintf("%s.",varname),"")
      etiquette <- suppressWarnings(parse_number(etiq_raw))
      if(is.null(attr(etiquette,"problems"))){
        names(etiquette) <- names(etiq_raw)
        db[,varname] <- parse_number(as.character(db[,varname] %>% unlist()))
        val_labels(db[,varname]) <- etiquette
        var_label(db[,varname]) <- with(dico,label[variable==varname]) %>%  to_character()
      }else{
        etiquette <- etiq_raw
        db[,varname] <- as.character(db[,varname] %>% unlist())
        val_labels(db[,varname]) <- etiquette
        var_label(db[,varname]) <- with(dico,label[variable==varname]) %>%  to_character()
      }
      # cat ("\n[ Fin de traiteme de ",varname," ]\n")
    }
    
    # Par defaut tibble donne comme le type logique (binaire) aux variables qui n'ont
    # aucune donnee .. cela est un soucis lors de l'importation.
    
    # # 1iere méthode de resolution du PB ... [on transforme le tout en numeric .. risque de perte d'infos]
    # # Traitement des types lgl qui ne sont pas reconnu comme type lors de l'exportation des donnees
    # # par defaut on les mettra en chaine de caractere
    # varnames <- dico$variable[str_which(dico$col_type,"lgl")]
    # for(varname in varnames){
    #   # cat ("[ Traitement de ",varname," ]\n")
    #   db[,varname] <- as.numeric(db[,varname] %>%  unlist())
    # }
    
    # 2e methode on utilise les faciliter de dplyr et on transforme le tout
    # en chaine de caractere plus prudent..
    # db <- database %>% mutate(across(where(is.logical),as.character))
    # Ou encore, pour recuperer les eventuels labels
    db <- db %>% mutate(across(where(is.logical),to_character))
    
    # Export du resultat
    result <- db
  }
  invisible(result)
}

# Traitement automatique des doublons d'une base de donnees
# On ne retient que les données les plus recentes
processing.duplicateData <- function(database,var_id="hhid",var_time="end"){
  result <- NULL
  if(!is.null(database)){
    result <- database %>% group_by(.data[[var_id]]) %>% 
      arrange(.data[[var_id]],desc(.data[[var_time]])) %>% 
      # In ne retient que les donnees les plus recentes
      slice_head(n=1) #%>% 
      # ci-dessous on conserve tous les doublons.. pas necessaire
      # add_count(.data[[var_id]]) %>% filter(n>1) %>% 
  }
  invisible(result)
}

# Fonction d'extraction des donnees a traiter manuellement depuis un fichier excel
# instructions <- rio::import("./data/treatment_instructions.xlsx",format="xlsx",setclass="tibble",sheet="school")
# getData.toBeProcessed(db_raw,import("./data/treatment_instructions.xlsx",setclass="tibble",sheet="school"))
getData.toBeProcessed <- function(database,instructions,data_treated=NULL,var_id="hhid"){
 result <- NULL
 if(!is.null(database) & !is.null(instructions)){
  
   # Extraction de tous les variables 
   # varlist <- with(instructions,unique(c(question_name[!is.na(question_name)],
   #                                       parent_other_question[!is.na(parent_other_question)])))
   varlist <- with(instructions,unique(question_name[!is.na(question_name)]))
   
   varlist_parent <- NULL
   rowid <- which(!is.na(instructions$parent_other_question))
   if(length(rowid)){
     varlist_parent <- instructions$parent_other_question[rowid]
     names(varlist_parent) <- instructions$question_name[rowid]
   }
   
   for(varname in varlist){
     tmp <- database %>% select(all_of(c(var_id,varname))) %>% 
       mutate(question_name=varname, 
              old_value=as.character(.data[[varname]]) %>% tidyr::replace_na("")) %>%
              # old_value=to_characater(.data[[varname]])) %>%
       select(all_of(c(var_id,"question_name","old_value"))) %>% 
       mutate(new_value="") %>% filter(!is.na(old_value) & old_value!="")
     
     if(varname %in% names(varlist_parent)){
       parent_question <- as.character(varlist_parent[varname])
       subtmp <- database %>% select(all_of(c(var_id,parent_question))) %>% 
         mutate(parent_question_name=parent_question,
                old_parent_question_value=as.character(.data[[parent_question]]),
                new_parent_question_value="") %>% 
         select(all_of(c(var_id,"parent_question_name","old_parent_question_value","new_parent_question_value"))) %>% 
         filter(!is.na(old_parent_question_value) & old_parent_question_value!="")
       tmp <- tmp %>% distinct() %>%  left_join(subtmp %>% distinct(),by=var_id)
     }else{
       tmp <- tmp %>% 
         mutate(parent_question_name="",old_parent_question_value="",new_parent_question_value="")
     }
     #action="remove/change/recode/check",format_result="text/number"
     tmp <- tmp %>% mutate(problem="",action="",format_result="",feedback="")
     result <- bind_rows(result,tmp)
   }
  result <- result %>% arrange(.data[[var_id]])  
  if(!is.null(result) & !is.null(data_treated))
    result <- result %>% distinct() %>%  
    anti_join(data_treated %>% 
                filter(action!="" & !is.na(action)) %>% 
                distinct(),
              by=c(var_id,"question_name","old_value","parent_question_name",
                   "old_parent_question_value"))
 }
 invisible(result)
}

# Traitement des données en suivant les instructions d'un fichier Excel
# Il s'agit principalement de suppression et d'encodage
processing.data <- function(database, xls_processing,caseid="hhid"){
  
  # TEST => change hhid ci-dessous par {{caseid}} en lieu et place de {caseid}
  #check = acun traitement a effectué
  result <-  database
  
  db_instruction <- xls_processing %>% 
    filter(action %in% c("change","recode","remove"),!is.na({{caseid}})) 
  
  # change = modifie l'ancienne valeur de question_name [old_value] par new_value en se
  # se referant au type de la cellule [format_result] pour savoir si on doit caster en chaine de
  # caractere ou en numerique... on peut aussi utiliser class pour s'en assurer .. meilleure
  
  dbtmp <-  db_instruction %>% filter(action=="change")
  for(var_id in unique(pull(dbtmp,{{caseid}}))){
    
    nun_ligne <- which(dbtmp[[caseid]]==var_id)
    varlist <- pull(dbtmp[nun_ligne,],question_name)
    valeur <-  pull(dbtmp[nun_ligne,],new_value,question_name)
    # valeur_type <- pull(dbtmp %>% filter(hhid==var_id),format_result,question_name)
    # row_id <- which(result$hhid==var_id)
    row_id <- which(pull(result,{{caseid}})==var_id)
    cat("\n[change] ",caseid," = ",var_id)
    for(varname in varlist){
      cat("\n[change] > varname = ",varname)
      # result[row_id,varname] <- switch (
      #   class(pull(result[row_id,],varname)),
      #   character = as.character(valeur[varname]),
      #   integer = as.integer(valeur[varname]),
      #   numeric = as.numeric(valeur[varname])
      # )
      valeur_type <- class(pull(result[row_id,],varname))
      if(length(valeur_type)>1){
        if("double" %in% valeur_type) valeur_type <- "double"
        if("integer" %in% valeur_type) valeur_type <- "integer"
        if("character" %in% valeur_type) valeur_type <- "character"
        if("numeric" %in% valeur_type) valeur_type <- "numeric"
      }
      result[row_id,varname] <- switch (
        valeur_type,
        character = as.character(valeur[varname]),
        integer = as.integer(valeur[varname]),
        numeric = as.numeric(valeur[varname]),
        double = as.numeric(valeur[varname])
      )
    }
  }
  
  # recode applique d'abord change et puis modifier l'ancienne valeur de parent_question_name
  # [old_parent_question_value] avec la nouvelle valeur new_parent_question_value en utilisation
  # format_result ou la fonction class
  dbtmp <-  db_instruction %>% filter(action=="recode")
  for(var_id in unique(pull(dbtmp,{{caseid}}))){
    
    # row_id <- which(result$hhid==var_id)
    row_id <- which(pull(result,{{caseid}})==var_id)
    cat("\n[recode] ",caseid," = ",var_id)
    
    # Change valeur pour les variables noeuds
    nun_ligne <- which(dbtmp[[caseid]]==var_id)
    varlist <- pull(dbtmp[nun_ligne,],question_name)
    valeur <-  pull(dbtmp[nun_ligne,],new_value,question_name)
    # valeur_type <- pull(dbtmp %>% filter(hhid==var_id),format_result,question_name)
    
    for(varname in varlist){
      cat("\n[recode] > varname = ",varname)
      # result[row_id,varname] <- switch (
      #   class(pull(result[row_id,],varname)),
      #   character = as.character(valeur[varname]),
      #   integer = as.integer(valeur[varname]),
      #   numeric = as.numeric(valeur[varname])
      # )
      valeur_type <- class(pull(result[row_id,],varname))
      if(length(valeur_type)>1){
        if("double" %in% valeur_type) valeur_type <- "double"
        if("integer" %in% valeur_type) valeur_type <- "integer"
        if("character" %in% valeur_type) valeur_type <- "character"
        if("numeric" %in% valeur_type) valeur_type <- "numeric"
      }
      result[row_id,varname] <- switch (
        valeur_type,
        character = as.character(valeur[varname]),
        integer = as.integer(valeur[varname]),
        numeric = as.numeric(valeur[varname]),
        double = as.numeric(valeur[varname])
      )
    }
    
    # Change valeur pour les variables parent
    nun_ligne <- which(dbtmp[[caseid]]==var_id)
    varlist <- pull(dbtmp[nun_ligne,],parent_question_name)
    valeur <-  pull(dbtmp[nun_ligne,],new_parent_question_value,parent_question_name)
    # varlist <- pull(dbtmp %>% filter(hhid==var_id),parent_question_name)
    # valeur <-  pull(dbtmp %>% filter(hhid==var_id),new_parent_question_value,parent_question_name)
    # valeur_type <- pull(dbtmp %>% filter(hhid==var_id),format_result,parent_question_name)
    for(varname in varlist){
      cat("\n[recode] > varname_parent = ",varname)
      valeur_type <- class(pull(result[row_id,],varname))
      if(length(valeur_type)>1){
        if("double" %in% valeur_type) valeur_type <- "double"
        if("integer" %in% valeur_type) valeur_type <- "integer"
        if("character" %in% valeur_type) valeur_type <- "character"
        if("numeric" %in% valeur_type) valeur_type <- "numeric"
      }
      result[row_id,varname] <- switch (
        valeur_type,
        character = as.character(valeur[varname]),
        integer = as.integer(valeur[varname]),
        numeric = as.numeric(valeur[varname]),
        double = as.numeric(valeur[varname])
      )
    }
  }
  
  #remove/delete = on supprime toute l'observation en se fiant à la colonne var_id
  dbtmp <-  db_instruction %>% filter(action=="remove")
  for(var_id in unique(pull(dbtmp,{{caseid}}))){
    row_id <- which(result[[caseid]]==var_id)
    if(length(rowid)) result <- result %>% slice(-{row_id})
  }
  
  # retourne de la base de donnees apuree
  invisible(result)
}
