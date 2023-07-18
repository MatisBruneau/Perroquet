generateur_figure4 <- function(annee, trimestre, horizon) {
  code_trimestre <- paste0(annee, "-Q", trimestre)
  start_period <- paste0(annee-horizon, "-Q", trimestre)
  
  # récupération des séries
  taux_de_chomage_IDF <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515843?", "endPeriod=", code_trimestre, "&startPeriod=", start_period)))[c("TIME_PERIOD", "OBS_VALUE")]
  taux_de_chomage_France_hors_Mayotte <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001688527?", "endPeriod=", code_trimestre, "&startPeriod=", start_period)))[c("TIME_PERIOD", "OBS_VALUE")]
  
  liste_dataframe = list(taux_de_chomage_IDF, taux_de_chomage_France_hors_Mayotte)
  
  # conversion des dates et données au bon format
  for (i in seq_along(liste_dataframe)) {
    liste_dataframe[[i]]$TIME_PERIOD <- as.yearqtr(liste_dataframe[[i]]$TIME_PERIOD, format = "%Y-Q%q")
    liste_dataframe[[i]]$OBS_VALUE <- as.numeric(liste_dataframe[[i]]$OBS_VALUE)
  }
  
  # mise en forme pour affichage
  taux_de_chomage_IDF <- liste_dataframe[[1]][c("TIME_PERIOD", "OBS_VALUE")]
  taux_de_chomage_IDF$table <- "Taux de chômage en IDF"
  taux_de_chomage_France_hors_Mayotte <- liste_dataframe[[2]][c("TIME_PERIOD", "OBS_VALUE")]
  taux_de_chomage_France_hors_Mayotte$table <- "Taux de chômage en France hors Mayotte"
  
  fig4_df <- rbind(taux_de_chomage_IDF, taux_de_chomage_France_hors_Mayotte)
  
  # graphique
  g <- ggplot(data = fig4_df, aes(y= OBS_VALUE, x=TIME_PERIOD, color = table)) + geom_line() + labs(color = "Légende", title = "Taux de chômage")
  g <- ggplotly(g)
  return(g)
}

####################################Commentaire associé#########################
generateur_chômage <- function(annee, num_trimestre){
  code_trimestre <- paste(annee, "-Q", num_trimestre, sep = "")
  taux_IDF <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515843?lastNobservations=2&endPeriod=", code_trimestre)))[c("TIME_PERIOD", "OBS_VALUE")]
  
  if (taux_IDF$TIME_PERIOD[1] != code_trimestre) {
    return("Pas de données pour ce trimestre.")
  } 
  
  taux_FR <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001688527?lastNobservations=2&endPeriod=", code_trimestre)))[c("TIME_PERIOD", "OBS_VALUE")]
  taux_Paris <-as.numeric(as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515940?lastNobservations=1&endPeriod=", code_trimestre)))[c("OBS_VALUE")][1])
  taux_SM <- as.numeric(as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515942?lastNobservations=1&endPeriod=", code_trimestre)))[c("OBS_VALUE")][1])
  taux_Yves <- as.numeric(as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515943?lastNobservations=1&endPeriod=", code_trimestre)))[c("OBS_VALUE")][1])
  taux_ESS <- as.numeric(as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515955?lastNobservations=1&endPeriod=", code_trimestre)))[c("OBS_VALUE")][1])
  taux_HDS <- as.numeric(as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515956?lastNobservations=1&endPeriod=", code_trimestre)))[c("OBS_VALUE")][1])
  taux_SSD <- as.numeric(as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515957?lastNobservations=1&endPeriod=", code_trimestre)))[c("OBS_VALUE")][1])
  taux_VDM <- as.numeric(as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515958?lastNobservations=1&endPeriod=", code_trimestre)))[c("OBS_VALUE")][1])
  taux_VO <- as.numeric(as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515959?lastNobservations=1&endPeriod=", code_trimestre)))[c("OBS_VALUE")][1])
  taux_IDF$OBS_VALUE <- as.numeric(taux_IDF$OBS_VALUE)
  taux_FR$OBS_VALUE <- as.numeric(taux_FR$OBS_VALUE)
  
  diff_IDF <- round(taux_IDF$OBS_VALUE[1] - taux_IDF$OBS_VALUE[2], digits = 1)
  diff_FR <- round(taux_FR$OBS_VALUE[1] - taux_FR$OBS_VALUE[2], digits = 1)
  
  if (num_trimestre==1){
    numero_trim = "Au premier trimestre de l'année"
  }else if (num_trimestre==2){
    numero_trim="Au deuxième trimestre de l'année"
  }else if ((num_trimestre==3)){
    numero_trim="Au troisième trimestre de l'année"
  }else{
    numero_trim="Au quatrième trimestre de l'année"
  }
  
  
  
  
  if (diff_IDF <=-0.2) {
    evol_IDF = "a diminué"
  }else if (diff_IDF >= 0.2) {
    evol_IDF = "a augmenté"
    diff_IDF <- paste0("+", diff_IDF)
  } else if (diff_IDF >= 0) {
    evol_IDF =  "est resté stable"
    diff_IDF <- paste0("+", diff_IDF)
  } else {
    evol_IDF = "est resté stable"
  }
  
  if (diff_FR <=-0.2) {
    evol_FR = "a diminué"
  }else if (diff_FR >= 0.2) {
    evol_FR = "a augmenté"
    diff_FR <- paste0("+", diff_FR)
  } else if (diff_FR >= 0) {
    evol_FR =  "est resté stable"
    diff_FR <- paste0("+", diff_FR)
  } else {
    evol_FR = "est resté stable"
  }
  
  phrase_IDF <- paste(numero_trim, annee, ", le taux de chômage en Ile-de-France", 
                      evol_IDF, "(", diff_IDF, "point), à", 
                      taux_IDF$OBS_VALUE[1], "%.", sep = " ")
  
  phrase_FR <- paste("En France (hors Mayotte), le taux de chômage", evol_FR, "("
                     , diff_FR,
                     "point), à", taux_FR$OBS_VALUE[1],"%.")
  
  
  
  taux_Paris <- as.numeric(as.data.frame(readSDMX("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515940?lastNobservations=1"))[c("OBS_VALUE")][1])
  
  taux_classes <- c(taux_Paris, taux_SM, taux_ESS, taux_HDS, taux_SSD, taux_VDM, taux_VO, taux_Yves)
  noms_departements <- c("Paris", "la Seine et Marne", "l'Essonne", "les Hauts de Seine", "la Seine Saint-Denis", "le Val-de-Marne", "le Val d'Oise", "les Yvelines")
  
  departements_df <- as.data.frame(cbind(noms_departements, taux_classes))
  departements_df$taux_classes <- as.numeric(departements_df$taux_classes)
  departements_df<- departements_df[order(departements_df$taux_classes),]
  
  phrase_dep <- paste("Au sein de la région, le taux de chômage est le plus bas dans", departements_df$noms_departements[1],
                      "à", departements_df$taux_classes[1], "%, suivi par", departements_df$noms_departements[2], "(",
                      departements_df$taux_classes[2], "%),", departements_df$noms_departements[3], "(",
                      departements_df$taux_classes[3], "%) et", departements_df$noms_departements[4], "(", 
                      departements_df$taux_classes[4], "%). Viennent ensuite", departements_df$noms_departements[5],
                      "(", departements_df$taux_classes[5], "%) et", departements_df$noms_departements[6],
                      "(", departements_df$taux_classes[6], "%). Le taux de chômage reste le plus élevé dans", 
                      departements_df$noms_departements[7], "(", departements_df$taux_classes[7], "%) et",
                      departements_df$noms_departements[8], "(", departements_df$taux_classes[8], "%)." )
  
  return(paste(phrase_IDF, phrase_FR, phrase_dep))
}

generateur_table_chomage <- function(annee, trimestre, horizon) {
  code_trimestre <- paste(annee, "-Q", trimestre, sep = "")
  horizon <- as.numeric(horizon)*4
  
  taux_FR <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001688527?lastNobservations=", horizon, "&endPeriod=", code_trimestre)))[c("TIME_PERIOD", "OBS_VALUE")]
  taux_IDF <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515843?lastNobservations=", horizon, "&endPeriod=", code_trimestre)))[c("OBS_VALUE")]
  taux_Paris <-as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515940?lastNobservations=", horizon, "&endPeriod=", code_trimestre)))[c("OBS_VALUE")]
  taux_SM <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515942?lastNobservations=", horizon, "&endPeriod=", code_trimestre)))[c("OBS_VALUE")]
  taux_Yves <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515943?lastNobservations=", horizon, "&endPeriod=", code_trimestre)))[c("OBS_VALUE")]
  taux_ESS <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515955?lastNobservations=", horizon, "&endPeriod=", code_trimestre)))[c("OBS_VALUE")]
  taux_HDS <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515956?lastNobservations=", horizon, "&endPeriod=", code_trimestre)))[c("OBS_VALUE")]
  taux_SSD <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515957?lastNobservations=", horizon, "&endPeriod=", code_trimestre)))[c("OBS_VALUE")]
  taux_VDM <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515958?lastNobservations=", horizon, "&endPeriod=", code_trimestre)))[c("OBS_VALUE")]
  taux_VO <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001515959?lastNobservations=", horizon, "&endPeriod=", code_trimestre)))[c("OBS_VALUE")]
  
  nom_colonnes <- c("Trimestre", "France hors Mayotte", "Île-de-France", "Paris", "Seine-et-Marne", "Yvelines", "Essonne", "Hauts-de-Seine", "Seine-Saint-Denis", "Val-de-Marne", "Val-d'Oise")
  
  table <- as.data.frame(cbind(taux_FR, taux_IDF, taux_Paris, taux_SM, taux_Yves, taux_ESS, taux_HDS, taux_SSD, taux_VDM, taux_VO))
  names(table) <- nom_colonnes
  return(table)
}

### EMPLOI ###
##################################Graphique#####################################
generateur_figure2 <- function(annee, trimestre, horizon) {
  code_trimestre <- paste0(annee, "-Q", trimestre)
  start_period <- paste0(annee-horizon, "-Q", trimestre)
  # récupération des séries
  emploi_salarie_total_IDF <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567988?", "endPeriod=", code_trimestre, "&startPeriod=", start_period)))[c("TIME_PERIOD", "OBS_VALUE")]
  emploi_salarie_total_France_hors_Mayotte <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010568001?", "endPeriod=", code_trimestre, "&startPeriod=", start_period)))[c("TIME_PERIOD", "OBS_VALUE")]
  emploi_salarie_prive_IDF <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010568006?", "endPeriod=", code_trimestre, "&startPeriod=", start_period)))[c("TIME_PERIOD", "OBS_VALUE")]
  emploi_salarie_prive_France_hors_Mayotte <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010568019?", "endPeriod=", code_trimestre, "&startPeriod=", start_period)))[c("TIME_PERIOD", "OBS_VALUE")]
  
  liste_dataframe = list(emploi_salarie_total_IDF, emploi_salarie_total_France_hors_Mayotte, emploi_salarie_prive_IDF, emploi_salarie_prive_France_hors_Mayotte)
  
  # conversion des dates et données au bon format et en base 100
  for (i in seq_along(liste_dataframe)) {
    liste_dataframe[[i]]$TIME_PERIOD <- as.yearqtr(liste_dataframe[[i]]$TIME_PERIOD, format = "%Y-Q%q")
    liste_dataframe[[i]]$OBS_VALUE <- as.numeric(liste_dataframe[[i]]$OBS_VALUE)
    liste_dataframe[[i]]$BASE_100 <- 100 * liste_dataframe[[i]]$OBS_VALUE / tail(liste_dataframe[[i]]$OBS_VALUE, n=1)
  }
  
  #mise en forme pour affichage
  emploi_salarie_total_IDF <- liste_dataframe[[1]][c("TIME_PERIOD", "BASE_100")]
  emploi_salarie_total_IDF$table <- "Emploi salarié total IDF"
  emploi_salarie_total_France_hors_Mayotte <- liste_dataframe[[2]][c("TIME_PERIOD", "BASE_100")]
  emploi_salarie_total_France_hors_Mayotte$table <- "Emploi salarié total France hors Mayotte"
  emploi_salarie_prive_IDF <- liste_dataframe[[3]][c("TIME_PERIOD", "BASE_100")]
  emploi_salarie_prive_IDF$table <- "Emploi salarié privé IDF"
  emploi_salarie_prive_France_hors_Mayotte <- liste_dataframe[[4]][c("TIME_PERIOD", "BASE_100")]
  emploi_salarie_prive_France_hors_Mayotte$table <- "Emploi salarié privé France hors Mayotte"
  
  fig2_df <- rbind(emploi_salarie_total_IDF, emploi_salarie_total_France_hors_Mayotte, emploi_salarie_prive_IDF, emploi_salarie_prive_France_hors_Mayotte)
  
  # graphique
  g <- ggplot(data = fig2_df, aes(x = TIME_PERIOD, y = BASE_100, color = table)) + geom_line() + geom_hline(yintercept = 100) + labs(color = "Légende", title = "Emploi salarié total")
  return(g)
}

generateur_figure3 <- function(annee, trimestre, horizon) {
  code_trimestre <- paste0(annee, "-Q", trimestre)
  start_period <- paste0(annee-horizon, "-Q", trimestre)
  
  #récupération des séries
  emploi_salarie_construction <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567304?", "endPeriod=", code_trimestre, "&startPeriod=", start_period)))[c("TIME_PERIOD", "OBS_VALUE")]
  emploi_salarie_industrie <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/001737420?", "endPeriod=", code_trimestre, "&startPeriod=", start_period)))[c("TIME_PERIOD", "OBS_VALUE")]
  emploi_salarie_tertiaire_non_marchand <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567646?", "endPeriod=", code_trimestre, "&startPeriod=", start_period)))[c("TIME_PERIOD", "OBS_VALUE")]
  
  liste_dataframe = list(emploi_salarie_construction, emploi_salarie_industrie, emploi_salarie_tertiaire_non_marchand)
  
  # conversion des dates et données au bon format et en base 100
  for (i in seq_along(liste_dataframe)) {
    liste_dataframe[[i]]$TIME_PERIOD <- as.yearqtr(liste_dataframe[[i]]$TIME_PERIOD, format = "%Y-Q%q")
    liste_dataframe[[i]]$OBS_VALUE <- as.numeric(liste_dataframe[[i]]$OBS_VALUE)
    liste_dataframe[[i]]$BASE_100 <- 100 * liste_dataframe[[i]]$OBS_VALUE / tail(liste_dataframe[[i]]$OBS_VALUE, n=1)
  }
  
  # mise en forme pour affichage
  emploi_salarie_construction <- liste_dataframe[[1]][c("TIME_PERIOD", "BASE_100")]
  emploi_salarie_construction$table <- "Emploi salarié construction"
  emploi_salarie_industrie <- liste_dataframe[[2]][c("TIME_PERIOD", "BASE_100")]
  emploi_salarie_industrie$table <- "Emploi salarié industrie"
  emploi_salarie_tertiaire_non_marchand <- liste_dataframe[[3]][c("TIME_PERIOD", "BASE_100")]
  emploi_salarie_tertiaire_non_marchand$table <- "Emploi salarié tertiaire non marchand"
  
  fig3_df <- rbind(emploi_salarie_construction, emploi_salarie_industrie, emploi_salarie_tertiaire_non_marchand)
  
  # graphique
  g <- ggplot(data = fig3_df, aes(x = TIME_PERIOD, y = BASE_100, color = table)) + geom_line() + geom_hline(yintercept = 100) + labs(color = "Légende", title = "Emploi salarié par secteur en IDF")
  return(g)
}

generateur_figure2_figure3 <- function(annee, trimestre, horizon){
  fig2 <- ggplotly(generateur_figure2(annee, trimestre, horizon))
  fig3 <- ggplotly(generateur_figure3(annee, trimestre, horizon))
  g <- subplot(list(fig2, fig3), nrows = 2)
  return(g)
}

############################Commentaires########################################
### Récupération des Données ###
nom_depart <- c("Île-de-France", "Paris", "la Seine-et-Marne", "les Yvelines", "l'Essonne", "les Hauts-de-Seine", "la Seine-Saint-Denis", "Val-de-Marne", "Val-d'Oise","France-Hors-Mayotte")



generateur_emplois <- function(annee, trimestre){
  nom_depart <- c("Île-de-France", "Paris", "la Seine-et-Marne", "les Yvelines", "l'Essonne", "les Hauts-de-Seine", "la Seine-Saint-Denis", "Val-de-Marne", "Val-d'Oise","France-Hors-Mayotte")
  
  code_trimestre <- paste0(annee, "-Q", trimestre)
  # récupération des séries
  #code_trimestre <- "2022-Q4"
  
  emploi_IDF <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567988?", "endPeriod=", code_trimestre, "&lastNobservations=5")))[c("TIME_PERIOD", "OBS_VALUE")]
  
  if (emploi_IDF$TIME_PERIOD[1] != code_trimestre) {
    return("Pas de données pour ce trimestre.")
  }
  
  emploi_Paris <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567965?", "endPeriod=", code_trimestre, "&lastNobservations=2")))[c("OBS_VALUE")]
  emploi_SM <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567967?", "endPeriod=", code_trimestre, "&lastNobservations=2")))[c("OBS_VALUE")]
  emploi_YV <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567968?", "endPeriod=", code_trimestre, "&lastNobservations=2")))[c("OBS_VALUE")]
  emploi_ES <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567981?", "endPeriod=", code_trimestre, "&lastNobservations=2")))[c("OBS_VALUE")]
  emploi_HS <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567982?", "endPeriod=", code_trimestre, "&lastNobservations=2")))[c("OBS_VALUE")]
  emploi_SSD <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567983?", "endPeriod=", code_trimestre, "&lastNobservations=2")))[c("OBS_VALUE")]
  emploi_VM <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567984?", "endPeriod=", code_trimestre, "&lastNobservations=2")))[c("OBS_VALUE")]
  emploi_VO <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567985?", "endPeriod=", code_trimestre, "&lastNobservations=2")))[c("OBS_VALUE")]
  emploi_France_hors_Mayotte <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010568001?", "endPeriod=", code_trimestre, "&lastNobservations=5")))[c("OBS_VALUE")]
  
  nb_emplois <- as.data.frame(cbind(emploi_IDF[c(1,2),], emploi_Paris, emploi_SM, emploi_YV, emploi_ES, emploi_HS, emploi_SSD, emploi_VM, emploi_VO, emploi_France_hors_Mayotte[c(1,2),]))
  nb_emplois_act <- as.numeric(nb_emplois[1,][-1])
  nb_emplois_av <- as.numeric(nb_emplois[2,][-1])
  
  diff <- nb_emplois_act-nb_emplois_av
  diff_pourcent <- round(((diff)/nb_emplois_av)*100, digits=1)
  
  diff_an_pourcent_IDF <- round((as.numeric(emploi_IDF$OBS_VALUE[1])-as.numeric(emploi_IDF$OBS_VALUE[5]))/as.numeric(emploi_IDF$OBS_VALUE[5])*100, digits=1)
  diff_an_pourcent_FR <-round((as.numeric(emploi_France_hors_Mayotte$OBS_VALUE[1])-as.numeric(emploi_France_hors_Mayotte$OBS_VALUE[5]))/as.numeric(emploi_France_hors_Mayotte$OBS_VALUE[5])*100, digits=1)
  
  emplois_df <- as.data.frame(cbind(nom_depart, nb_emplois_act, diff, diff_pourcent))
  names(emplois_df) <- c("nom_dep", "nb_emplois_act", "diff", "diff_pourcent")
  emplois_df$diff <- as.numeric(emplois_df$diff)
  emplois_df$diff_pourcent <- as.numeric(emplois_df$diff_pourcent)
  emplois_df$nb_emplois <- as.numeric(emplois_df$nb_emplois)
  
  # Texte
  if (trimestre==1){
    numero_trim = "Au premier trimestre de l'année"
  }else if (trimestre==2){
    numero_trim="Au deuxième trimestre de l'année"
  }else if ((trimestre==3)){
    numero_trim="Au troisième trimestre de l'année"
  }else{
    numero_trim="Au quatrième trimestre de l'année"
  }
  
  diff_IDF_FRANCE <- abs(as.numeric(emplois_df$diff_pourcent[1])-as.numeric(emplois_df$diff_pourcent[10]))
  if (diff_IDF_FRANCE>0){
    text_IDF_FRANCE = "supérieur"
  }else if (diff_IDF_FRANCE<0){
    text_IDF_FRANCE = "inférieur"
  }else{
    text_IDF_FRANCE = "similaire"
  }
  
  
  if (emplois_df[1,"diff_pourcent"] <=-0.2) {
    evol_IDF = "a diminué"
  }else if (emplois_df[1,"diff_pourcent"] >= 0.2) {
    evol_IDF = "a augmenté"
    emplois_df[1,"diff_pourcent"] <- paste0("+", emplois_df[1,"diff_pourcent"])
  } else if (emplois_df[1,"diff_pourcent"] >= 0) {
    evol_IDF =  "est resté stable"
    emplois_df[1,"diff_pourcent"] <- paste0("+", emplois_df[1,"diff_pourcent"])
  } else {
    evol_IDF = "est resté stable"
  }
  
  
  if (abs(emplois_df[1,"diff_pourcent"] <=0.4) &  abs(emplois_df[1,"diff_pourcent"] > 0.2)) {
    detail = " légèrement "
  }else {
    detail =""
  }
  
  if (emplois_df[1,"diff"] >=0) {
    emplois_df[1,"diff"]<- paste0("+", emplois_df[1,"diff"])
  }
  
  if (all((emplois_df$diff > 0), na.rm = FALSE)){
    evolute = "progresse dans tous les départements."
  }else if (all((emplois_df$diff < 0), na.rm = FALSE)) {
    evolute = "régresse dans tous les départements."
  }else{
    evolute = "évolue de façon disparate au niveau départemental."
  }
  
  
  
  
  phrase_1 <- paste0(numero_trim, " ", annee, " l’emploi salarié francilien ", evol_IDF, detail,
                     " (", emplois_df$diff_pourcent[1]," %)" ," par rapport au trimestre précédent ",
                     "( ",emplois_df$diff[1]," emplois)", " soit un rythme ",text_IDF_FRANCE," à celui de la France hors Mayotte ",
                     "(",emplois_df$diff_pourcent[10]," %).")
  
  
  phrase_2 <- paste0(" Sur un an, l'évolution en Île-de-France est de ", diff_an_pourcent_IDF, " %", " ( et de ", diff_an_pourcent_FR, " % au niveau national).")
  
  phrase_3 <- paste0(" L’emploi ",evolute," En effet, il change de  ", emplois_df$diff_pourcent[2], 
                     " point à Paris et de ", emplois_df$diff_pourcent[6], " dans les Hauts-de-Seine. En Seine-et-Marne, il varie de ", emplois_df$diff_pourcent[3], 
                     " point et de ", emplois_df$diff_pourcent[4]," point les Yvelines. En Essonne, il évolue de ", emplois_df$diff_pourcent[5],
                     " point, tandis qu'en Seine-Saint-Denis, le changement est de ", emplois_df$diff_pourcent[7], " point. Dans le Val-de-Marne, il évolue de ", 
                     emplois_df$diff_pourcent[8], " point et dans le Val-d'Oise, de ", emplois_df$diff_pourcent[9], ".") 
  
  
  return(paste0(phrase_1, phrase_2, phrase_3))
  
}

generateur_table_emploi <- function(annee, trimestre, horizon){
  code_trimestre <- paste0(annee, "-Q", trimestre)
  horizon <- as.numeric(horizon)*4
  emploi_IDF <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567988?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  emploi_Paris <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567965?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  emploi_SM <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567967?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  emploi_YV <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567968?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  emploi_ES <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567981?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  emploi_HS <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567982?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  emploi_SSD <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567983?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  emploi_VM <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567984?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  emploi_VO <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010567985?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  emploi_FR <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010568001?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("TIME_PERIOD", "OBS_VALUE")]
  
  nom_colonnes <- c("Trimestre", "France hors Mayotte", "Île-de-France", "Paris", "Seine-et-Marne", "Yvelines", "Essonne", "Hauts-de-Seine", "Seine-Saint-Denis", "Val-de-Marne", "Val-d'Oise")
  
  table <- as.data.frame(cbind(emploi_FR, emploi_IDF, emploi_Paris, emploi_SM, emploi_YV, emploi_ES, emploi_HS, emploi_SSD, emploi_VM, emploi_VO))
  names(table) <- nom_colonnes
  return(table)
}

### DEMANDEURS EMPLOI ###

generateur_demandeurs_d_emploi <- function(annee, trimestre) {
  code_trimestre <- paste0(annee, "-Q", trimestre)
  
  A_FR <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010597456?", "endPeriod=", code_trimestre, "&lastNobservations=5")))[c("TIME_PERIOD", "OBS_VALUE")]
  
  if (A_FR$TIME_PERIOD[1] != code_trimestre) {
    return("Pas de données pour ce trimestre.")
  }
  
  A_FR <- as.data.frame(A_FR$OBS_VALUE)
  
  A_IDF <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010597442?", "endPeriod=", code_trimestre, "&lastNobservations=5")))[c("OBS_VALUE")]
  
  ABC_IDF <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010597557?", "endPeriod=", code_trimestre, "&lastNobservations=5")))[c("OBS_VALUE")]
  ABC_FR <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010597571?", "endPeriod=", code_trimestre, "&lastNobservations=2")))[c("OBS_VALUE")]
  
  ABC_1_ou_plus_IDF <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010598248?", "endPeriod=", code_trimestre, "&lastNobservations=2")))[c("OBS_VALUE")]
  
  demandeurs_df <- as.data.frame(list(as.numeric(A_IDF[1,]), as.numeric(ABC_IDF[1,]), as.numeric(ABC_1_ou_plus_IDF[1,])))
  names(demandeurs_df) <- c("A", "ABC", "ABC_1_ou_plus")
  demandeurs_df$BC <- demandeurs_df$ABC - demandeurs_df$A
  demandeurs_df$ABC_moins_1 <- demandeurs_df$ABC - demandeurs_df$ABC_1_ou_plus
  demandeurs_df$diff_pourcent_A <- round(100 * (demandeurs_df$A - as.numeric(A_IDF[2,])) / as.numeric(A_IDF[2,]), digits = 1)
  demandeurs_df$diff_pourcent_ABC <- round(100 * (demandeurs_df$ABC - as.numeric(ABC_IDF[2,])) / as.numeric(ABC_IDF[2,]), digits = 1)
  demandeurs_df$diff_pourcent_an_ABC <- round(100 * (demandeurs_df$ABC- as.numeric(ABC_IDF[5,])) / as.numeric(ABC_IDF[5,]), digits = 1)
  BC_IDF_avant <- as.numeric(ABC_IDF[2,]) - as.numeric(A_IDF[2,])
  demandeurs_df$diff_pourcent_BC <- round(100 * (demandeurs_df$BC - BC_IDF_avant) / BC_IDF_avant)
  demandeurs_df$diff_pourcent_an_A_IDF <- round(100 * (demandeurs_df$A - as.numeric(A_IDF[5,])) / as.numeric(A_IDF[5,]))
  demandeurs_df$diff_pourcent_ABC_1_an_ou_plus <- round(100 * (demandeurs_df$ABC_1_ou_plus - as.numeric(ABC_1_ou_plus_IDF[2,])) / as.numeric(ABC_1_ou_plus_IDF[2,]), digits = 1)
  ABC_moins_1_avant <- as.numeric(ABC_IDF[2,]) - as.numeric(ABC_1_ou_plus_IDF[2,])
  demandeurs_df$diff_pourcent_ABC_moins_1 <- round(100 * (demandeurs_df$ABC_moins_1 - ABC_moins_1_avant) / ABC_moins_1_avant, digits = 1)
  
  diff_pourcent_A_FR <- round(100 * (as.numeric(A_FR[1,]) - as.numeric(A_FR[2,])) / as.numeric(A_FR[2,]))
  diff_pourcent_an_A_FR <- round(100 * (as.numeric(A_FR[1,]) - as.numeric(A_FR[5,])) / as.numeric(A_FR[5,]), digits = 1)
  
  # Texte
  if (trimestre==1){
    numero_trim = "Au premier trimestre de l'année"
  }else if (trimestre==2){
    numero_trim="Au deuxième trimestre de l'année"
  }else if ((trimestre==3)){
    numero_trim="Au troisième trimestre de l'année"
  }else{
    numero_trim="Au quatrième trimestre de l'année"
  }
  
  # Comparaison 1
  if (demandeurs_df$diff_pourcent_A >=0.2) {
    variation_IDF = "augmente"
  }else if (demandeurs_df$diff_pourcent_A <= -0.2){
    variation_IDF = "diminue"
    
  }else{
    variation_IDF = "reste stable"
  }
  
  
  # Comparaison 2
  if (demandeurs_df$diff_pourcent_BC >=0.2) {
    variation_BC = "augmente"
  }else if (demandeurs_df$diff_pourcent_BC <= -0.2){
    variation_BC = "diminue"
    
  }else{
    variation_BC = "reste stable"
  }
  
  
  # Nuance 
  if (abs(demandeurs_df$diff_pourcent_BC >=2)){
    nuance = " nettement"
  }else{
    nuance = ""
  }
  
  # Augmente Abc
  if (demandeurs_df$diff_pourcent_ABC >= 0.2){
    variation_ABC = "augmente"} else if (demandeurs_df$diff_pourcent_ABC <= 0.2) {
      variation_ABC = "diminue"
    } else {
      variation_ABC = "reste stable"
    }
  
  # evolution ABC 1 an ou plus
  if (demandeurs_df$diff_pourcent_ABC_1_an_ou_plus >= 0.2){
    variation_ABC_1_an_ou_plus = "augmente"} else if (demandeurs_df$diff_pourcent_ABC_1_an_ou_plus <= 0.2) {
      variation_ABC_1_an_ou_plus = "diminue"
    } else {
      variation_ABC_1_an_ou_plus = "reste stable"
    }
  
  # evolution ABC moins 1 an
  if (demandeurs_df$diff_pourcent_ABC_moins_1 >= 0.2){
    variation_ABC_moins_1 = "augmente"} else if (demandeurs_df$diff_pourcent_ABC_moins_1 <= 0.2) {
      variation_ABC_moins_1 = "diminue"
    } else {
      variation_ABC_moins_1 = "reste stable"
    }
  
  
  
  
  phrase_1 <- paste0(numero_trim, " ", annee, " en Île-de-France, le nombre de demandeurs d’emploi tenus de rechercher un emploi et sans activité (catégorie A) s’établit en moyenne à ", demandeurs_df$A, " personnes. Il ", 
                     variation_IDF, " de ", abs(demandeurs_df$diff_pourcent_A), " % par rapport au trimestre précédent alors qu'en France hors Mayotte, l'évolution est de ", diff_pourcent_A_FR, " %. Sur une année, l'évolution s'établit à ", demandeurs_df$diff_pourcent_an_A_IDF, " % (", diff_pourcent_an_A_FR, " % au niveau national)." 
  )
  
  phrase_2 <- paste0("Le nombre de demandeurs d’emploi en activité réduite (catégories B et C) ",variation_BC, nuance, " (", demandeurs_df$diff_pourcent_BC, 
                     " %).Au total, le nombre de demandeurs d’emploi (catégories A, B et C) ", variation_ABC, " de",
                     demandeurs_df$diff_pourcent_ABC, " % sur le trimestre (", demandeurs_df$diff_pourcent_an_ABC, ")."  )
  
  phrase_3 <- paste0("
                     Enfin, le nombre de demandeurs d'emploi inscrits depuis un an ou plus ", variation_ABC_1_an_ou_plus, 
                     " (", demandeurs_df$diff_pourcent_ABC_1_an_ou_plus, " %), tandis que le nombre d'inscrits depuis moins d'un an ",
                     variation_ABC_moins_1, "  (", demandeurs_df$diff_pourcent_ABC_moins_1, " %).")
  
  return(paste(phrase_1, phrase_2, phrase_3))
}

### Tableau - Application ###
generateur_table_demandeurs_d_emploi <- function(annee, trimestre, horizon) {
  code_trimestre <- paste0(annee, "-Q", trimestre)
  horizon <- as.numeric(horizon)*4
  
  A_FR <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010597456?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  A_IDF <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010597442?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  ABC_IDF <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010597557?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  ABC_FR <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010597571?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("TIME_PERIOD", "OBS_VALUE")]
  
  table <- as.data.frame(cbind(ABC_FR, ABC_IDF, A_FR, A_IDF))
  names(table) <- c("Trimestre", "ABC_FR", "ABC_IDF", "A_FR", "A_IDF")
  table$BC_IDF <- as.numeric(table$ABC_IDF) - as.numeric(table$A_IDF)
  table$BC_FR <- as.numeric(table$ABC_FR) - as.numeric(table$A_FR)
  
  names(table) <- c("Trimestre", "Catégories ABC - France hors Mayotte", "Catégories ABC - IDF", "Catégorie A - France hors Mayotte", "Catégorie A - IDF", "Catégories BC - France hors Mayotte", "Catégories BC - IDF")
  return(table)
}

### Création d'entreprises ###
# commentaire
generateur_creation_entreprise <- function(annee, trimestre){
  code_trimestre <- paste0(annee, "-Q", trimestre)
  total_IDF <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010756498?", "endPeriod=", code_trimestre, "&lastNobservations=5")))[c("TIME_PERIOD","OBS_VALUE")]
  
  if (total_IDF$TIME_PERIOD[1] != code_trimestre) {
    return("Pas de données pour ce trimestre.")
  }
  
  sans_micro_IDF <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010756192?", "endPeriod=", code_trimestre, "&lastNobservations=5")))[c("OBS_VALUE")]
  total_FR <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010756085?", "endPeriod=", code_trimestre, "&lastNobservations=5")))[c("OBS_VALUE")]
  sans_micro_FR <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010756115?", "endPeriod=", code_trimestre, "&lastNobservations=5")))[c("OBS_VALUE")]
  
  crea_entreprises_df <- as.data.frame(cbind(total_IDF, sans_micro_IDF, total_FR, sans_micro_FR))
  names(crea_entreprises_df) <- c("Trimestre", "total_IDF", "sans_micro_IDF", "total_FR", "sans_micro_FR")
  
  diff_total_IDF <- as.numeric(crea_entreprises_df$total_IDF[1]) - as.numeric(crea_entreprises_df$total_IDF[2])
  diff_pourcent_total_IDF <- 100 * diff_total_IDF / as.numeric(crea_entreprises_df$total_IDF[2])
  diff_an_total_IDF <- as.numeric(crea_entreprises_df$total_IDF[1]) - as.numeric(crea_entreprises_df$total_IDF[5])
  diff_an_pourcent_total_IDF <- 100 * diff_an_total_IDF / as.numeric(crea_entreprises_df$total_IDF[5])
  
  diff_total_FR <- as.numeric(crea_entreprises_df$total_FR[1]) - as.numeric(crea_entreprises_df$total_FR[2])
  diff_pourcent_total_FR <- 100 * diff_total_FR / as.numeric(crea_entreprises_df$total_FR[2])
  diff_an_total_FR <- as.numeric(crea_entreprises_df$total_FR[1]) - as.numeric(crea_entreprises_df$total_FR[5])
  diff_an_pourcent_total_FR <- 100 * diff_an_total_FR / as.numeric(crea_entreprises_df$total_FR[5])
  
  diff_sans_micro_IDF <- as.numeric(crea_entreprises_df$sans_micro_IDF[1]) - as.numeric(crea_entreprises_df$sans_micro_IDF[2])
  diff_avec_micro_IDF_pourcent <- 100* ((as.numeric(crea_entreprises_df$total_IDF[1])-as.numeric(crea_entreprises_df$sans_micro_IDF[1])) - (as.numeric(crea_entreprises_df$total_IDF[2])-as.numeric(crea_entreprises_df$sans_micro_IDF[2]))/(as.numeric(crea_entreprises_df$total_IDF[2])-as.numeric(crea_entreprises_df$sans_micro_IDF[2])))
  diff_pourcent_sans_micro_IDF <- 100 * diff_sans_micro_IDF / as.numeric(crea_entreprises_df$sans_micro_IDF[2])
  diff_an_sans_micro_IDF <- as.numeric(crea_entreprises_df$sans_micro_IDF[1]) - as.numeric(crea_entreprises_df$sans_micro_IDF[5])
  diff_an_pourcent_sans_micro_IDF <- 100 * diff_an_sans_micro_IDF / as.numeric(crea_entreprises_df$sans_micro_IDF[5])
  
  
  diff_sans_micro_FR <- as.numeric(crea_entreprises_df$sans_micro_FR[1]) - as.numeric(crea_entreprises_df$sans_micro_FR[2])
  diff_avec_micro_FR_pourcent <- 100* ((as.numeric(crea_entreprises_df$total_FR[1])-as.numeric(crea_entreprises_df$sans_micro_FR[1])) - (as.numeric(crea_entreprises_df$total_FR[2])-as.numeric(crea_entreprises_df$sans_micro_FR[2]))/(as.numeric(crea_entreprises_df$total_FR[2])-as.numeric(crea_entreprises_df$sans_micro_FR[2])))
  diff_pourcent_sans_micro_FR <- 100 * diff_sans_micro_FR / as.numeric(crea_entreprises_df$sans_micro_FR[2])
  diff_an_sans_micro_FR <- as.numeric(crea_entreprises_df$sans_micro_FR[1]) - as.numeric(crea_entreprises_df$sans_micro_FR[5])
  diff_an_pourcent_sans_micro_FR <- 100 * diff_an_sans_micro_FR / as.numeric(crea_entreprises_df$sans_micro_FR[5])
  
  pourcent_crea_entreprise <- 100*((as.numeric(crea_entreprises_df$total_IDF[1])-as.numeric(crea_entreprises_df$sans_micro_IDF[1]))/as.numeric(crea_entreprises_df$total_IDF[1]))
  # Texte
  if (trimestre==1){
    numero_trim = "Au premier trimestre de l'année "
  }else if (trimestre==2){
    numero_trim="Au deuxième trimestre de l'année "
  }else if ((trimestre==3)){
    numero_trim="Au troisième trimestre de l'année "
  }else{
    numero_trim="Au quatrième trimestre de l'année "
  }
  
  # Comparaison 1
  if (diff_pourcent_total_IDF >=0.2) {
    variation_IDF = "augmente"
  }else if (diff_pourcent_total_IDF <= -0.2){
    variation_IDF = "diminue"
    
  }else{
    variation_IDF = "reste stable"
  }
  
  # Comparaison 2
  if (diff_pourcent_total_IDF >=0){
    evol = "hausse"
  }else{
    evol = "baisse"
  }
  # Comparaison rythme
  if (diff_an_pourcent_total_FR < diff_pourcent_total_IDF){
    nuance = "à un rythme plus élevé"
  }else if (diff_pourcent_total_IDF < diff_an_pourcent_total_FR) {
    nuance = "à un rythme plus faible"
  }else{
    nuance = "au même rythme"
  }
  
  # Somme des créations d'entreprise
  somme = 0
  for (i in 1:length(total_IDF$OBS_VALUE)){
    somme = somme + as.numeric(total_IDF$OBS_VALUE[i])
  }
  
  # Micro ou non 
  if (abs(diff_avec_micro_IDF_pourcent -diff_avec_micro_FR_pourcent) <= 0.2){
    Comp = "Dans la région comme au niveau national"
  }else if ((diff_avec_micro_IDF_pourcent -diff_avec_micro_FR_pourcent > 0.2)){
    Comp = "Au niveau régional"
  }else{
    Comp = "Au niveau national"
  }
  
  # Comparaison 3
  if (diff_an_pourcent_total_IDF <= 0.2){
    valuation = "quasi stables"
  }else{
    valuation = "peu stables"
  }
  
  # Comparaison 4
  if (diff_an_pourcent_total_FR <= 0.2){
    valuation2 = "quasi stable"
  }else{
    valuation2 = "peu stable"
  }
  
  phrase_1 <- paste0(numero_trim, annee, ", ",  crea_entreprises_df$total_IDF[1], " entreprises ont été créées en Île-de-France. Par rapport au trimestre précédent, le nombre global de créations dans la région ", variation_IDF, " à ",
                     round(diff_pourcent_total_FR, digits=1), " % , c'est-à-dire ", nuance, " que le niveau national. Le nombre total de créations en Île-de-France s’élève ainsi à ", somme, ". ")
  
  phrase_2 <- paste0(Comp," ,la ", evol, " est portée par les créations sous le régime de micro-entrepreneur (", round(diff_avec_micro_IDF_pourcent,digits=1), " pour le niveau régional et ", round(diff_avec_micro_FR_pourcent, digits=1), " au niveau national.) Les créations des entreprises classiques sont ", valuation,
                     "( ", round(diff_an_pourcent_total_IDF,digits=1),
                     " %) en Île-de-France et au niveau national elles sont ", valuation2, " avec ", round(diff_an_pourcent_total_FR,digits=1), " %. Ainsi ", round(pourcent_crea_entreprise, digits=1), " % des nouvelles entreprises ont été créées sous le régime de micro-entrepreneur ce trimestre.")
  
  phrase_3 <- paste0("Par rapport au trimestre précédent, le nombre de créations d'entreprises ")
  
  return(paste0(phrase_1,phrase_2))
}

#graphique
generateur_figure5 <- function(annee, trimestre, horizon) {
  code_trimestre <- paste0(annee, "-Q", trimestre)
  start_period <- paste0(annee-horizon, "-Q", trimestre)
  
  IDF_hors_micro <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010756192?", "endPeriod=", code_trimestre, "&startPeriod=", start_period)))[c("TIME_PERIOD", "OBS_VALUE")]
  France_hors_micro <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010756115?", "endPeriod=", code_trimestre, "&startPeriod=", start_period)))[c("TIME_PERIOD", "OBS_VALUE")]
  IDF_avec_micro <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010756498?", "endPeriod=", code_trimestre, "&startPeriod=", start_period)))[c("TIME_PERIOD", "OBS_VALUE")]
  France_avec_micro <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010756085?", "endPeriod=", code_trimestre, "&startPeriod=", start_period)))[c("TIME_PERIOD", "OBS_VALUE")]
  
  liste_dataframe = list(IDF_hors_micro, France_hors_micro, IDF_avec_micro, France_avec_micro)
  
  # conversion des dates et données au bon format et en base 100
  for (i in seq_along(liste_dataframe)) {
    liste_dataframe[[i]]$TIME_PERIOD <- as.yearqtr(liste_dataframe[[i]]$TIME_PERIOD, format = "%Y-Q%q")
    liste_dataframe[[i]]$OBS_VALUE <- as.numeric(liste_dataframe[[i]]$OBS_VALUE)
    liste_dataframe[[i]]$BASE_100 <- 100 * liste_dataframe[[i]]$OBS_VALUE / tail(liste_dataframe[[i]]$OBS_VALUE, n=1)
  }
  
  # mise en forme pour affichage
  IDF_hors_micro <- liste_dataframe[[1]][c("TIME_PERIOD", "BASE_100")]
  IDF_hors_micro$table <- "IDF hors microentreprise"
  France_hors_micro <- liste_dataframe[[2]][c("TIME_PERIOD", "BASE_100")]
  France_hors_micro$table <- "France hors microentreprise"
  IDF_avec_micro <- liste_dataframe[[3]][c("TIME_PERIOD", "BASE_100")]
  IDF_avec_micro$table <- "IDF avec microentreprise"
  France_avec_micro <- liste_dataframe[[4]][c("TIME_PERIOD", "BASE_100")]
  France_avec_micro$table <- "France avec microentreprise"
  
  fig5_df <- rbind(IDF_hors_micro, France_hors_micro, IDF_avec_micro, France_avec_micro)
  # graphique
  g <- ggplot(data = fig5_df, aes(x = TIME_PERIOD, y = BASE_100, color = table)) + geom_line() + geom_hline(yintercept = 100) + labs(x = "Trimestre", color="Légende")
  return(g)
}

# table
generateur_table_creation_entreprise <- function(annee, trimestre, horizon) {
  code_trimestre <- paste0(annee, "-Q", trimestre)
  horizon <- as.numeric(horizon)*4
  
  IDF_hors_micro <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010756192?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  France_hors_micro <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010756115?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  IDF_avec_micro <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010756498?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("TIME_PERIOD","OBS_VALUE")]
  France_avec_micro <- as.data.frame(readSDMX(paste0("http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/010756085?", "endPeriod=", code_trimestre, "&lastNobservations=", horizon)))[c("OBS_VALUE")]
  
  table_df <- as.data.frame(cbind(IDF_avec_micro, IDF_hors_micro, France_avec_micro, France_hors_micro))
  names(table_df) <- c("Trimestre", "Total Île-de-France", "Île-de-France hors microentreprises", "Total France", "France hors microentreprises")
  return(table_df)
}
