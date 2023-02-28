#' preprocess_beruf
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

prep_arbeitsmarkt_east_west <- function(df) {

  df_incl <- df

  # set dummy variable to indicate "Osten" und "Westen"
  ## Falls DE enthalten falsch
  #df_incl$dummy_west <- ifelse(df_incl$region %in% states_east_west$west, "Westen", "Osten")

   df_incl$dummy_west <- ifelse(df_incl$region %in% states_east_west$west & df_incl$region != "Deutschland", "Westen", NA)
  df_incl$dummy_west <- ifelse(df_incl$region %in% states_east_west$east & df_incl$region != "Deutschland", "Osten", df_incl$dummy_west)
  df <- na.omit(df) # NA aus ifelse erstellt nochmal DE mit NA als region-Name -->löschen

  # sum values
  df_incl <- df_incl %>% dplyr::group_by(jahr, geschlecht, indikator, fachbereich, dummy_west,
                                         anforderung, bereich) %>%
    dplyr::summarise(wert = sum(wert, na.rm = T))

  names(df_incl)[5] <- "region"


  df <- rbind(df, df_incl)

  return(df)

}

#' preprocess_beruf
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

calc_arbeitsmarkt_males <- function(df) {

  help_gesamt <- df %>% dplyr::filter(geschlecht == "Gesamt") %>%
    dplyr::group_by(jahr, fachbereich)

  help_weiblich <- df %>% dplyr::filter(geschlecht == "Frauen") %>%
    dplyr::group_by(jahr, fachbereich)

  wert_männlich <- help_gesamt$wert - help_weiblich$wert

  help_männlich <- help_weiblich

  help_männlich$wert <- wert_männlich

  help_männlich$geschlecht <- "Männer"

  df <- rbind(df, help_männlich)

  return(df)

}

#' preprocess_beruf
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

calc_arbeitsmarkt_mint <- function(df) {

  df_alle <- df %>% dplyr::filter(fachbereich == "Alle") %>%
    dplyr::select(-fachbereich)

  df_mint <- df %>% dplyr::filter(fachbereich == "MINT") %>%
    dplyr::rename(wert_mint = wert) %>%
    dplyr::select(-fachbereich)

  df_andere <- df_alle %>% dplyr::left_join(df_mint) %>%
    dplyr::mutate(wert = wert-wert_mint) %>%
    dplyr::mutate(fachbereich = "Andere Berufe") %>%
    dplyr::select(-wert_mint)

  df_mint <- df_mint %>% dplyr::mutate(fachbereich = "MINT") %>%
    dplyr::rename(wert = wert_mint)

  df_return <- rbind(df_andere, df_mint)

  return(df_return)
}

#' preprocess_beruf
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

calc_arbeitsmarkt_share_bl_gender <- function(df) {

  df_alle <- df %>% dplyr::filter(fachbereich == "Alle",
                                  anforderung == "Gesamt")

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  df_female <- df %>% dplyr::filter(geschlecht == "Frauen") %>%
    dplyr::rename(wert_female = wert)

  df_male <- df %>% dplyr::filter(geschlecht == "Männer") %>%
    dplyr::rename(wert_male = wert)

  df_female <- df_female %>% dplyr::left_join(df_alle, by=c("region", "indikator", "jahr", "bereich")) %>%
    dplyr::select(-c("anforderung.y", "fachbereich.y")) %>%
    dplyr::rename(anforderung = "anforderung.x",
                  fachbereich = "fachbereich.x") %>%
    dplyr::mutate(proportion = (wert_female/wert)*100) %>%
    dplyr::mutate(geschlecht = "Frauen") %>%
    dplyr::select(-wert_female)

  df_male <- df_male %>% dplyr::left_join(df_alle, by=c("region", "indikator", "jahr", "bereich")) %>%
    dplyr::select(-c("anforderung.y", "fachbereich.y")) %>%
    dplyr::rename(anforderung = "anforderung.x",
                  fachbereich = "fachbereich.x") %>%
    dplyr::mutate(proportion = (wert_male/wert)*100) %>%
    dplyr::mutate(geschlecht = "Männer") %>%
    dplyr::select(-wert_male)

  df_return <- rbind(df_female, df_male)

  return(df_return)
}

#' preprocess_beruf
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

calc_arbeitsmarkt_share_bl <- function(df) {

  df_alle <- df %>% dplyr::filter(geschlecht == "Gesamt",
                                  anforderung == "Gesamt") %>%
    # dplyr::select(-fachbereich) %>%
    dplyr::group_by(region, indikator, anforderung, jahr, geschlecht, bereich) %>%
    #dplyr::summarise(wert = sum(wert)) # Fehler: rechnet damit in jeder Gruppierung Alle + MINT aus
    dplyr::mutate(wert = wert[fachbereich == "Alle"]) %>%
    dplyr::select(-fachbereich)

  #löschen doppelter Zeilen, die durch Ersetzten vs. falsch Aufsummieren entstanden sind
  df_alle <- unique.data.frame(df_alle)

  df_employed <- df %>% dplyr::filter(indikator == "Beschäftigte") %>%
    dplyr::rename(wert_employed = wert)

  df_trainee <- df %>% dplyr::filter(indikator == "Auszubildende") %>%
    dplyr::rename(wert_trainee = wert)

  df_employed <- df_employed %>% dplyr::left_join(df_alle, by=c("region", "indikator", "geschlecht", "jahr", "bereich")) %>%
    dplyr::select(-"anforderung.y") %>%
    dplyr::rename(anforderung = "anforderung.x") %>%
    dplyr::mutate(proportion = (wert_employed/wert)*100) %>%
    # dplyr::mutate(indikator = "Beschäftigte") %>%
    dplyr::select(-wert_employed)

  df_trainee <- df_trainee %>% dplyr::left_join(df_alle, by=c("region", "indikator", "geschlecht", "jahr", "bereich")) %>%
    dplyr::select(-"anforderung.y") %>%
    dplyr::rename(anforderung = "anforderung.x") %>%
    dplyr::mutate(proportion = (wert_trainee/wert)*100) %>%
    # dplyr::mutate(indikator = "Auszubildende") %>%
    dplyr::select(-wert_trainee)

  df_return <- rbind(df_employed, df_trainee)


  return(df_return)
}





#' preprocess_beruf on landkreis level
#'
#' @description Function calculates the shares on landkreis level
#'
#' @return a dataframe.
#'
#' @noRd
calculate_landkreis <- function(df, states, category, domain, indikator_azubi, indikator_besch, region = "") {

  # filter dataset based on UI inputs
  df_filtered <- df %>% dplyr::filter(bundesland == states,
                                      anforderung == "Gesamt",
                                      kategorie == category) # dropdown 1 - Azubis oder Beschäftigte

  # dropdown 2 auf Gesamt --> kein Fachbereich ausgewählt, nur Indikator
  if (domain == "Alle") {
    df_gesamt <- df_filtered %>% dplyr::filter(fachbereich == "Alle",
                                               indikator == category,
                                               geschlecht == "Gesamt")
    #titel_gesamt_1 <- paste0(" an allen ")
    titel_gesamt_1 <- ifelse(domain == "Alle", " an allen ", paste0(" in ", domain, " an allen "))


  } else {
    # dropdown 2 nicht auf Gesamt

    # dropdown 3 auf Gesamt --> nach folgendem filter selbes wie drüber
    if ((category == indikator_besch) |
        (category == indikator_azubi)) {
      df_gesamt <- df_filtered %>% dplyr::filter(fachbereich == "Alle",
                                                 indikator == category,
                                                 geschlecht == "Gesamt")

      # titel_gesamt_1 <- paste0(" an allen ")
      titel_gesamt_1 <- ifelse(domain == "Alle", " an allen ", paste0(" in ", domain, " an allen "))


    } else {
      # dropdown 3 nicht auf Gesamt --> Fachbereich und Indikator ausgewählt

      df_gesamt <- df_filtered %>% dplyr::filter(fachbereich == domain,
                                                 indikator == category,
                                                 geschlecht == "Gesamt")

      # titel_gesamt_1 <- paste0(" in ", domain, " an allen ")
      titel_gesamt_1 <- ifelse(domain == "Alle", " an allen ", paste0(" in ", domain, " an allen "))

    }

  }

  df_sub <- df_filtered %>% dplyr::filter(fachbereich == domain)

  # dropdown 3
  if(category == "Beschäftigte"){

    titel_sub <- indikator_besch

    if(indikator_besch != "Frauen"){

      df_sub <- df_sub %>% dplyr::filter(indikator == indikator_besch,
                                         geschlecht == "Gesamt")

      titel_sub <- paste0(indikator_besch)
      titel_sub <- ifelse(grepl("ausl", indikator_besch), "ausländischer Beschäftigter", titel_sub)
      titel_sub <- ifelse(grepl("u25", indikator_besch), "Beschäftigter unter 25 Jahren", titel_sub)
      titel_sub <- ifelse(grepl("25-55", indikator_besch), "Beschäftigter zwischen 25 und 55 Jahren", titel_sub)
      titel_sub <- ifelse(grepl("ü55", indikator_besch), "Beschäftigter über 55 Jahren", titel_sub)
      titel_sub2 <- paste0(indikator_besch, "n")
      titel_sub2 <- ifelse(grepl("ausl", indikator_besch), "ausländischen Beschäftigten", titel_sub2)
      titel_sub2 <- ifelse(grepl("u25", indikator_besch), "Beschäftigten unter 25 Jahren", titel_sub2)
      titel_sub2 <- ifelse(grepl("25-55", indikator_besch), "Beschäftigten zwischen 25 und 55 Jahren", titel_sub2)
      titel_sub2 <- ifelse(grepl("ü55", indikator_besch), "Beschäftigten über 55 Jahren", titel_sub2)
      # titel_gesamt_1 <- paste0(" in ", domain, " an allen ")
      titel_gesamt_1 <- ifelse(domain == "Alle", " an allen ", paste0(" in ", domain, " an allen "))



    } else if(indikator_besch == "Frauen"){

      df_sub <- df_sub %>% dplyr::filter(indikator == category,
                                         geschlecht == indikator_besch)

      titel_sub <- paste0(" weiblicher ", category, "r")
      titel_sub2 <- paste0(" weiblichen ", category, "n")
      # titel_gesamt_1 <- paste0(" in ", domain, " an allen ")
      titel_gesamt_1 <- ifelse(domain == "Alle", " an allen ", paste0(" in ", domain, " an allen "))

    }

  } else if(category == "Auszubildende"){

    titel_sub <- indikator_azubi

    if(indikator_azubi != "Frauen"){

      df_sub <- df_sub %>% dplyr::filter(indikator == indikator_azubi,
                                         geschlecht == "Gesamt")

      titel_sub <- paste0(indikator_azubi)
      titel_sub <- ifelse(grepl("ausl", indikator_azubi), "ausländischer Auszubildender", titel_sub)
      titel_sub <- ifelse(grepl("(1.Jahr)", indikator_besch), "Auszubildender im 1. Lehrjahr", titel_sub)
      titel_sub2 <- paste0(indikator_azubi, "n")
      titel_sub2 <- ifelse(grepl("ausl", indikator_azubi), "ausländischen Auszubildenden", titel_sub2)
      titel_sub2 <- ifelse(grepl("(1.Jahr)", indikator_besch), "Auszubildenden im 1. Lehrjahr", titel_sub2)
      # titel_gesamt_1 <- paste0(" in ", domain, " an allen ")
      titel_gesamt_1 <- ifelse(domain == "Alle", " an allen ", paste0(" in ", domain, " an allen "))


    } else if(indikator_azubi == "Frauen"){

      df_sub <- df_sub %>% dplyr::filter(indikator == category,
                                         geschlecht == indikator_azubi)

      titel_sub <- paste0(" weiblicher ", category, "r")
      titel_sub2 <- paste0(" weiblichen ", category, "n")
      # titel_gesamt_1 <- paste0(" in ", domain, " an allen ")
      titel_gesamt_1 <- ifelse(domain == "Alle", " an allen ", paste0(" in ", domain, " an allen "))
    }
  }

  # merge dataframes and compute prob
  df_compare <- df_sub %>%
    dplyr::left_join(df_gesamt,
                     by = c("bereich",
                            "kategorie",
                            "bundesland",
                            "landkreis",
                            "landkreis_zusatz",
                            "landkreis_nummer",
                            "jahr",
                            "anforderung"))

  if(region == "Gesamt"){
    df_compare <- df_compare %>%
      dplyr::group_by(bereich,
                      kategorie,
                      bundesland,
                      jahr,
                      anforderung) %>%
      dplyr::summarise(wert.x = sum(wert.x),
                       wert.y = sum(wert.y)) %>%
      dplyr::mutate(prob = round((wert.x/wert.y)*100)) %>%
      dplyr::rename(wert = wert.x) %>%
      dplyr::select(-wert.y) %>%
      dplyr::ungroup()

  } else {
    df_compare <- df_compare %>%
      dplyr::mutate(prob = round((wert.x/wert.y)*100)) %>%
      dplyr::rename(wert = wert.x,
                    geschlecht = geschlecht.x) %>%
      dplyr::select(-c(wert.y, geschlecht.y))

  }

  # return relevant values as a list
  return_list <- list()
  return_list[[1]] <- df_compare
  return_list[[2]] <- titel_gesamt_1
  return_list[[3]] <- titel_sub
  return_list[[4]] <- titel_sub2

  return(return_list)
}
