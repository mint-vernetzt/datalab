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
calculate_landkreis <- function(df, states, category, domain, indikator_azubi, indikator_besch) {

  # filter dataset based on UI inputs
  df_filtered <- df %>% dplyr::filter(bundesland == states,
                                      anforderung == "Gesamt",
                                      kategorie == category) # dropdown 1

  # dropdown 2 auf Gesamt
  if (domain == "Alle") {
    df_gesamt <- df_filtered %>% dplyr::filter(fachbereich == "Alle",
                                               indikator == category,
                                               geschlecht == "Gesamt")

    titel_gesamt <- paste0("allen ", domain)

  } else {
    # dropdown 2 nicht auf Gesamt

    # dropdown 3 auf Gesamt
    if ((category == indikator_besch) |
        (category == indikator_azubi)) {
      df_gesamt <- df_filtered %>% dplyr::filter(fachbereich == "Alle",
                                                 indikator == category,
                                                 geschlecht == "Gesamt")

      titel_gesamt <- paste0("allen ", domain)

    } else {
      # dropdown 3 nicht auf Gesamt

      df_gesamt <- df_filtered %>% dplyr::filter(fachbereich == domain,
                                                 indikator == category,
                                                 geschlecht == "Gesamt")

      titel_gesamt <- paste0(domain, " in der Kategorie ", category)

    }

  }

  df_sub <- df_filtered %>% dplyr::filter(fachbereich == domain)

  # dropdown 3
  if(category == "Beschäftigte"){

    titel_sub <- indikator_besch

    if(indikator_besch != "Frauen"){

      df_sub <- df_sub %>% dplyr::filter(indikator == indikator_besch,
                                         geschlecht == "Gesamt")

    } else if(indikator_besch == "Frauen"){

      df_sub <- df_sub %>% dplyr::filter(indikator == category,
                                         geschlecht == indikator_besch)
    }

  } else if(category == "Auszubildende"){

    titel_sub <- indikator_azubi

    if(indikator_azubi != "Frauen"){

      df_sub <- df_sub %>% dplyr::filter(indikator == indikator_azubi,
                                         geschlecht == "Gesamt")

    } else if(indikator_azubi == "Frauen"){

      df_sub <- df_sub %>% dplyr::filter(indikator == category,
                                         geschlecht == indikator_azubi)
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
                            "anforderung")) %>%
    dplyr::mutate(prob = round((wert.x/wert.y)*100)) %>%
    dplyr::rename(wert = wert.x,
                  geschlecht = geschlecht.x) %>%
    dplyr::select(-c(wert.y, geschlecht.y))

  # return relevant values as a list
  return_list <- list()
  return_list[[1]] <- df_compare
  return_list[[2]] <- titel_gesamt
  return_list[[3]] <- titel_sub

  return(return_list)
}
