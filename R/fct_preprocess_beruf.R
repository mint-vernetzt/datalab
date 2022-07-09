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
  df_incl$dummy_west <- ifelse(df_incl$region %in% states_east_west$west, "Westen", "Osten")

  # sum values
  df_incl <- df_incl %>% dplyr::group_by(jahr, anzeige_geschlecht, indikator, fachbereich, dummy_west,
                                         anforderungsniveau, bereich) %>%
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

  help_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt") %>%
    dplyr::group_by(jahr, fachbereich)

  help_weiblich <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen") %>%
    dplyr::group_by(jahr, fachbereich)

  wert_männlich <- help_gesamt$wert - help_weiblich$wert

  help_männlich <- help_weiblich

  help_männlich$wert <- wert_männlich

  help_männlich$anzeige_geschlecht <- "Männer"

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
                                  anforderungsniveau == "Gesamt")

  df <- df %>% dplyr::filter(fachbereich == "MINT")

  df_female <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen") %>%
    dplyr::rename(wert_female = wert)

  df_male <- df %>% dplyr::filter(anzeige_geschlecht == "Männer") %>%
    dplyr::rename(wert_male = wert)

  df_female <- df_female %>% dplyr::left_join(df_alle, by=c("region", "indikator", "jahr", "bereich")) %>%
    dplyr::select(-c("anforderungsniveau.y", "fachbereich.y")) %>%
    dplyr::rename(anforderungsniveau = "anforderungsniveau.x",
                  fachbereich = "fachbereich.x") %>%
    dplyr::mutate(proportion = (wert_female/wert)*100) %>%
    dplyr::mutate(anzeige_geschlecht = "Frauen") %>%
    dplyr::select(-wert_female)

  df_male <- df_male %>% dplyr::left_join(df_alle, by=c("region", "indikator", "jahr", "bereich")) %>%
    dplyr::select(-c("anforderungsniveau.y", "fachbereich.y")) %>%
    dplyr::rename(anforderungsniveau = "anforderungsniveau.x",
                  fachbereich = "fachbereich.x") %>%
    dplyr::mutate(proportion = (wert_male/wert)*100) %>%
    dplyr::mutate(anzeige_geschlecht = "Männer") %>%
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

  df_alle <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt",
                                  anforderungsniveau == "Gesamt") %>%
    # dplyr::select(-fachbereich) %>%
    dplyr::group_by(region, indikator, anforderungsniveau, jahr, anzeige_geschlecht, bereich) %>%
    dplyr::summarise(wert = sum(wert))

  df_employed <- df %>% dplyr::filter(indikator == "Beschäftigte") %>%
    dplyr::rename(wert_employed = wert)

  df_trainee <- df %>% dplyr::filter(indikator == "Auszubildende") %>%
    dplyr::rename(wert_trainee = wert)

  df_employed <- df_employed %>% dplyr::left_join(df_alle, by=c("region", "indikator", "anzeige_geschlecht", "jahr", "bereich")) %>%
    dplyr::select(-"anforderungsniveau.y") %>%
    dplyr::rename(anforderungsniveau = "anforderungsniveau.x") %>%
    dplyr::mutate(proportion = (wert_employed/wert)*100) %>%
    # dplyr::mutate(indikator = "Beschäftigte") %>%
    dplyr::select(-wert_employed)

  df_trainee <- df_trainee %>% dplyr::left_join(df_alle, by=c("region", "indikator", "anzeige_geschlecht", "jahr", "bereich")) %>%
    dplyr::select(-"anforderungsniveau.y") %>%
    dplyr::rename(anforderungsniveau = "anforderungsniveau.x") %>%
    dplyr::mutate(proportion = (wert_trainee/wert)*100) %>%
    # dplyr::mutate(indikator = "Auszubildende") %>%
    dplyr::select(-wert_trainee)

  df_return <- rbind(df_employed, df_trainee)

  return(df_return)
}
