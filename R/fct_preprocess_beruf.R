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
