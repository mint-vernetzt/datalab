#' @description A function to create the value used to fill the value boxes in the
#' the first box of the tab "Beruf"
#'
#' @return The return value is a list of values used to fill the value box
#' @param data The dataframe "Arbeitsmarkt.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
#'

box_einstieg_beruf <- function(df,r){

  # load UI inputs from reactive value
  timerange <- r$date_arbeitsmarkt_einstieg

  status_arbeitnehmer <- r$indikator_arbeitsmarkt_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_arbeitnehmer)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(anforderungsniveau == "Gesamt")

  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Berufszweige"

  # calculate the share of males
  help_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt") %>%
    dplyr::group_by(jahr, fachbereich)

  help_weiblich <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen") %>%
    dplyr::group_by(jahr, fachbereich)

  wert_männlich <- help_gesamt$wert - help_weiblich$wert

  help_männlich <- help_weiblich

  help_männlich$wert <- wert_männlich

  help_männlich$anzeige_geschlecht <- "Männer"

  df <- rbind(df, help_männlich)

  df <- df[with(df, order(anzeige_geschlecht, jahr, decreasing = TRUE)), ]

  # calculate the mean
  df <- df %>%
    dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
    dplyr::summarise(wert_mean = mean(wert))

  df <- df %>% dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::mutate(props = sum(wert_mean))


  df <- df %>% dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
    dplyr::summarize(proportion = wert_mean/props)


  # calculate the share of females on MINT
  anteil_mint_female <- round(df[(df$anzeige_geschlecht == "Frauen" & df$fachbereich == "MINT"), "proportion"]*100)

  # calculate the share of females on the remaining subjects
  anteil_mint_male <- round(df[(df$anzeige_geschlecht == "Männer" & df$fachbereich == "MINT"), "proportion"]*100)

  return(list(anteil_mint_female = anteil_mint_female$proportion, anteil_mint_male = anteil_mint_male$proportion))
}
