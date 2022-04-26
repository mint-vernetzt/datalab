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

  # calculate the mean
  df <- df %>%
    dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
    dplyr::summarise(wert_mean = mean(wert))

  # calculate the share of females on MINT
  anteil_mint <- round((df[(df$anzeige_geschlecht == "Frauen" & df$fachbereich == "MINT"), "wert_mean"][[1]] /
                          df[(df$anzeige_geschlecht == "Gesamt" & df$fachbereich == "MINT"), "wert_mean"][[1]])*100)

  # calculate the share of females on the remaining subjects
  anteil_rest <- round((df[(df$anzeige_geschlecht == "Frauen" & df$fachbereich == "andere Berufszweige"), "wert_mean"][[1]] /
                          df[(df$anzeige_geschlecht == "Gesamt" & df$fachbereich == "andere Berufszweige"), "wert_mean"][[1]])*100)

  return(list(anteil_mint = anteil_mint, anteil_rest = anteil_rest))
}
