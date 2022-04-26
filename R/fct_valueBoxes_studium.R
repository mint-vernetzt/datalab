#' @description A function to create the value used to fill the value boxes in the
#' the first box of the tab "Studium"
#'
#' @return The return value is a list of values used to fill the value box
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
#'

box_einstieg_studium <- function(df,r){

  # load UI inputs from reactive value
  timerange <- r$date_studierende_einstieg

  status_studierende <- r$indikator_studierende_einstieg

  lehramt_enthalten <- r$nurLehramt_studierende_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_studierende)

  df <- df %>% dplyr::filter(region == "Deutschland")

  # call function to calculate the share of MINT and the remaining subjects
  df <- calc_share_MINT(df)

  # calculate the share of males
  df <- calc_share_male(df, type = "box_1") # I dont need that

  # calculate the mean
  df <- df %>% dplyr::filter(hochschulform == "insgesamt") %>%
    dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
    dplyr::summarise(wert_mean = mean(wert))

  # calculate the share of females on MINT
  anteil_mint <- round((df[(df$anzeige_geschlecht == "frauen" & df$fachbereich == "MINT"), "wert_mean"][[1]] /
    df[(df$anzeige_geschlecht == "gesamt" & df$fachbereich == "MINT"), "wert_mean"][[1]])*100)

  # calculate the share of females on the remaining subjects
  anteil_rest <- round((df[(df$anzeige_geschlecht == "frauen" & df$fachbereich == "andere Studiengänge"), "wert_mean"][[1]] /
                          df[(df$anzeige_geschlecht == "gesamt" & df$fachbereich == "andere Studiengänge"), "wert_mean"][[1]])*100)

  return(list(anteil_mint = anteil_mint, anteil_rest = anteil_rest))
}




