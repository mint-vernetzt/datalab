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

#  lehramt_enthalten <- r$nurLehramt_studierende_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_studierende)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df <- df %>% dplyr::filter(hochschulform == "insgesamt")

  # call function to calculate the share of MINT and the remaining subjects
  df <- calc_share_MINT(df)

  # calculate the mean
  df <- df %>%
    dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
    dplyr::summarise(wert_mean = mean(wert))

  df <- df %>% dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::mutate(props = sum(wert_mean))


  df <- df %>% dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
    dplyr::summarize(proportion = wert_mean/props)


  # calculate the share of females on MINT
  anteil_mint <- df[(df$anzeige_geschlecht == "Frauen" & df$fachbereich == "MINT"), "proportion"]*100

  # calculate the share of females on the remaining subjects
  anteil_rest <- df[(df$anzeige_geschlecht == "Frauen" & df$fachbereich == "andere Studiengänge"), "proportion"]*100

  vec_anteile <- c(anteil_mint$proportion, anteil_rest$proportion)

  vec_anteile <- round_preserve_sum(round(vec_anteile),0)

  return(list(anteil_mint = vec_anteile[1], anteil_rest = vec_anteile[2]))
}




