#' @description A function to create the value used to fill the value boxes in the
#' the first box of the tab "Schule"
#'
#' @return The return value is a list of values used to fill the value box
#' @param data The dataframe "Kurse.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
#'

box_einstieg_kurse <- function(df,r){

  # load UI inputs from reactive value
  timerange <- r$date_kurse_einstieg

  level_kurs <- r$indikator_kurse_einstieg

  switch_absolut <- r$switch_rel_abs

  geschlecht <- r$geschlecht_kurse_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == level_kurs)

  df <- df %>% dplyr::filter(region == "Deutschland")

  # combine subjects to get numbers on share of MINT
  # make a function out of it
  subjects <- c("Mathematik", "Informatik", "Naturwissenschaften",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer",  "Erdkunde", "Alle Fächer")

  df <- df %>% dplyr::filter(fachbereich %in% subjects)

  df$fachbereich <- ifelse(df$fachbereich != "Alle Fächer", "MINT", "andere Fächer")

  df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht, jahr) %>%
    dplyr::summarize(wert = sum(wert))


  # call function to calculate the share of MINT and the remaining subjects
  df[df$fachbereich == "andere Fächer", "wert"] <- df[df$fachbereich == "andere Fächer", "wert"] -
    df[df$fachbereich == "MINT", "wert"]

  # calculate the mean
  df <- df %>%
    dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
    dplyr::summarise(wert_mean = mean(wert))

  df <- df %>% dplyr::group_by(anzeige_geschlecht) %>%
    dplyr::mutate(props = sum(wert_mean))

  df <- df %>% dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
    dplyr::summarize(proportion = wert_mean/props)


  # calculate the share of females on MINT
  anteil_mint <- round(df[(df$anzeige_geschlecht == "Frauen" & df$fachbereich == "MINT"), "proportion"]*100)

  # calculate the share of females on the remaining subjects
  anteil_rest <- round(df[(df$anzeige_geschlecht == "Frauen" & df$fachbereich == "andere Fächer"), "proportion"]*100)

  vec_anteile <- c(anteil_mint$proportion, anteil_rest$proportion)

  vec_anteile <- round_preserve_sum(round(vec_anteile),0)

  return(list(anteil_mint = vec_anteile[1], anteil_rest = vec_anteile[2]))
}
