#' filter_studium_studienzahl
#'
#' @description A fct function
#' @param data
#' @return The return value, if any, from executing the function.
#'
#' @noRd
filter_data_studienanzahl <- function(data){

  df <- data %>% dplyr::filter((indikator == "studierendenanzahl") &
                                 (typ_absolut_oder_realtiv == "anzahl")) %>%
    dplyr::select(var,frauen_manner_alle,
                  fachbereich_alle_mint_mathe_ing, jahr, wert, quelle)

  df <- df[!grepl("lehramt", df$var),]

  df <- df %>% tidyr::extract(var, c("var", "status"), "(.*)_([^_]+)")

  df$var <- NULL

  return(df)
}



#' filter_studium_studienzahl
#'
#' @description A fct function
#' @param data
#' @return The return value, if any, from executing the function.
#'
#' @noRd
filter_data_abschluss <- function(data){

  df <- data %>% dplyr::filter((indikator == "abschlusspruefungszahlen") &
                                 (typ_absolut_oder_realtiv == "anzahl")) %>%
    dplyr::select(var,frauen_manner_alle,
                  fachbereich_alle_mint_mathe_ing, jahr, wert, quelle)

  df <- df %>% tidyr::extract(var, c("var", "status"), "(.*)_([^_]+)")

  df <- df %>% dplyr::filter(status != "alle")

  df <- df[!grepl("quote", df$var),]

  df$var <- NULL

  return(df)
}
