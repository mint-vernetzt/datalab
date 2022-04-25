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

  # extract string after last underscore
  df <- df %>% tidyr::extract(var, c("var", "status"), "(.*)_([^_]+)")

  # extract string before first underscore
  df <- df %>% tidyr::extract(var, c("prüfungsstatus", "var"), "([^_]*)_(.*)")

  df <- df %>% dplyr::filter(status != "alle")

  df <- df[!grepl("quote", df$var),]

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
filter_data_compare <- function(data){

  df <- data %>% dplyr::filter((bereich == "studium") &
                                 (typ_absolut_oder_realtiv == "anzahl")) %>%
    dplyr::select(var,indikator,frauen_manner_alle,
                  fachbereich_alle_mint_mathe_ing, jahr, wert, quelle)

  # extract string after last underscore
  df <- df %>% tidyr::extract(var, c("var", "status"), "(.*)_([^_]+)")

  # extract string before first underscore
  df <- df %>% tidyr::extract(var, c("prüfungsstatus", "var"), "([^_]*)_(.*)")

  df <- df %>% dplyr::filter(status != "alle")

  df <- df[!grepl("quote", df$var),]

  df <- df %>%  dplyr::filter((fachbereich_alle_mint_mathe_ing != "alle"))

  df <- df[!grepl("lehramt", df$var),]

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
filter_indikator <- function(df, subject, type){

  if (type == "Abschluss"){

    df <- df %>% dplyr::filter(indikator == "abschlusspruefungszahlen")

  } else if (type == "Studienzahl"){

    df <- df %>% dplyr::filter(indikator == "studierendenanzahl")

  } else {

    df <- df %>% dplyr::filter(indikator == "habilitationszahlen")

  }



  df <- df %>% subset(fachbereich_alle_mint_mathe_ing %in% subject)


  return(df)
}



#' filter_studium_studienzahl
#'
#' @description A fct function
#' @param data
#' @return The return value, if any, from executing the function.
#'
#' @noRd
calc_anteil_MINT <- function(df){

  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "Mathe", "wert"] -
    df[df$fachbereich == "Ingenieur", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "Rest"


  df[df$fachbereich == "Ingenieur", "wert"] <- df[df$fachbereich == "Mathe", "wert"] +
    df[df$fachbereich == "Ingenieur", "wert"]

  df[df$fachbereich == "Ingenieur", "fachbereich"] <- "MINT"

  df <- df %>% dplyr::filter(fachbereich != "Mathe")

  return(df)
}
