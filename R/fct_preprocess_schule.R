#' preprocess_schule
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

prep_kurse_proportion <- function(df, indikator_choice) {


  df <- df %>% dplyr::filter(indikator == indikator_choice)


  df[df$anzeige_geschlecht == "Gesamt", "wert"] <-  df %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
                       wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)

  df <- df %>% dplyr::filter(fachbereich != "Alle Fächer")

  mint_value <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt") %>%
    dplyr::summarise(props = sum(wert))

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/mint_value$props)

  df$proportion <- df$proportion * 100

  df$anzeige_geschlecht <- paste0(df$anzeige_geschlecht, " (", df$fachbereich, ")")

  x <- setNames(round_preserve_sum(as.numeric(df$proportion),0),
                   df$anzeige_geschlecht)

  return(x)

}

#' preprocess_schule
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

prep_kurse_east_west <- function(df) {

  df_incl <- df

  df_incl$dummy_west <- ifelse(df_incl$region %in% states_east_west$west, "Westen", "Osten")

  df_incl <- df_incl %>% dplyr::group_by(jahr, anzeige_geschlecht, indikator, fachbereich, dummy_west, bereich) %>%
    dplyr::summarise(wert = sum(wert, na.rm = T))

  names(df_incl)[5] <- "region"


  df_incl <-  df_incl %>%
    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                    wert[anzeige_geschlecht == "Männer"])


  df_incl <- df_incl[, colnames(df)]


  df <- rbind(df, df_incl)

  return(df)

}
