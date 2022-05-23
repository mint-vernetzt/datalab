#' preprocess_schule
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

prep_arbeitsmarkt_east_west <- function(df) {

  df_incl <- df

  df_incl$dummy_west <- ifelse(df_incl$region %in% states_east_west$west, "Westen", "Osten")

  df_incl <- df_incl %>% dplyr::group_by(jahr, anzeige_geschlecht, indikator, fachbereich, dummy_west,
                                         anforderungsniveau, bereich) %>%
    dplyr::summarise(wert = sum(wert, na.rm = T))

  names(df_incl)[5] <- "region"


  df <- rbind(df, df_incl)

  return(df)

}

