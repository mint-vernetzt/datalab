#' preprocess_schule
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

prep_kurse_proportion <- function(df, indikator_choice) {


  df <- df %>% dplyr::filter(indikator == indikator_choice)

  # calculate new "Gesamt"
  df[df$anzeige_geschlecht == "Gesamt", "wert"] <-  df %>%
    dplyr::group_by(indikator, jahr) %>%
    dplyr::summarise(wert = wert[anzeige_geschlecht == "Frauen"] +
                       wert[anzeige_geschlecht == "Männer"]) %>% dplyr::pull(wert)

  # extract new "Gesamt"
  mint_value <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt") %>%
    dplyr::summarise(props = sum(wert))

  df <- df %>% dplyr::filter(anzeige_geschlecht != "Gesamt")

  # calculate proportion
  df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/mint_value$props)

  df$proportion <- df$proportion * 100

  df$anzeige_geschlecht <- paste0(df$anzeige_geschlecht, " (", df$fachbereich, ")")

  # ensure that proportions sum to 1
  x <- setNames(round_preserve_sum(as.numeric(df$proportion),1),
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

prep_kurse_east_west <- function(df, type = "no_subjects") {
  #berechnet war Falsch - hat Deutschland in Osten enthalten
  browser()
  df_incl <- df

  # create dummy variable to indicate east or west
  df_incl$dummy_west <- ifelse(df_incl$region %in% states_east_west$west & df_incl$region != "Deutschland", "Westdeutschland (o. Berlin)", NA)
  df_incl$dummy_west <- ifelse(df_incl$region %in% states_east_west$east & df_incl$region != "Deutschland", "Ostdeutschland (inkl. Berlin)", df_incl$dummy_west)
  df_incl <- na.omit(df_incl)# ifelse erstellt nochmal DE mit "NA" als region-Namen -->löschen

  # aggregate values
  df_incl <- df_incl %>% dplyr::group_by(jahr, anzeige_geschlecht, indikator, fachbereich, dummy_west) %>%
    dplyr::summarise(wert = sum(wert, na.rm = T))

  names(df_incl)[names(df_incl) == "dummy_west"] <- "region"


    df_incl <- df_incl[, colnames(df)]


    df <- rbind(df, df_incl)

  # }


  return(df)

}

#' preprocess_schule
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'

share_mint_kurse <- function(df){

  #combine subjects to get numbers on share of MINT
  subjects <- c("Mathematik", "Informatik",  "Biologie", "Chemie",
                "Physik", "andere naturwiss.-technische Fächer"
                )

  #calculating MINT
  values_Mint <- df %>%
    dplyr::filter(fachbereich %in% subjects)%>%
     dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, bereich) %>%
     dplyr::summarise(wert = sum(wert))

   values_Mint$fachbereich <- "MINT"


  values_andere <- df %>% dplyr::filter(fachbereich == "Alle Fächer")

  #sorting for subtraction
  values_Mint <- values_Mint[with(values_Mint, order(jahr, region, anzeige_geschlecht, indikator, bereich, decreasing = FALSE)), ]
  values_andere <- values_andere[with(values_andere, order(jahr, region, anzeige_geschlecht, indikator, bereich, decreasing = FALSE)), ]

  values_andere$wert <- values_andere$wert - values_Mint$wert

  values_andere$fachbereich <- "andere Fächer"

   df <- rbind(values_Mint, values_andere)



  return(df)
}
