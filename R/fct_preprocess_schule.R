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

prep_kurse_east_west <- function(df, type = "no_subjects") {

  df_incl <- df

  # create dummy variable to indicate east or west
  df_incl$dummy_west <- ifelse(df_incl$region %in% states_east_west$west, "Westen", "Osten")

  # aggregate values
  df_incl <- df_incl %>% dplyr::group_by(jahr, anzeige_geschlecht, indikator, fachbereich, dummy_west, bereich) %>%
    dplyr::summarise(wert = sum(wert, na.rm = T))

  names(df_incl)[5] <- "region"


  if(type == "subjects"){

    df_incl <-  df_incl %>%
      dplyr::group_by(region, fachbereich, indikator, jahr) %>%
      dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
                      wert[anzeige_geschlecht == "Männer"])

    df_incl <- df_incl[, colnames(df)]

    df <- rbind(df, df_incl)

  } else {
    # calcualte new "Gesamt"
    # df_incl <-  df_incl %>%
    #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
    #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
    #                   wert[anzeige_geschlecht == "Männer"])

    df_incl <- df_incl[, colnames(df)]


    df <- rbind(df, df_incl)

  }


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

  # drop
  subjects_drop <- c("Englisch", "Französisch", "andere moderne Fremdsprachen", "Naturwissenschaften",
                      "andere naturwiss.-technische Fächer", "Erdkunde", "Geschichte", "Wirtschaft/Verwaltung/Recht",
                     "Sozialkunde/Gesellschaftslehre/Politik", "Psychologie, Pädagogik", "Sonstige Fächer",
                     "Latein und andere antike Sprachen")

  df <- df[!(df$fachbereich %in% subjects_drop),]

  # combine subjects to get numbers on share of MINT
  subjects <- c("Mathematik", "Informatik",  "Biologie", "Chemie",
                "Physik")


  values_Mint <- df %>%
    dplyr::filter(fachbereich %in% subjects) %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, bereich) %>%
    dplyr::summarise(wert = sum(wert))

  values_Mint$fachbereich <- "MINT"


  values_andere <- df %>%
    dplyr::filter(fachbereich %!in% subjects, fachbereich != "Alle Fächer") %>%
    dplyr::group_by(jahr, region, indikator, anzeige_geschlecht, bereich) %>%
    dplyr::summarise(wert = sum(wert))

  values_andere$fachbereich <- "andere Fächer"

  df <- rbind(values_Mint, values_andere)

}
