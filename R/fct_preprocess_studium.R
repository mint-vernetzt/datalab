#' preprocess_studium
#'
#' @description A function to calculate the share
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @return The return value is a dataframe
#'
#' @noRd
calc_share_waffle <- function(df){

  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "Mathe", "wert"] -
    df[df$fachbereich == "Ingenieur", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Studiengänge"

  df <- df %>%
    dplyr::mutate(props = sum(wert))

  df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/props)

  df$proportion <- df$proportion * 100

  x <- setNames(round_preserve_sum(as.numeric(df$proportion),0),
                df$fachbereich)


  return(x)
}



#' preprocess_studium
#'
#' @description A function to calculate the share of MINT
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @return The return value is a dataframe
#'
#' @noRd
calc_share_MINT <- function(df){

  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "Mathe", "wert"] -
    df[df$fachbereich == "Ingenieur", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Studiengänge"


  df[df$fachbereich == "Ingenieur", "wert"] <- df[df$fachbereich == "Mathe", "wert"] +
    df[df$fachbereich == "Ingenieur", "wert"]

  df[df$fachbereich == "Ingenieur", "fachbereich"] <- "MINT"

  df <- df %>% dplyr::filter(fachbereich != "Mathe")

  return(df)
}


#' preprocess_studium
#'
#' @description A function to calculate the share of males
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @return The return value is a dataframe
#'
#' @noRd
calc_share_male <- function(df, type){

  if(type == "box_1"){

    #extract values of "Gesamt" and "Frauen" to calculate "Männer"
    help_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt") %>%
      dplyr::group_by(jahr, fachbereich)

    help_weiblich <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen") %>%
      dplyr::group_by(jahr, fachbereich)

    wert_männlich <- help_gesamt$wert - help_weiblich$wert

    help_männlich <- help_weiblich

    help_männlich$wert <- wert_männlich

    help_männlich$anzeige_geschlecht <- "Männer"

    df <- rbind(df, help_männlich)

    df <- df[with(df, order(fachbereich, jahr, decreasing = TRUE)), ]

  } else if(type == "box_2"){

    values <- df %>%
      dplyr::group_by(jahr, fachbereich, indikator) %>%
      dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()

    df[df$anzeige_geschlecht == "Gesamt", "wert"] <- values$wert

    df[df$anzeige_geschlecht == "Gesamt", "anzeige_geschlecht"] <- "Männer"

  }

  return(df)
}

#' preprocess_studium
#'
#' @description A function to calculate the share of teacher
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @return The return value is a dataframe
#'
#' @noRd
calc_share_teacher <- function(df){

  # calculate proportion of teacher
  values <- df %>%
    dplyr::group_by(jahr, fachbereich, anzeige_geschlecht) %>%
    dplyr::mutate(wert = wert - dplyr::lead(wert, n = 2)) %>% dplyr::select(wert) %>% na.omit()

  toDelete <- seq(2, nrow(values), 2)
  values <- values[-toDelete ,]

  df[df$hochschulform == "insgesamt", "wert"] <- values$wert

  df <- df[!(df$nur_lehramt == "Nein" & df$hochschulform == "Uni"), ]

  df <- df[!(df$nur_lehramt == "Nein" & df$hochschulform == "FH"), ]

  df[df$nur_lehramt == "Ja", "fachbereich"] <- interaction(df[df$nur_lehramt == "Ja", "fachbereich"][[1]], "_lehramt", sep = "")

  return(df)
}

#' preprocess_studium
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

prep_studium_proportion <- function(df) {

  df <- df %>%
    dplyr::mutate(props = sum(wert))

  df <- df %>% dplyr::group_by(fachbereich, anzeige_geschlecht) %>%
    dplyr::summarize(proportion = wert/props)

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

prep_studierende_east_west <- function(df) {

  df_incl <- df

  # create dummy to indicate "Osten" or "Westen"
  df_incl$dummy_west <- ifelse(df_incl$region %in% states_east_west$west, "Westen", "Osten")

  df_incl <- df_incl %>% dplyr::group_by(jahr, anzeige_geschlecht, indikator, fachbereich, dummy_west,
                                         nur_lehramt, hochschulform ,bereich) %>%
    dplyr::summarise(wert = sum(wert, na.rm = T))

  names(df_incl)[5] <- "region"

  df_incl <- df_incl[, colnames(df)]

  df <- rbind(df, df_incl)

  return(df)

}



#' @description A function to calculate the share of MINT
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @return The return value is a dataframe
#'
#' @noRd
calc_share_MINT_bl <- function(df){

  df[df$fachbereich == "Alle", "fachbereich"] <- "andere Studiengänge"


  df <- df[with(df, order(anzeige_geschlecht, fachbereich, indikator, jahr, decreasing = FALSE)), ]

  # calcualte the share of MINT by aggregating "Mathe" and "Ingenieur"
  df[df$fachbereich == "Ingenieur", "wert"] <- df[df$fachbereich == "Mathe", "wert"] +
    df[df$fachbereich == "Ingenieur", "wert"]

  df[df$fachbereich == "Ingenieur", "fachbereich"] <- "MINT"

  df <- df %>% dplyr::filter(fachbereich != "Mathe")

  df <- df[with(df, order(anzeige_geschlecht, fachbereich, indikator, jahr, decreasing = FALSE)), ]

  df[df$fachbereich == "andere Studiengänge", "wert"] <- df[df$fachbereich == "andere Studiengänge", "wert"] -
    df[df$fachbereich == "MINT", "wert"]


  return(df)
}

#' @description A function to calculate the share of males
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @return The return value is a dataframe
#'
#' @noRd
calc_share_male_bl <- function(df){

    # extract values of "Gesamt" and "Frauen" to calculate the share of males
    help_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "Gesamt") %>%
      dplyr::group_by(jahr, region, indikator, fachbereich)

    help_weiblich <- df %>% dplyr::filter(anzeige_geschlecht == "Frauen") %>%
      dplyr::group_by(jahr, region, indikator, fachbereich)

    wert_männlich <- help_gesamt$wert - help_weiblich$wert

    help_männlich <- help_weiblich

    help_männlich$wert <- wert_männlich

    help_männlich$anzeige_geschlecht <- "Männer"

    df <- rbind(df, help_männlich)

    df <- df[with(df, order(fachbereich, jahr, decreasing = TRUE)), ]

  return(df)
}

