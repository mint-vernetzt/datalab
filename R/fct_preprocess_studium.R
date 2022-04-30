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



#' @description A function to calculate the share of males
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @return The return value is a dataframe
#'
#' @noRd
calc_share_male <- function(df, type){

  if(type == "box_1"){

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
      dplyr::group_by(jahr, fachbereich) %>%
      dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()

    df[df$anzeige_geschlecht == "Gesamt", "wert"] <- values$wert

    df[df$anzeige_geschlecht == "Gesamt", "anzeige_geschlecht"] <- "Männer"

  }

  return(df)
}


#' @description A function to calculate the share of teacher
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @return The return value is a dataframe
#'
#' @noRd
calc_share_teacher <- function(df){

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

