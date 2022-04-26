#' @description A function to calculate the share of males
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @return The return value is a dataframe
#'
#' @noRd
calc_share_male <- function(df, type){

  if(type == "box_1"){

    help_gesamt <- df %>% dplyr::filter(anzeige_geschlecht == "gesamt") %>%
      dplyr::group_by(jahr, fachbereich)

    help_weiblich <- df %>% dplyr::filter(anzeige_geschlecht == "frauen") %>%
      dplyr::group_by(jahr, fachbereich)

    wert_männlich <- help_gesamt$wert - help_weiblich$wert

    help_männlich <- help_weiblich

    help_männlich$wert <- wert_männlich

    help_männlich$anzeige_geschlecht <- "männer"

    df <- rbind(df, help_männlich)

    df <- df[with(df, order(fachbereich, jahr, decreasing = TRUE)), ]

  } else if(type == "box_2"){

    values <- df %>%
      dplyr::group_by(jahr, fachbereich) %>%
      dplyr::mutate(wert = wert - dplyr::lead(wert)) %>% dplyr::select(wert) %>% na.omit()

    df[df$anzeige_geschlecht == "gesamt", "wert"] <- values$wert

    df[df$anzeige_geschlecht == "gesamt", "anzeige_geschlecht"] <- "männer"

  }

  return(df)
}
