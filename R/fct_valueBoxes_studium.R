#' make_valueBoxes_studium
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param data
#' @param r
#' @noRd
#'

box_dynamic_1 <- function(data,r){

  timestamp <- r$date_waffle

  indicator <- r$indikator_waffle

  df <- filter_data(data)

  df <- df %>% dplyr::filter(jahr == timestamp)

  df <- df %>% dplyr::filter(status == indicator)

  value_string_male <- as.character(round(mean(df$wert[df$frauen_manner_alle == "männlich"]),1))
  value_string_female <- as.character(round(mean(df$wert[df$frauen_manner_alle == "weiblich"]),1))

  return(list(male = value_string_male, female = value_string_female, year = timestamp))
}

#' make_valueBoxes_studium
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param data
#' @param r
#' @noRd
#'

box_dynamic_2 <- function(data,r){

  timestamp <- r$date_abschluss

  #geschlecht <- r$geschlecht_abschluss

  df <- filter_data_abschluss(data)

  df <- df %>% dplyr::filter(jahr == timestamp)

  #df <- df %>% dplyr::filter(status == indicator)

  value_string_male <- as.character(sum(df$wert[df$frauen_manner_alle == "männlich"]))
  value_string_female <- as.character(sum(df$wert[df$frauen_manner_alle == "weiblich"]))

  return(list(male = value_string_male, female = value_string_female))
}

#' make_valueBoxes_studium
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param data
#' @param r
#' @noRd
#'

box_dynamic_3 <- function(data,r){

  date_range <- r$date_abschluss_1

  df <- filter_data_abschluss(data)

  df <- df %>% dplyr::filter(jahr >= date_range[1] & jahr <= date_range[2])

  value_string_male <- as.character(round(mean(df$wert[df$frauen_manner_alle == "männlich"], na.rm = TRUE),1))
  value_string_female <- as.character(round(mean(df$wert[df$frauen_manner_alle == "weiblich"], na.rm = TRUE),1))

  return(list(male = value_string_male, female = value_string_female,
              year_1 = date_range[1], year_2 = date_range[2]))
}
