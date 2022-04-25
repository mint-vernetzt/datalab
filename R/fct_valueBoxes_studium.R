#' make_valueBoxes_studium
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param data
#' @param r
#' @noRd
#'

box_einstieg <- function(df,r){

  timerange <- r$date_studierende_einstieg

  status_studierende <- r$indikator_studierende_einstieg

  lehramt_enthalten <- r$nurLehramt_studierende_einstieg

  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_studierende)

  df <- df %>% dplyr::filter(region == "Deutschland")

  df[df$fachbereich == "Alle", "wert"] <- df[df$fachbereich == "Alle", "wert"] -
    df[df$fachbereich == "Mathe", "wert"] -
    df[df$fachbereich == "Ingenieur", "wert"]

  df[df$fachbereich == "Alle", "fachbereich"] <- "Rest"


  df[df$fachbereich == "Ingenieur", "wert"] <- df[df$fachbereich == "Mathe", "wert"] +
    df[df$fachbereich == "Ingenieur", "wert"]

  df[df$fachbereich == "Ingenieur", "fachbereich"] <- "MINT"

  df <- df %>% dplyr::filter(fachbereich != "Mathe")

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

  df <- df %>% dplyr::filter(hochschulform == "insgesamt") %>%
    dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
    dplyr::summarise(wert_mean = mean(wert))

  anteil_mint <- round((df[(df$anzeige_geschlecht == "frauen" & df$fachbereich == "MINT"), "wert_mean"][[1]] /
    df[(df$anzeige_geschlecht == "gesamt" & df$fachbereich == "MINT"), "wert_mean"][[1]])*100)

  anteil_rest <- round((df[(df$anzeige_geschlecht == "frauen" & df$fachbereich == "Rest"), "wert_mean"][[1]] /
                          df[(df$anzeige_geschlecht == "gesamt" & df$fachbereich == "Rest"), "wert_mean"][[1]])*100)

  return(list(anteil_mint = anteil_mint, anteil_rest = anteil_rest))
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

box_dynamic_1 <- function(data,r){

  timestamp <- r$date_waffle

  indicator <- r$indikator_waffle

  df <- filter_data_studienanzahl(data)

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

  pass_fail <- r$durchgefallen

  df <- filter_data_abschluss(data)

  df <- df %>% dplyr::filter(jahr == timestamp)

  df <- df %>% dplyr::filter(prüfungsstatus == pass_fail)

  value_string_male <- as.character(sum(df$wert[df$frauen_manner_alle == "männlich"]))
  value_string_female <- as.character(sum(df$wert[df$frauen_manner_alle == "weiblich"]))

  return(list(male = value_string_male, female = value_string_female, pass_fail = pass_fail))
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

  pass_fail <- r$durchgefallen_1

  df <- filter_data_abschluss(data)

  df <- df %>% dplyr::filter(jahr >= date_range[1] & jahr <= date_range[2])

  df <- df %>% dplyr::filter(prüfungsstatus == pass_fail)

  value_string_male <- as.character(round(mean(df$wert[df$frauen_manner_alle == "männlich"], na.rm = TRUE),1))
  value_string_female <- as.character(round(mean(df$wert[df$frauen_manner_alle == "weiblich"], na.rm = TRUE),1))

  return(list(male = value_string_male, female = value_string_female,
              year_1 = date_range[1], year_2 = date_range[2], pass_fail = pass_fail))
}




#' make_valueBoxes_studium
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @param value
#' @param title
#' @param subtitle
#' @param icon
#' @param color
#' @param width
#' @param href
#' @param info
#' @param sparkobj
#' @noRd
#'
valueBox2 <- function(value, title, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL,
                      info = NULL, sparkobj = NULL){

  #shinydashboard:::validateColor(color)

  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")

  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    class = "pull-right"
  )

  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      info_icon,
      tags$small(title),
      h3(value),
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon, style = "z-index; 0")
  )

  if (!is.null(href))
    boxContent <- a(href = href, boxContent)

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}
