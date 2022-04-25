#' @description A function to create the value used to fill the value boxes in the
#' the first box of the tab "Studium"
#'
#' @return The return value is a list of values used to fill the value box
#' @param data The dataframe "Studierende.xlsx" needs to be used for this function
#' @param r Reactive variable that stores all the inputs from the UI
#' @noRd
#'

box_einstieg <- function(df,r){

  # load UI inputs from reactive value
  timerange <- r$date_studierende_einstieg

  status_studierende <- r$indikator_studierende_einstieg

  lehramt_enthalten <- r$nurLehramt_studierende_einstieg

  # filter dataset based on UI inputs
  df <- df %>% dplyr::filter(jahr >= timerange[1] & jahr <= timerange[2])

  df <- df %>% dplyr::filter(indikator == status_studierende)

  df <- df %>% dplyr::filter(region == "Deutschland")

  # call function to calculate the share of MINT and the remaining subjects
  df <- calc_share_MINT(df)

  # calculate the share of males
  df <- calc_share_male(df, type = "box_1")

  # calculate the mean
  df <- df %>% dplyr::filter(hochschulform == "insgesamt") %>%
    dplyr::group_by(anzeige_geschlecht, fachbereich) %>%
    dplyr::summarise(wert_mean = mean(wert))

  # calculate the share of females on MINT
  anteil_mint <- round((df[(df$anzeige_geschlecht == "frauen" & df$fachbereich == "MINT"), "wert_mean"][[1]] /
    df[(df$anzeige_geschlecht == "gesamt" & df$fachbereich == "MINT"), "wert_mean"][[1]])*100)

  # calculate the share of females on the remaining subjects
  anteil_rest <- round((df[(df$anzeige_geschlecht == "frauen" & df$fachbereich == "Rest"), "wert_mean"][[1]] /
                          df[(df$anzeige_geschlecht == "gesamt" & df$fachbereich == "Rest"), "wert_mean"][[1]])*100)

  return(list(anteil_mint = anteil_mint, anteil_rest = anteil_rest))
}




#' @description A function to create the value box
#'
#' @return The return is a value box
#' @param value The value to display in the box. Usually a number or short text.
#' @param title Title of the value box
#' @param subtitle Subtitle below the big number
#' @param icon An icon tag
#' @param color color of the box
#' @param width Width of the box
#' @param href An optional URL to link to.
#' @param info Text of information helper.
#' @noRd
#'
valueBox2 <- function(value, title, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL,
                      info = NULL){

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
