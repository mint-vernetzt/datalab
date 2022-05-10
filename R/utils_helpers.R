#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

dictionary_title_studium_studentenzahl <- list("eingeschrieben" = "die insgesamt eingeschrieben sind",
                                               "1hs" = "die im 1. Hochschulsemester eingeschrieben sind",
                                               "1fs" = "die im 1. Fachsemester eingeschrieben sind")


#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

dictionary_title_studium <- list("Mathe" = "in Mathematik",
                                               "Ingenieur" = "am Ingenieurswesen")


#' helpers
#'
#' @description A utils function to make sure that rounding does not cause
#' a value below or above 100
#'
#' @return The return value is a vector of values
#'
#' @noRd
round_preserve_sum <- function(x, digits = 0) {

  up <- 10 ^ digits

  x <- x * up

  y <- floor(x)

  indices <- tail(order(x-y), round(sum(x)) - sum(y))

  y[indices] <- y[indices] + 1

   y <- y / up

  return(y)
}


#' helpers
#'
#' @description A utils list which contains the hex codes of colors from the
#' design guide
#'
#' @noRd
colors_mint_vernetzt <- list(general = c("#154194", "#b16fab", "#efe8e6"),
                             attention = c("#00a87a", "#fcc433", "#ee7775"),
                             neutral = c("#141416", "#e6e8ec"),
                             gender = c("#f5adac", "#b1b5c3"))

#' helpers
#'
#' @description A utils list which contains the all states of the former west and
#' east of germany
#'
#' @noRd
states_east_west <- list(west = c("Baden-Württemberg", "Bayern", "Bremen", "Hamburg",
                                  "Hessen", "Niedersachsen", "Nordrhein-Westfalen",
                                  "Rheinland-Pfalz", "Saarland", "Schleswig-Holstein"),
                        east = c("Brandenburg", "Mecklenburg-Vorpommern", "Sachsen",
                                 "Sachsen-Anhalt", "Thüringen", "Berlin"))


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
                      info = NULL, type = "andere"){

  if (type == "Frauen"){

    style <- paste0("background-color: ", "#f5adac; color:white;")

  } else {

    style <- paste0("background-color: ", "#b1b5c3; color:white;")

  }

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
    class = "small-box",
    style = style,
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


#' @description A function to return a grouped and stacked highcharter output
#' @param data The dataframe
#' @param geschlecht A vector containing a selection of "Männer, "Frauen" and "Gesamt"
#' @param type A string which specifies whether stack is 100% or absolut
#' @param andere_name A string which specifies how to name the "Other subjects"
#' @param lehramt A string which specifies whether lehramt is true or not
#' @return plot
#'
#' @noRd
highchart_obj <- function(df, geschlecht, type, andere_name, lehramt){

  values_1 <- df %>% dplyr::filter((anzeige_geschlecht == "Frauen" & fachbereich == "MINT"))
  values_2 <- df %>% dplyr::filter((anzeige_geschlecht == "Frauen" & fachbereich == andere_name))

  values_3 <- df %>% dplyr::filter((anzeige_geschlecht == "Männer" & fachbereich == "MINT"))
  values_4 <- df %>% dplyr::filter((anzeige_geschlecht == "Männer" & fachbereich == andere_name))

  values_5 <- df %>% dplyr::filter((anzeige_geschlecht == "Gesamt" & fachbereich == "MINT"))
  values_6 <- df %>% dplyr::filter((anzeige_geschlecht == "Gesamt" & fachbereich == andere_name))

  p <- highcharter::highchart() %>%
    highcharter::hc_chart(type = "column") %>%
    highcharter::hc_plotOptions(column = list(stacking = type)) %>% # , series = list(pointWidth = 60)
    highcharter::hc_xAxis(categories = unique(df$jahr), offset = 20, dataLabels = list(enabled = TRUE),
                          top = "-128%", lineWidth = 0, minorGridLineWidth = 0)

  vec_help_full <- c("Männer", "Gesamt", "Frauen")

  vec_help_male_female <- c("Männer", "Frauen")

  vec_help_ges_male <- c("Männer", "Gesamt")

  vec_help_ges_female <- c("Gesamt", "Frauen")

  if (lehramt == "Nein") {

    if (setequal(geschlecht, vec_help_full)) {

        p %>%
          highcharter::hc_add_series(name = "MINT",
                                     data = values_1$wert,
                                     stack = "Frauen",
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_2$wert,
                                     color = "#154194",
                                     stack = "Frauen",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen',
          'Frauen', 'Frauen', 'Frauen', 'Frauen'];
          return labels[this.point.x];
        }"))) %>%
          highcharter::hc_add_series(name="MINT",
                                     data = values_3$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_4$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Männer', 'Männer', 'Männer', 'Männer', 'Männer', 'Männer',
          'Männer', 'Männer', 'Männer', 'Männer'];
          return labels[this.point.x];
        }"))) %>%
          highcharter::hc_add_series(name = "MINT",
                                     data = values_5$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_6$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt',
          'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt'];
          return labels[this.point.x];
        }")))

      } else if (setequal(geschlecht, vec_help_male_female)) {

        p %>%
          highcharter::hc_add_series(name = "MINT",
                                     data = values_1$wert,
                                     stack = "Frauen",
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_2$wert,
                                     color = "#154194",
                                     stack = "Frauen",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen',
          'Frauen', 'Frauen', 'Frauen', 'Frauen'];
          return labels[this.point.x];
        }"))) %>%
          highcharter::hc_add_series(name="MINT",
                                     data = values_3$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_4$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       enabled = TRUE,
                                       rotation = 90,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Männer', 'Männer', 'Männer', 'Männer', 'Männer', 'Männer',
          'Männer', 'Männer', 'Männer', 'Männer'];
          return labels[this.point.x];
        }")))

      } else if (setequal(geschlecht, vec_help_ges_male)) {

        p %>%
          highcharter::hc_add_series(name="MINT",
                                     data = values_3$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_4$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Männer', 'Männer', 'Männer', 'Männer', 'Männer', 'Männer',
          'Männer', 'Männer', 'Männer', 'Männer'];
          return labels[this.point.x];
        }"))) %>%
          highcharter::hc_add_series(name="MINT",
                                     data = values_5$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_6$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt',
          'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt'];
          return labels[this.point.x];
        }")))


      } else if (setequal(geschlecht, vec_help_ges_female)) {

        p %>%
          highcharter::hc_add_series(name = "MINT",
                                     data = values_1$wert,
                                     stack = "Frauen",
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_2$wert,
                                     color = "#154194",
                                     stack = "Frauen",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen',
          'Frauen', 'Frauen'];
          return labels[this.point.x];
        }"))) %>%
          highcharter::hc_add_series(name="MINT",
                                     data = values_5$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_6$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt',
          'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt'];
          return labels[this.point.x];
        }")))

      } else if (geschlecht == "Gesamt") {

        p %>%
          highcharter::hc_add_series(name="MINT",
                                     data = values_5$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_6$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt',
          'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt'];
          return labels[this.point.x];
        }")))

      } else if (geschlecht == "Frauen") {

        p %>%
          highcharter::hc_add_series(name = "MINT",
                                     data = values_1$wert,
                                     stack = "Frauen",
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_2$wert,
                                     color = "#154194",
                                     stack = "Frauen",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen',
          'Frauen', 'Frauen', 'Frauen', 'Frauen'];
          return labels[this.point.x];
        }")))


      } else if (geschlecht == "Männer") {

        p %>%
          highcharter::hc_add_series(name="MINT",
                                     data = values_3$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_4$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Männer', 'Männer', 'Männer', 'Männer', 'Männer', 'Männer',
          'Männer', 'Männer', 'Männer', 'Männer'];
          return labels[this.point.x];
        }")))

        }

    } else {

      values_7 <- df %>% dplyr::filter((anzeige_geschlecht == "Gesamt" & fachbereich == "MINT (Lehramt)"))
      values_8 <- df %>% dplyr::filter((anzeige_geschlecht == "Gesamt" & fachbereich == "andere Studiengänge (Lehramt)"))

      values_9 <- df %>% dplyr::filter((anzeige_geschlecht == "Männer" & fachbereich == "MINT (Lehramt)"))
      values_10 <- df %>% dplyr::filter((anzeige_geschlecht == "Männer" & fachbereich == "andere Studiengänge (Lehramt)"))

      values_11 <- df %>% dplyr::filter((anzeige_geschlecht == "Frauen" & fachbereich == "MINT (Lehramt)"))
      values_12 <- df %>% dplyr::filter((anzeige_geschlecht == "Frauen" & fachbereich == "andere Studiengänge (Lehramt)"))

      if (setequal(geschlecht, vec_help_full)) {

        p %>%
          highcharter::hc_add_series(name = "MINT",
                                     data = values_1$wert,
                                     stack = "Frauen",
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = "MINT (Lehramt)",
                                     data = values_11$wert,
                                     stack = "Frauen",
                                     showInLegend= T,
                                     color = 'rgba(177, 111, 171, 0.50)') %>%
          highcharter::hc_add_series(name = "Studiengänge (Lehramt)",
                                     data = values_12$wert,
                                     stack = "Frauen",
                                     showInLegend= T,
                                     color = 'rgba(21, 65, 148, 0.50)') %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_2$wert,
                                     color = "#154194",
                                     stack = "Frauen",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen',
          'Frauen', 'Frauen'];
          return labels[this.point.x];
        }"))) %>%
          highcharter::hc_add_series(name="MINT",
                                     data = values_3$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = "MINT (Lehramt)",
                                     data = values_9$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color ='rgba(177, 111, 171, 0.50)') %>%
          highcharter::hc_add_series(name = "Studiengänge (Lehramt)",
                                     data = values_10$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = 'rgba(21, 65, 148, 0.50)') %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_4$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Männer', 'Männer', 'Männer', 'Männer', 'Männer', 'Männer',
          'Männer', 'Männer', 'Männer', 'Männer'];
          return labels[this.point.x];
        }"))) %>%
          highcharter::hc_add_series(name = "MINT",
                                     data = values_5$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = "MINT (Lehramt)",
                                     data = values_7$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = 'rgba(177, 111, 171, 0.50)') %>%
          highcharter::hc_add_series(name = "Studiengänge (Lehramt)",
                                     data = values_8$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = 'rgba(21, 65, 148, 0.50)') %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_6$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt',
          'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt'];
          return labels[this.point.x];
        }")))

      } else if (setequal(geschlecht, vec_help_male_female)) {

        p %>%
          highcharter::hc_add_series(name = "MINT",
                                     data = values_1$wert,
                                     stack = "Frauen",
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = "MINT (Lehramt)",
                                     data = values_11$wert,
                                     stack = "Frauen",
                                     showInLegend= T,
                                     color = 'rgba(177, 111, 171, 0.50)') %>%
          highcharter::hc_add_series(name = "Studiengänge (Lehramt)",
                                     data = values_12$wert,
                                     stack = "Frauen",
                                     showInLegend= T,
                                     color = 'rgba(21, 65, 148, 0.50)') %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_2$wert,
                                     color = "#154194",
                                     stack = "Frauen",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen',
          'Frauen', 'Frauen', 'Frauen', 'Frauen'];
          return labels[this.point.x];
        }"))) %>%
          highcharter::hc_add_series(name="MINT",
                                     data = values_3$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = "MINT (Lehramt)",
                                     data = values_9$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = 'rgba(177, 111, 171, 0.50)') %>%
          highcharter::hc_add_series(name = "Studiengänge (Lehramt)",
                                     data = values_10$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = 'rgba(21, 65, 148, 0.50)') %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_4$wert,
                                     stack = "Männer",
                                     showInLegend= F,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       enabled = TRUE,
                                       rotation = 90,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Männer', 'Männer', 'Männer', 'Männer', 'Männer', 'Männer',
          'Männer', 'Männer', 'Männer', 'Männer'];
          return labels[this.point.x];
        }")))

      } else if (setequal(geschlecht, vec_help_ges_male)) {

        p %>%
          highcharter::hc_add_series(name="MINT",
                                     data = values_3$wert,
                                     stack = "Männer",
                                     showInLegend= T,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = "MINT (Lehramt)",
                                     data = values_9$wert,
                                     stack = "Männer",
                                     showInLegend= T,
                                     color = 'rgba(177, 111, 171, 0.50)') %>%
          highcharter::hc_add_series(name = "Studiengänge (Lehramt)",
                                     data = values_10$wert,
                                     stack = "Männer",
                                     showInLegend= T,
                                     color = 'rgba(21, 65, 148, 0.50)') %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_4$wert,
                                     stack = "Männer",
                                     showInLegend= T,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Männer', 'Männer', 'Männer', 'Männer', 'Männer', 'Männer',
          'Männer', 'Männer', 'Männer', 'Männer'];
          return labels[this.point.x];
        }"))) %>%
          highcharter::hc_add_series(name="MINT",
                                     data = values_5$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = "MINT (Lehramt)",
                                     data = values_7$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = 'rgba(177, 111, 171, 0.50)') %>%
          highcharter::hc_add_series(name = "Studiengänge (Lehramt)",
                                     data = values_8$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = 'rgba(21, 65, 148, 0.50)') %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_6$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt'];
          return labels[this.point.x];
        }")))


      } else if (setequal(geschlecht, vec_help_ges_female)) {

        p %>%
          highcharter::hc_add_series(name = "MINT",
                                     data = values_1$wert,
                                     stack = "Frauen",
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = "MINT (Lehramt)",
                                     data = values_11$wert,
                                     stack = "Frauen",
                                     showInLegend= T,
                                     color = 'rgba(177, 111, 171, 0.50)') %>%
          highcharter::hc_add_series(name = "Studiengänge (Lehramt)",
                                     data = values_8$wert,
                                     stack = "Frauen",
                                     showInLegend= T,
                                     color = 'rgba(21, 65, 148, 0.50)') %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_12$wert,
                                     color = "#154194",
                                     stack = "Frauen",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen'];
          return labels[this.point.x];
        }"))) %>%
          highcharter::hc_add_series(name="MINT",
                                     data = values_5$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = "MINT (Lehramt)",
                                     data = values_7$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = 'rgba(177, 111, 171, 0.50)') %>%
          highcharter::hc_add_series(name = "Studiengänge (Lehramt)",
                                     data = values_8$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color ='rgba(21, 65, 148, 0.50)') %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_6$wert,
                                     stack = "Gesamt",
                                     showInLegend= F,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt'];
          return labels[this.point.x];
        }")))

      } else if (geschlecht == "Gesamt") {

        p %>%
          highcharter::hc_add_series(name="MINT",
                                     data = values_5$wert,
                                     stack = "Gesamt",
                                     showInLegend= T,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = "MINT (Lehramt)",
                                     data = values_7$wert,
                                     stack = "Gesamt",
                                     showInLegend= T,
                                     color = 'rgba(177, 111, 171, 0.50)') %>%
          highcharter::hc_add_series(name = "Studiengänge (Lehramt)",
                                     data = values_8$wert,
                                     stack = "Gesamt",
                                     showInLegend= T,
                                     color = 'rgba(21, 65, 148, 0.50)') %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_6$wert,
                                     stack = "Gesamt",
                                     showInLegend= T,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt', 'Gesamt'];
          return labels[this.point.x];
        }")))

      } else if (geschlecht == "Frauen") {

        p %>%
          highcharter::hc_add_series(name = "MINT",
                                     data = values_1$wert,
                                     stack = "Frauen",
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = "MINT (Lehramt)",
                                     data = values_11$wert,
                                     stack = "Frauen",
                                     showInLegend= T,
                                     color = 'rgba(177, 111, 171, 0.50)') %>%
          highcharter::hc_add_series(name = "Studiengänge (Lehramt)",
                                     data = values_12$wert,
                                     stack = "Frauen",
                                     showInLegend= T,
                                     color = 'rgba(21, 65, 148, 0.50)') %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_2$wert,
                                     color = "#154194",
                                     stack = "Frauen",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen', 'Frauen','Frauen', 'Frauen'];
          return labels[this.point.x];
        }")))


      } else if (geschlecht == "Männer") {

        p %>%
          highcharter::hc_add_series(name="MINT",
                                     data = values_3$wert,
                                     stack = "Männer",
                                     showInLegend= T,
                                     color = "#b16fab") %>%
          highcharter::hc_add_series(name = "MINT (Lehramt)",
                                     data = values_9$wert,
                                     stack = "Männer",
                                     showInLegend= T,
                                     color = 'rgba(177, 111, 171, 0.50)') %>%
          highcharter::hc_add_series(name = "Studiengänge (Lehramt)",
                                     data = values_10$wert,
                                     stack = "Männer",
                                     showInLegend= T,
                                     color = 'rgba(21, 65, 148, 0.50)') %>%
          highcharter::hc_add_series(name = andere_name,
                                     data = values_4$wert,
                                     stack = "Männer",
                                     showInLegend= T,
                                     color = "#154194",
                                     dataLabels = list(
                                       color = "black",
                                       rotation = 90,
                                       enabled = TRUE,
                                       style = list(fontSize = '12px'),
                                       align = "left",
                                       y = 8,
                                       verticalAlign = "bottom",
                                       inside = TRUE,
                                       crop =  FALSE,
                                       overflow = 'allow',
                                       formatter = highcharter::JS(
                                         "function() {
          var labels = ['Männer', 'Männer', 'Männer', 'Männer', 'Männer', 'Männer', 'Männer', 'Männer'];
          return labels[this.point.x];
        }")))


      }
    }

}
