#' studium UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        shinydashboard::box(
          title = "Studentenzahl nach Fach",
          width = 12,
          shiny::mainPanel(
            mod_studium_studienzahl_ui("mod_studium_studienzahl_ui_1")
          )
        )
      ),
      fluidRow(
        shinydashboard::box(
          title = "Abschlusszahlen nach akademischem Grad",
          width = 12,
          shiny::mainPanel(
            mod_studium_abschluss_ui("mod_studium_abschluss_ui_1")
          )
        )
      )
    )

  )
}

#' studium Server Functions
#'
#' @noRd
mod_studium_server <- function(id, data, r){

  r <- reactiveValues(geschlecht = NULL,
                      date = NULL,
                      indikator = NULL,
                      geschlecht_waffle = NULL,
                      date_waffle = NULL,
                      indikator_waffle = NULL,
                      date_abschluss = NULL,
                      geschlecht_abschluss = NULL,
                      date_abschluss_1 = NULL,
                      geschlecht_abschluss_1 = NULL,
                      indikator_abschluss_1 = NULL,
                      durchgefallen = NULL,
                      ing_natwi = NULL,
                      ing_natwi_1 = NULL,
                      durchgefallen_1 = NULL,)
  # comment
  mod_studium_studienzahl_choice_1_server("mod_studium_studienzahl_choice_ui_1_1", r)
  mod_studium_studienzahl_choice_2_server("mod_studium_studienzahl_choice_ui_2_1", r)
  mod_studium_studienzahl_server("mod_studium_studienzahl_ui_1", data, r)

  # comment
  mod_studium_abschluss_choice_1_server("mod_studium_abschluss_choice_ui_1_1", r)
  mod_studium_abschluss_choice_2_server("mod_studium_abschluss_choice_ui_2_1", r)
  mod_studium_abschluss_server("mod_studium_abschluss_ui_1", data, r)

}

## To be copied in the UI
# mod_studium_ui("studium_1")

## To be copied in the server
# mod_studium_server("studium_1")
