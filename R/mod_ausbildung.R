#' ausbildung UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ausbildung_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        mod_ausbildung_vertraege_ui("mod_ausbildung_vertraege_ui_1")
      )
    )
  )
}

#' ausbildung Server Functions
#'
#' @noRd
mod_ausbildung_server <- function(id, data_ausbildungsvertraege, r){


  r <- reactiveValues(indikator_ausbildungsvertraege = NULL,
                      date_ausbildungsvertraege = NULL,
                      indikator_ausbildung_verlauf = NULL,
                      states_ausbildung_verlauf = NULL,
                      ost_west = NULL)

  # Ausbildung
  mod_ausbildung_vertraege_verlauf_server("mod_ausbildung_vertraege_verlauf_ui_1", r)
  mod_ausbildung_vertraege_multiple_server("mod_ausbildung_vertraege_multiple_ui_1", r)
  mod_ausbildung_vertraege_server("mod_ausbildung_vertraege_ui_1", data_ausbildungsvertraege, r)





}

## To be copied in the UI
# mod_ausbildung_ui("ausbildung_1")

## To be copied in the server
# mod_ausbildung_server("ausbildung_1")
