#' schule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        mod_schule_kurse_ui("mod_schule_kurse_ui_1")
      )
    )
  )
}

#' schule Server Functions
#'
#' @noRd
mod_schule_server <- function(id, data_kurse, r){

  r <- reactiveValues(date_kurse_einstieg = NULL,
                      indikator_kurse_einstieg = NULL,
                      switch_rel_abs = NULL,
                      geschlecht_kurse_einstieg = NULL,
                      date_kurse = NULL,
                      indikator_kurse = NULL,
                      date_kurse_verlauf = NULL,
                      indikator_kurse_verlauf = NULL,
                      topic_kurse_verlauf = NULL,
                      states_kurse_verlauf = NULL,
                      ost_west = NULL,
                      subjects_aggregated = NULL,
                      subject_selected = NULL)


  # Kurse
  mod_schule_kurse_einstieg_server("mod_schule_kurse_einstieg_ui_1", r)
  mod_schule_kurse_multiple_server("mod_schule_kurse_multiple_ui_1", r)
  mod_schule_kurse_verlauf_server("mod_schule_kurse_verlauf_ui_1", r)
  mod_schule_kurse_server("mod_schule_kurse_ui_1", data_kurse, r)


}

## To be copied in the UI
# mod_schule_ui("schule_1")

## To be copied in the server
# mod_schule_server("schule_1")
