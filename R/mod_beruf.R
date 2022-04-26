#' beruf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        mod_beruf_arbeitsmarkt_ui("mod_beruf_arbeitsmarkt_ui_1")
      )
    )
  )
}

#' beruf Server Functions
#'
#' @noRd
mod_beruf_server <- function(id, data_arbeitsmarkt, r){

  r <- reactiveValues(geschlecht_arbeitsmarkt_einstieg = NULL,
                      indikator_arbeitsmarkt_einstieg = NULL,
                      date_arbeitsmarkt_einstieg = NULL,
                      switch_rel_abs = NULL,
                      date_arbeitsmarkt = NULL,
                      indikator_arbeitsmarkt = NULL,
                      anforderungsniveau_arbeitsmarkt = NULL,
                      date_arbeitsmarkt_verlauf = NULL,
                      indikator_arbeitsmarkt_verlauf = NULL,
                      topic_arbeitsmarkt_verlauf = NULL,
                      states_arbeitsmarkt_verlauf = NULL,
                      anforderungsniveau_verlauf = NULL)

  # Arbeitsmarkt
  mod_beruf_arbeitsmarkt_einstieg_server("mod_beruf_arbeitsmarkt_einstieg_ui_1", r)
  mod_beruf_arbeitsmarkt_multiple_server("mod_beruf_arbeitsmarkt_multiple_ui_1", r)
  mod_beruf_arbeitsmarkt_verlauf_server("mod_beruf_arbeitsmarkt_verlauf_ui_1", r)
  mod_beruf_arbeitsmarkt_server("mod_beruf_arbeitsmarkt_ui_1", data_arbeitsmarkt, r)


}

## To be copied in the UI
# mod_beruf_ui("beruf_1")

## To be copied in the server
# mod_beruf_server("beruf_1")
