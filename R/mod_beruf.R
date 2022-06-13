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

  r <- reactiveValues(date_arbeitsmarkt_einstieg = NULL,
                      date_arbeitsmarkt = NULL,
                      gender_switch = NULL,
                      anforderungsniveau_arbeitsmarkt = NULL,
                      date_arbeitsmarkt_verlauf = NULL,
                      indikator_arbeitsmarkt_verlauf = NULL,
                      topic_arbeitsmarkt_verlauf = NULL,
                      states_arbeitsmarkt_verlauf = NULL,
                      anforderungsniveau_arbeitsmarkt_verlauf = NULL,
                      date_arbeitsmarkt_verlauf_bl = NULL,
                      states_arbeitsmarkt_verlauf_bl = NULL,
                      topic_arbeitsmarkt_verlauf_bl = NULL,
                      anforderungsniveau_arbeitsmarkt_verlauf_bl = NULL)

  # Arbeitsmarkt
  mod_beruf_arbeitsmarkt_multiple_server("mod_beruf_arbeitsmarkt_multiple_ui_1", r)
  mod_beruf_arbeitsmarkt_verlauf_server("mod_beruf_arbeitsmarkt_verlauf_ui_1", r)
  mod_beruf_arbeitsmarkt_verlauf_bl_server("mod_beruf_arbeitsmarkt_verlauf_bl_ui_1", r)
  mod_beruf_arbeitsmarkt_server("mod_beruf_arbeitsmarkt_ui_1", data_arbeitsmarkt, r)

  # Box 2
  mod_beruf_arbeitsmarkt_einstieg_server("mod_beruf_arbeitsmarkt_einstieg_ui_1", r)
  mod_beruf_arbeitsmarkt_einstieg_verlauf_server("mod_beruf_arbeitsmarkt_einstieg_verlauf_ui_1", r)
  mod_beruf_arbeitsmarkt_einstieg_vergleich_server("mod_beruf_arbeitsmarkt_einstieg_vergleich_ui_1", r)

  # Box 3
  mod_beruf_arbeitsmarkt_einstieg_gender_server("mod_beruf_arbeitsmarkt_einstieg_gender_ui_1", r)
  mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_server("mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui_1", r)
  mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_server("mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_ui_1", r)

  # Box 4
  mod_beruf_arbeitsmarkt_anforderungen_server("mod_beruf_arbeitsmarkt_anforderungen_ui_1", r)
  mod_beruf_arbeitsmarkt_anforderungen_verlauf_server("mod_beruf_arbeitsmarkt_anforderungen_verlauf_ui_1", r)
  mod_beruf_arbeitsmarkt_anforderungen_vergleich_server("mod_beruf_arbeitsmarkt_anforderungen_vergleich_ui_1", r)

  # Box 5
  mod_beruf_arbeitsmarkt_anforderungen_gender_server("mod_beruf_arbeitsmarkt_anforderungen_gender_ui_1", r)
  mod_beruf_arbeitsmarkt_anforderungen_gender_verlauf_server("mod_beruf_arbeitsmarkt_anforderungen_gender_verlauf_ui_1", r)
  mod_beruf_arbeitsmarkt_anforderungen_gender_vergleich_server("mod_beruf_arbeitsmarkt_anforderungen_gender_vergleich_ui_1", r)

  # Box 6
  mod_beruf_arbeitsmarkt_bl_server("mod_beruf_arbeitsmarkt_bl_ui_1", r)
  mod_beruf_arbeitsmarkt_bl_verlauf_server("mod_beruf_arbeitsmarkt_bl_verlauf_ui_1", r)
  mod_beruf_arbeitsmarkt_bl_vergleich_server("beruf_arbeitsmarkt_bl_vergleich_ui_1", r)

  # Box 7
  mod_beruf_arbeitsmarkt_bl_gender_server("mod_beruf_arbeitsmarkt_bl_gender_ui_1", r)
  mod_beruf_arbeitsmarkt_bl_gender_verlauf_server("mod_beruf_arbeitsmarkt_bl_gender_verlauf_ui_1", r)
  mod_beruf_arbeitsmarkt_bl_gender_vergleich_server("beruf_arbeitsmarkt_bl_gender_vergleich_ui_1", r)


}

## To be copied in the UI
# mod_beruf_ui("beruf_1")

## To be copied in the server
# mod_beruf_server("beruf_1")
