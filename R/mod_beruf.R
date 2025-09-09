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
mod_beruf_server <- function(id,
                             r){

  r <- reactiveValues()

  # Arbeitsmarkt
  mod_beruf_arbeitsmarkt_server("mod_beruf_arbeitsmarkt_ui_1", r)

  # Box 1
  mod_beruf_arbeitsmarkt_einstieg_vergleich_server("mod_beruf_arbeitsmarkt_einstieg_vergleich_ui_1", r)
  mod_beruf_arbeitsmarkt_einstieg_verlauf_server("mod_beruf_arbeitsmarkt_einstieg_verlauf_ui_1", r)
  mod_beruf_arbeitsmarkt_mint_bula_server("mod_beruf_arbeitsmarkt_mint_bula_ui_1", r)

  # Box 2
  mod_beruf_arbeitsmarkt_anforderungen_server("mod_beruf_arbeitsmarkt_anforderungen_ui_1", r)
  mod_beruf_arbeitsmarkt_faecher_verlauf_server("mod_beruf_arbeitsmarkt_faecher_verlauf_ui_1", r)
  mod_beruf_arbeitsmarkt_bula_faecher_server("mod_beruf_arbeitsmarkt_bula_faecher_ui_1", r)

  # Box 3
  mod_beruf_arbeitsmarkt_einstieg_gender_server("mod_beruf_arbeitsmarkt_einstieg_gender_ui_1", r)
  mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_server("mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui_1", r)
  mod_beruf_arbeitsmarkt_anforderungen_gender_server("mod_beruf_arbeitsmarkt_anforderungen_gender_ui_1", r)
  mod_beruf_arbeitsmarkt_top10_server("mod_beruf_arbeitsmarkt_top10_ui_1", r)
  mod_beruf_arbeitsmarkt_anforderungen_frauen_server("mod_beruf_arbeitsmarkt_anforderungen_frauen_ui_1", r)

  # Box 4 regional
  mod_beruf_arbeitsmarkt_landkreis_map_server("mod_beruf_arbeitsmarkt_landkreis_map_ui_1", r)
  mod_beruf_arbeitsmarkt_landkreis_vergleich_server("mod_beruf_arbeitsmarkt_landkreis_vergleich_ui_1", r)
  mod_beruf_arbeitsmarkt_regional_verlauf_server("mod_beruf_arbeitsmarkt_regional_verlauf_ui_1", r)

  # Box 5
  mod_beruf_arbeitsmarkt_entgelt_vergleich_server("mod_beruf_arbeitsmarkt_entgelt_vergleich_ui_1", r)
  mod_beruf_arbeitsmarkt_entgelt_verlauf_server("mod_beruf_arbeitsmarkt_entgelt_verlauf_ui_1", r)
  mod_beruf_arbeitsmarkt_balken_entgelt_server("mod_beruf_arbeitsmarkt_balken_entgelt_ui_1", r)
  mod_beruf_arbeitsmarkt_top_entgelt_server("mod_beruf_arbeitsmarkt_top_entgelt_ui_1", r)

}

## To be copied in the UI
# mod_beruf_ui("beruf_1")

## To be copied in the server
# mod_beruf_server("beruf_1")
