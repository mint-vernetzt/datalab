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

  # Box 2
  #mod_beruf_arbeitsmarkt_einstieg_server("mod_beruf_arbeitsmarkt_einstieg_ui_1", r)
  mod_beruf_arbeitsmarkt_einstieg_verlauf_server("mod_beruf_arbeitsmarkt_einstieg_verlauf_ui_1", r)
  mod_beruf_arbeitsmarkt_einstieg_vergleich_server("mod_beruf_arbeitsmarkt_einstieg_vergleich_ui_1", r)

  # Box 3
  mod_beruf_arbeitsmarkt_einstieg_gender_server("mod_beruf_arbeitsmarkt_einstieg_gender_ui_1", r)
  mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_server("mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui_1", r)
  mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_server("mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_ui_1", r)

  # Box 4
  mod_beruf_arbeitsmarkt_anforderungen_server("mod_beruf_arbeitsmarkt_anforderungen_ui_1", r)
  #mod_beruf_arbeitsmarkt_anforderungen_verlauf_server("mod_beruf_arbeitsmarkt_anforderungen_verlauf_ui_1", r)
  #mod_beruf_arbeitsmarkt_anforderungen_vergleich_server("mod_beruf_arbeitsmarkt_anforderungen_vergleich_ui_1", r)
  mod_beruf_arbeitsmarkt_überblick_fächer_server("mod_beruf_arbeitsmarkt_überblick_fächer_ui_1", r)

  # Box 5
  mod_beruf_arbeitsmarkt_anforderungen_gender_server("mod_beruf_arbeitsmarkt_anforderungen_gender_ui_1", r)
  #mod_beruf_arbeitsmarkt_anforderungen_gender_verlauf_server("mod_beruf_arbeitsmarkt_anforderungen_gender_verlauf_ui_1", r)
  #mod_beruf_arbeitsmarkt_anforderungen_gender_vergleich_server("mod_beruf_arbeitsmarkt_anforderungen_gender_vergleich_ui_1", r)

  # Box 6
  mod_beruf_arbeitsmarkt_bl_server("mod_beruf_arbeitsmarkt_bl_ui_1", r)
  mod_beruf_arbeitsmarkt_bl_verlauf_server("mod_beruf_arbeitsmarkt_bl_verlauf_ui_1", r)
  mod_beruf_arbeitsmarkt_bl_vergleich_server("beruf_arbeitsmarkt_bl_vergleich_ui_1", r)
  mod_beruf_arbeitsmarkt_top10_server("mod_beruf_arbeitsmarkt_top10_ui_1", r)

  # Box 7
  mod_beruf_arbeitsmarkt_bl_gender_server("mod_beruf_arbeitsmarkt_bl_gender_ui_1", r)
  mod_beruf_arbeitsmarkt_bl_gender_verlauf_server("mod_beruf_arbeitsmarkt_bl_gender_verlauf_ui_1", r)
  #mod_beruf_arbeitsmarkt_bl_gender_vergleich_server("beruf_arbeitsmarkt_bl_gender_vergleich_ui_1", r)

  # Box 8
  mod_beruf_arbeitsmarkt_landkreis_map_server("mod_beruf_arbeitsmarkt_landkreis_map_ui_1", r)
  mod_beruf_arbeitsmarkt_landkreis_vergleich_server("mod_beruf_arbeitsmarkt_landkreis_vergleich_ui_1", r)
  #mod_beruf_arbeitsmarkt_landkreis_table_lk_server("mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_1", r )
  #mod_beruf_arbeitsmarkt_landkreis_table_lk_server("mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_2", r )
  #mod_beruf_arbeitsmarkt_landkreis_table_lk_server("mod_beruf_arbeitsmarkt_landkreis_table_lk_ui_3", r)

  # was ist das?
  #mod_beruf_arbeitsmarkt_landkreis_table_lk_analysis_server("xxxxxxxxxxx", btn, r)

  # Box international
  #mod_international_start_server("mod_international_start_ui_1", r)
  mod_international_map_arb_server("mod_international_map_arb_ui_1",r)
  mod_international_map_arb_gender_server("mod_international_map_arb_gender_ui_1",r)
  mod_international_top10_mint_arb_server("mod_international_top10_mint_arb_ui_1", r)
  mod_international_top10_mint_arb_gender_server("mod_international_top10_mint_arb_gender_ui_1", r)
  mod_international_arbeitsmarkt_vergleich_server("international_arbeitsmarkt_vergleich_1", r)


}

## To be copied in the UI
# mod_beruf_ui("beruf_1")

## To be copied in the server
# mod_beruf_server("beruf_1")
