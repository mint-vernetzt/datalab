#' international UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidPage(
      fluidRow(
        mod_international_start_ui("mod_international_start_ui_1")
      )
    )
  )


}

#' international Server Functions
#'
#' @noRd
mod_international_server <- function(id, r){

  r <- reactiveValues()

  # international Gesamtseite
  mod_international_start_server("mod_international_start_ui_1", r)

  # Box 1 - Schule
  mod_international_schule_map_server("international_schule_map_1", r)
  mod_international_schule_item_server("international_schule_item_1", r)
  mod_international_schule_migration_server("international_schule_migration_1", r)

  # Box 2 - Studium
  mod_international_map_server("mod_international_map_ui_1", r)
  mod_international_top10_mint_server("international_top10_mint_1", r)
  mod_international_top10_mint_gender_server("international_top10_mint_gender_1", r)
  mod_international_map_fem_server("international_map_fem_ui_1", r)
  mod_international_top10_mint_intl_server("mod_international_top10_mint_intl_ui_1", r)

  # Box 3 - Ausbildung
  #mod_international_start_server("mod_international_start_ui_1", r)
  mod_international_map_arb_server("mod_international_map_arb_ui_1",r)
  mod_international_map_arb_gender_server("mod_international_map_arb_gender_ui_1",r)
  mod_international_top10_mint_arb_server("mod_international_top10_mint_arb_ui_1", r)
  mod_international_top10_mint_arb_gender_server("mod_international_top10_mint_arb_gender_ui_1", r)
  mod_international_arbeitsmarkt_vergleich_server("international_arbeitsmarkt_vergleich_1", r)


}

## To be copied in the UI
# mod_international_ui("international_1")

## To be copied in the server
# mod_international_server("international_1")
