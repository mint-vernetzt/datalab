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

  logger::log_debug("start mod_international_ui")

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
mod_international_server <- function(id,
                                     data_studierende_absolventen_weltweit,
                                     data_studierende_anzahl_oecd,
                                     data_studierende_europa,
                                     data_countries_names,
                                     r){

  logger::log_debug("Setup Seiten-Module: INTERNATIONAL")
  r <- reactiveValues()

  # Box 1 - Studium
  mod_international_start_server("mod_international_start_ui_1", r)
  mod_international_map_server("mod_international_map_ui_1", r)
  mod_international_top10_mint_server("international_top10_mint_1", r)
  mod_international_top10_mint_gender_server("international_top10_mint_gender_1", r)

  # Box 2 - Schule
  mod_international_schule_map_server("international_schule_map_1", r)
  mod_international_schule_item_server("international_schule_item_1", r)
  mod_international_schule_migration_server("international_schule_migration_1", r)

  # Box 3 - Arbeitsmarkt
  mod_international_arbeitsmarkt_map_server("international_arbeitsmarkt_map_1", r)
  mod_international_arbeitsmarkt_top10_server("international_arbeitsmarkt_top10_1", r)
  mod_international_arbeitsmarkt_vergleich_server("international_arbeitsmarkt_vergleich_1", r)


  logger::log_debug("Seiten-Module INTERNATIONAL done")
}

## To be copied in the UI
# mod_international_ui("international_1")

## To be copied in the server
# mod_international_server("international_1")
