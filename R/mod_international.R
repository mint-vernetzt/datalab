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
mod_international_server <- function(id,
                                     data_studierende_absolventen_weltweit,
                                     data_studierende_anzahl_oecd,
                                     data_studierende_europa,
                                     data_countries_names,
                                     data_arbeitsmarkt_absolvent_oecd,
                                     data_arbeitsmarkt_anfänger_absolv_oecd,
                                     data_arbeitsmarkt_anzahl_azubis_oecd,
                                     data_arbeitsmarkt_beschäftigte_eu,
                                     #data_studierende_intern_oecd,
                                     r){

  logger::log_debug("Setup Seiten-Module: INTERNATIONAL")
  r <- reactiveValues()

  # Box 1
  mod_international_start_server("mod_international_start_ui_1", r)
  mod_international_map_server("mod_international_map_ui_1", r)
  mod_international_top10_mint_server("international_top10_mint_1", r)
  mod_international_top10_mint_gender_server("international_top10_mint_gender_1", r)
  mod_international_map_fem_server("international_map_fem_ui_1", r)
  mod_international_top10_mint_intl_server("mod_international_top10_mint_intl_ui_1", r)


  # Box 2
  #mod_international_start_server("mod_international_start_ui_1", r)
  mod_international_map_arb_server("mod_international_map_arb_ui_1",r)
  mod_international_map_arb_gender_server("mod_international_map_arb_gender_ui_1",r)
  mod_international_top10_mint_arb_server("mod_international_top10_mint_arb_ui_1", r)
  mod_international_top10_mint_arb_gender_server("mod_international_top10_mint_arb_gender_ui_1", r)


  logger::log_debug("Seiten-Module INTERNATIONAL done")
}

## To be copied in the UI
# mod_international_ui("international_1")

## To be copied in the server
# mod_international_server("international_1")