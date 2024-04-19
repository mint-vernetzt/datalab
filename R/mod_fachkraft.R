#' Fachkraft-Fokus UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_ui <- function(id){

  #logger::log_debug("start mod_international_ui")

  ns <- NS(id)

  tagList(
    fluidPage(
      fluidRow(
        mod_fachkraft_start_ui("mod_fachkraft_start_ui_1")
      )
    )
  )


}

#' Fachkraft-Fokus Server Functions
#'
#' @noRd
mod_fachkraft_server <- function(id,
                                 r){

  #logger::log_debug("Setup Seiten-Module: INTERNATIONAL")
  r <- reactiveValues()

  mod_fachkraft_start_server("mod_fachkraft_start_ui_1", r)

  # box 4 fachkräfte
  mod_fachkraft_item_epa_server("fachkraft_item_epa_1", r)
  mod_fachkraft_item_mint_server("fachkraft_item_mint_1", r)
  mod_fachkraft_item_detail_server("fachkraft_item_detail_1", r)
  mod_fachkraft_bar_vakanz_server("fachkraft_bar_vakanz_1", r)
  mod_fachkraft_item_prog_server("fachkraft_item_prog_1", r)
  mod_fachkraft_item_prog_detail_server("fachkraft_item_prog_detail_1", r)
  mod_fachkraft_wirkhebel_analyse_server("fachkraft_item_wirkhebel_analyse_1", r)



  # box 5 International Table
  # mod_international_table_input_server("international_table_input_1", r)





  #logger::log_debug("Seiten-Module INTERNATIONAL done")
}

## To be copied in the UI
# mod_international_ui("international_1")

## To be copied in the server
# mod_international_server("international_1")
