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
#' @noRd
mod_fachkraft_server <- function(id,
                                 r){
  r <- reactiveValues()

  # Startseite
  mod_fachkraft_start_server("mod_fachkraft_start_ui_1", r)

  # box 1
  mod_fachkraft_wirkhebel_analyse_server("fachkraft_item_wirkhebel_analyse_1", r)
  mod_fachkraft_item_prog_server("fachkraft_item_prog_1", r)
  mod_fachkraft_item_prog_alle_server("fachkraft_item_prog_alle_1", r)
  mod_fachkraft_item_prog_detail_server("fachkraft_item_prog_detail_1", r)

  # box 2
  mod_fachkraft_item_epa_server("fachkraft_item_epa_1", r)
  mod_fachkraft_item_epa_bulas_server("fachkraft_item_epa_bulas", r)
 # mod_fachkraft_item_mint_server("fachkraft_item_mint_1", r)
  mod_fachkraft_bar_vakanz_server("fachkraft_bar_vakanz_1", r)

  # box 3
  mod_fachkraft_item_detail_server("fachkraft_item_detail_1", r)


}

## To be copied in the UI
# mod_international_ui("international_1")

## To be copied in the server
# mod_international_server("international_1")
