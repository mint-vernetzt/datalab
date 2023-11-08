#' fachkraft UI Function
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
        mod_fachkraft_start_ui("fachkraft_start_1")
      )
    )
  )
}

#' fachkraft Server Functions
#'
#' @noRd
mod_fachkraft_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    logger::log_debug("Setup Seiten-Module: FACHKRAFT")
    r <- reactiveValues()

    mod_fachkraft_start_server("fachkraft_start_1")

    # Box 1 - Arbeitsmarkt
    mod_fachkraft_item_epa_server("fachkraft_item_epa_1", r)
    mod_fachkraft_item_mint_server("fachkraft_item_mint_1", r)
    mod_fachkraft_item_detail_server("fachkraft_item_detail_1", r)

  })
}

## To be copied in the UI
# mod_fachkraft_ui("fachkraft_1")

## To be copied in the server
# mod_fachkraft_server("fachkraft_1")
