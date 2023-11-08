#' fachkraft_item_mint UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_item_mint_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("COMING SOON - mod_fachkraft_item_mint_ui")
  )
}

#' fachkraft_item_mint Server Functions
#'
#' @noRd
mod_fachkraft_item_mint_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_fachkraft_item_mint_ui("fachkraft_item_mint_1")

## To be copied in the server
# mod_fachkraft_item_mint_server("fachkraft_item_mint_1")
