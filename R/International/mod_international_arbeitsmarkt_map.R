#' international_arbeitsmarkt_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_arbeitsmarkt_map_ui <- function(id){
  ns <- NS(id)
  tagList(
  p("COMING SOON - ARBEITSMARKT MAP")
  )
}

#' international_arbeitsmarkt_map Server Functions
#'
#' @noRd
mod_international_arbeitsmarkt_map_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_international_arbeitsmarkt_map_ui("international_arbeitsmarkt_map_1")

## To be copied in the server
# mod_international_arbeitsmarkt_map_server("international_arbeitsmarkt_map_1")
