#' international_arbeitsmarkt_top10 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_arbeitsmarkt_top10_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("COMING SOON - ARBEITSMARKT TOP 10")
  )
}

#' international_arbeitsmarkt_top10 Server Functions
#'
#' @noRd
mod_international_arbeitsmarkt_top10_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_international_arbeitsmarkt_top10_ui("international_arbeitsmarkt_top10_1")

## To be copied in the server
# mod_international_arbeitsmarkt_top10_server("international_arbeitsmarkt_top10_1")
