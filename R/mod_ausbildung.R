#' ausbildung UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ausbildung_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' ausbildung Server Functions
#'
#' @noRd
mod_ausbildung_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_ausbildung_ui("ausbildung_1")

## To be copied in the server
# mod_ausbildung_server("ausbildung_1")
