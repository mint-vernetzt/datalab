#' studium UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' studium Server Functions
#'
#' @noRd
mod_studium_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_studium_ui("studium_1")

## To be copied in the server
# mod_studium_server("studium_1")
