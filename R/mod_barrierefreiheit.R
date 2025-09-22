#' datenschutz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.


#' kontakt Server Functions
#'
#' @noRd
mod_barrierefreiheit_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}


#' @noRd
#'
#' @importFrom shiny NS tagList
mod_barrierefreiheit_ui <- function(id){
  ns <- NS(id)
  tagList(

h1("Barrierefreiheitserklärung")

  )
}
