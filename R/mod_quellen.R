#' quellen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_quellen_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' quellen Server Functions
#'
#' @noRd 
mod_quellen_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_quellen_ui("quellen_1")
    
## To be copied in the server
# mod_quellen_server("quellen_1")
