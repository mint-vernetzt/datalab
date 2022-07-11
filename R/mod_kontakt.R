#' kontakt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_kontakt_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' kontakt Server Functions
#'
#' @noRd 
mod_kontakt_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_kontakt_ui("kontakt_1")
    
## To be copied in the server
# mod_kontakt_server("kontakt_1")
