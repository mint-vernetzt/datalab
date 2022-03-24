#' beruf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_beruf_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' beruf Server Functions
#'
#' @noRd 
mod_beruf_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_beruf_ui("beruf_1")
    
## To be copied in the server
# mod_beruf_server("beruf_1")
