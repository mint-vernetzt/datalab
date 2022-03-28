#' studium_compare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_studium_compare_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' studium_compare Server Functions
#'
#' @noRd 
mod_studium_compare_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_studium_compare_ui("studium_compare_1")
    
## To be copied in the server
# mod_studium_compare_server("studium_compare_1")
