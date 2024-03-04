#' startseite UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_startseite_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        mod_startseite_start_ui("mod_startseite_start_ui_1")
      )
    )
  )
}

#' beruf Server Functions
#'
#' @noRd
mod_startseite_server <- function(id){


  # Arbeitsmarkt
  mod_startseite_start_server("mod_startseite_start_ui_1")

}
