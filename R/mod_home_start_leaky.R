#' home_start_leaky UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_leaky_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_start_leaky"),
      label = NULL,
      choices = c(2013,2014,2015, 2016, 2017, 2018, 2019),
    )
  )
}

#' home_start_leaky Server Functions
#'
#' @noRd
mod_home_start_leaky_server <- function(id, r){
  moduleServer( id, function(input, output, session){

  observeEvent(input$date_start_leaky, {
    r$date_start_leaky <- input$date_start_leaky
  })

  })
}

## To be copied in the UI
# mod_home_start_leaky_ui("home_start_leaky_1")

## To be copied in the server
# mod_home_start_leaky_server("home_start_leaky_1")
