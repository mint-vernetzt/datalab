#' beruf_arbeitsmarkt_anforderungen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_anforderungen_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wählen Sie einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_anforderungen"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    )
  )
}

#' beruf_arbeitsmarkt_anforderungen Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_anforderungen_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_anforderungen, {
      r$date_arbeitsmarkt_anforderungen <- input$date_arbeitsmarkt_anforderungen
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_anforderungen_ui("beruf_arbeitsmarkt_anforderungen_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_anforderungen_server("beruf_arbeitsmarkt_anforderungen_1")
