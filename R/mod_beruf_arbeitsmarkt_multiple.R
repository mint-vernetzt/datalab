#' beruf_arbeitsmarkt_multiple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_multiple_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitpunkt:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    ),
    p("Wähle ein Anforderungsniveau:"),
    shinyWidgets::pickerInput(
      inputId = ns("anforderungsniveau_arbeitsmarkt"),
      choices = c("Gesamt", "Fachkraft", "Spezialist", "Experte")
    )
  )
}

#' beruf_arbeitsmarkt_multiple Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_multiple_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt, {
      r$date_arbeitsmarkt <- input$date_arbeitsmarkt
    })


    observeEvent(input$anforderungsniveau_arbeitsmarkt, {
      r$anforderungsniveau_arbeitsmarkt <- input$anforderungsniveau_arbeitsmarkt
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_multiple_ui("beruf_arbeitsmarkt_multiple_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_multiple_server("beruf_arbeitsmarkt_multiple_1")
