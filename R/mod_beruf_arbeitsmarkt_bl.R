#' beruf_arbeitsmarkt_bl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_bl_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_bl"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    ),
    p("Auswahl des Anforderungsniveaus:"),
    shinyWidgets::pickerInput(
      inputId = ns("anforderungsniveau_arbeitsmarkt_bl"),
      choices = c("Gesamt", "Fachkraft", "Spezialist*in", "Expert*in")
    )
  )
}

#' beruf_arbeitsmarkt_bl Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_bl_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_bl, {
      r$date_arbeitsmarkt_bl <- input$date_arbeitsmarkt_bl
    })

    observeEvent(input$anforderungsniveau_arbeitsmarkt_bl, {
      r$anforderungsniveau_arbeitsmarkt_bl <- input$anforderungsniveau_arbeitsmarkt_bl
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_bl_ui("beruf_arbeitsmarkt_bl_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_bl_server("beruf_arbeitsmarkt_bl_1")
