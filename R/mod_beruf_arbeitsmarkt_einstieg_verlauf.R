#' beruf_arbeitsmarkt_einstieg_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_einstieg_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_einstieg_verlauf"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = c("2015", "2020")
    )
  )
}

#' beruf_arbeitsmarkt_einstieg_verlauf Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_einstieg_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_arbeitsmarkt_einstieg_verlauf, {
      r$date_arbeitsmarkt_einstieg_verlauf <- input$date_arbeitsmarkt_einstieg_verlauf
    })


  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_einstieg_verlauf_ui("beruf_arbeitsmarkt_einstieg_verlauf_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_einstieg_verlauf_server("beruf_arbeitsmarkt_einstieg_verlauf_1")
