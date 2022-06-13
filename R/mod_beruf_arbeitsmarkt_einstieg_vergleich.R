#' beruf_arbeitsmarkt_einstieg_vergleich UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_einstieg_vergleich_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_einstieg_vergleich"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = "2020"
    )
  )
}

#' beruf_arbeitsmarkt_einstieg_vergleich Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_einstieg_vergleich_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_einstieg_vergleich, {
      r$date_arbeitsmarkt_einstieg_vergleich <- input$date_arbeitsmarkt_einstieg_vergleich
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_einstieg_vergleich_ui("beruf_arbeitsmarkt_einstieg_vergleich_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_einstieg_vergleich_server("beruf_arbeitsmarkt_einstieg_vergleich_1")