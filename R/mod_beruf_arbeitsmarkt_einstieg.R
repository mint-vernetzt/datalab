#' beruf_arbeitsmarkt_einstieg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_einstieg_ui <- function(id){
  ns <- NS(id)

  tagList(
    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_einstieg"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = "2020"
    ),
    p("Nach Geschlecht aufteilen?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("gender_switch"), label = "Nein", inline = TRUE),
      tags$span("Ja")
    )
  )
}

#' beruf_arbeitsmarkt_einstieg Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_einstieg_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_einstieg, {
      r$date_arbeitsmarkt_einstieg <- input$date_arbeitsmarkt_einstieg
    })

    observeEvent(input$gender_switch, {
      r$gender_switch <- input$gender_switch
    })

  })

}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_einstieg_ui("beruf_arbeitsmarkt_einstieg_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_einstieg_server("beruf_arbeitsmarkt_einstieg_1")
