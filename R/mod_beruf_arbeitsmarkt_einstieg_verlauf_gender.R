#' beruf_arbeitsmarkt_einstieg_verlauf_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Zeitraums:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_einstieg_verlauf_gender"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021"),
      selected = c("2016", "2021")
    ),
    p("Betrachtung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_arbeitsmarkt_einstieg_verlauf_gender"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    )
  )

}

#' beruf_arbeitsmarkt_einstieg_verlauf_gender Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_einstieg_verlauf_gender, {
      r$date_arbeitsmarkt_einstieg_verlauf_gender <- input$date_arbeitsmarkt_einstieg_verlauf_gender
    })

    observeEvent(input$abs_zahlen_arbeitsmarkt_einstieg_verlauf_gender, {
      r$abs_zahlen_arbeitsmarkt_einstieg_verlauf_gender <- input$abs_zahlen_arbeitsmarkt_einstieg_verlauf_gender
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui("beruf_arbeitsmarkt_einstieg_verlauf_gender_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_server("beruf_arbeitsmarkt_einstieg_verlauf_gender_1")
