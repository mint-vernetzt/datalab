#' studium_studienzahl_einstieg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_einstieg_ui <- function(id){
  ns <- NS(id)

  tagList(
    p("Wähle einen Zeitraum:"),
      shinyWidgets::sliderTextInput(
        inputId = ns("date_studierende_einstieg"),
        label = NULL,
        choices = c("2012", "2013", "2014", "2015", "2016", "2017",
                    "2018","2019", "2020"),
        selected = "2020"
      ),
    p("Soll der Anteil von Lehramt angezeigt werden?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studierende_einstieg"), label = "Nein", inline = TRUE),
      tags$span("Ja"))

  )
}

#' studium_studienzahl_einstieg Server Functions
#'
#' @noRd
mod_studium_studienzahl_einstieg_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_studierende_einstieg, {
      r$date_studierende_einstieg <- input$date_studierende_einstieg
    })


    observeEvent(input$nurLehramt_studierende_einstieg, {
      r$nurLehramt_studierende_einstieg <- input$nurLehramt_studierende_einstieg
    })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_einstieg_ui("studium_studienzahl_einstieg_1")

## To be copied in the server
# mod_studium_studienzahl_einstieg_server("studium_studienzahl_einstieg_1")
