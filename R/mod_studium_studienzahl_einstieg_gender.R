#' studium_studienzahl_einstieg_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_einstieg_gender_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studierende_einstieg_gender"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = "2020"
    ),
    p("Nur Lehramt angzeigen:?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studierende_einstieg_gender"), label = "Nein", inline = TRUE),
      tags$span("Ja")),
    p("Auswahl der Hochschulform:"),
    conditionalPanel(condition = "input.nurLehramt_studierende_einstieg_gender == false",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("hochschulform_studierende_einstieg_1_gender"),
                       choices = c("insgesamt", "Uni", "FH")
                     )),
    conditionalPanel(condition = "input.nurLehramt_studierende_einstieg_gender != false",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("hochschulform_studierende_einstieg_2_gender"),
                       choices = "Uni"
                     ))

  )
}

#' studium_studienzahl_einstieg_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_einstieg_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_studierende_einstieg_gender, {
      r$date_studierende_einstieg_gender <- input$date_studierende_einstieg_gender
    })


    observeEvent(input$nurLehramt_studierende_einstieg_gender, {
      r$nurLehramt_studierende_einstieg_gender <- input$nurLehramt_studierende_einstieg_gender
    })

    observeEvent(input$hochschulform_studierende_einstieg_1_gender, {
      r$hochschulform_studierende_einstieg_1_gender <- input$hochschulform_studierende_einstieg_1_gender
    })


    observeEvent(input$hochschulform_studierende_einstieg_2_gender, {
      r$hochschulform_studierende_einstieg_2_gender <- input$hochschulform_studierende_einstieg_2_gender
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_einstieg_gender_ui("studium_studienzahl_einstieg_gender_1")

## To be copied in the server
# mod_studium_studienzahl_einstieg_gender_server("studium_studienzahl_einstieg_gender_1")
