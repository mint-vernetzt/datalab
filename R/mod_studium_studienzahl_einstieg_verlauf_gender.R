#' studium_studienzahl_einstieg_verlauf_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_einstieg_verlauf_gender_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Wähle einen Zeitpunkt:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studienzahl_einstieg_verlauf_gender"),
      label = NULL,
      choices = c(2010, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = c(2015, 2020)
    ),
    p("Soll nur Lehramt angezeigt werden?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studierende_einstieg_verlauf_gender"), label = "Nein", inline = TRUE),
      tags$span("Ja")
    ),
    p("Wähle eine Hochschulform:"),
    conditionalPanel(condition = "input.nurLehramt_studierende_einstieg_verlauf_gender == false",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("hochschulform_studierende_einstieg_verlauf_gender_1"),
                       choices = c("insgesamt", "Uni", "FH")
                     )),
    conditionalPanel(condition = "input.nurLehramt_studierende_einstieg_verlauf_gender != false",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("hochschulform_studierende_einstieg_verlauf_gender_2"),
                       choices = "Uni"
                     ))

  )
}

#' studium_studienzahl_einstieg_verlauf_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_einstieg_verlauf_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_studienzahl_einstieg_verlauf_gender, {
      r$date_studienzahl_einstieg_verlauf_gender <- input$date_studienzahl_einstieg_verlauf_gender
    })

    observeEvent(input$nurLehramt_studierende_einstieg_verlauf_gender, {
      r$nurLehramt_studierende_einstieg_verlauf_gender <- input$nurLehramt_studierende_einstieg_verlauf_gender
    })


    observeEvent(input$hochschulform_studierende_einstieg_verlauf_gender_1, {
      r$hochschulform_studierende_einstieg_verlauf_gender_1 <- input$hochschulform_studierende_einstieg_verlauf_gender_1
    })


    observeEvent(input$hochschulform_studierende_einstieg_verlauf_gender_2, {
      r$hochschulform_studierende_einstieg_verlauf_gender_2 <- input$hochschulform_studierende_einstieg_verlauf_gender_2
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_einstieg_verlauf_gender_ui("studium_studienzahl_einstieg_verlauf_gender_1")

## To be copied in the server
# mod_studium_studienzahl_einstieg_verlauf_gender_server("studium_studienzahl_einstieg_verlauf_gender_1")