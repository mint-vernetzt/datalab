#' studium_studienzahl_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studierende_verlauf"),
      label = NULL,
      choices = c("2012", "2013", "2014", "2015", "2016", "2017",
                  "2018","2019"),
      selected = c("2015", "2019")
    ),
    p("Wähle den Status der Student*innen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("indikator_studierende_verlauf"),
      choices = c("Studierende", "Studienanfänger"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Soll nur Lehramt angezeigt werden?"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("nurLehramt_studierende_verlauf"),
      choices = c("Ja", "Nein")
    ),
    p("Wähle ob MINT oder alle anderen Studiefächen dargestellt werden sollen:"),
    shinyWidgets::pickerInput(
      inputId = ns("topic_studierende_verlauf"),
      choices = c("MINT", "Alle anderen Studienfächer" = "andere Studiengänge"),
      selected = "MINT"
    ),
    p("Sollen die Bundesländern auf Ost und West aggregiert werden?"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("ost_west"),
      choices = c("Ja", "Nein"),
      selected = "Nein"
    ),
    conditionalPanel(condition = "input.ost_west == 'Nein'",
                     ns = ns,
    p("Wähle ein oder mehrer Bundesländer:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_studierende_verlauf"),
      choices = c("Berlin",
                  "Brandenburg",
                  "Bremen",
                  "Hamburg",
                  "Hessen",
                  "Mecklenburg-Vorpommern",
                  "Niedersachsen",
                  "Nordrhein-Westfalen",
                  "Rheinland-Pfalz",
                  "Saarland",
                  "Sachsen",
                  "Sachsen-Anhalt",
                  "Schleswig-Holstein",
                  "Thüringen"),
      multiple = TRUE,
      selected = c("Hessen", "Hamburg")
    )),
    conditionalPanel(condition = "input.ost_west == 'Ja'",
                     ns = ns)
  )
}

#' studium_studienzahl_verlauf Server Functions
#'
#' @noRd
mod_studium_studienzahl_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_studierende_verlauf, {
      r$date_studierende_verlauf <- input$date_studierende_verlauf
    })

    observeEvent(input$indikator_studierende_verlauf, {
      r$indikator_studierende_verlauf <- input$indikator_studierende_verlauf
    })

    observeEvent(input$topic_studierende_verlauf, {
      r$topic_studierende_verlauf <- input$topic_studierende_verlauf
    })

    observeEvent(input$states_studierende_verlauf, {
      r$states_studierende_verlauf <- input$states_studierende_verlauf
    })

    observeEvent(input$nurLehramt_studierende_verlauf, {
      r$nurLehramt_studierende_verlauf <- input$nurLehramt_studierende_verlauf
    })

    observeEvent(input$ost_west, {
      r$ost_west <- input$ost_west
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_verlauf_ui("studium_studienzahl_verlauf_1")

## To be copied in the server
# mod_studium_studienzahl_verlauf_server("studium_studienzahl_verlauf_1")
