#' beruf_arbeitsmarkt_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_verlauf"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019"),
      selected = c("2015", "2019")
    ),
    p("Wähle den Status der Arbeitnehmer*innen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("indikator_arbeitsmarkt_verlauf"),
      choices = c("Beschäftigte", "Auszubildende"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Wähle ein Anforderungsniveu:"),
    shinyWidgets::pickerInput(
      inputId = ns("anforderungsniveau_arbeitsmarkt_verlauf"),
      choices = c("Gesamt", "Fachkraft", "Spezialist", "Experte")
    ),
    p("Wähle ob MINT oder alle anderen Fachbereiche dargestellt werden sollen:"),
    shinyWidgets::pickerInput(
      inputId = ns("topic_arbeitsmarkt_verlauf"),
      choices = c("MINT", "andere Berufszweige"),
      selected = "MINT"
    ),
    p("Sollen die Bundesländer in Ost und West zusammengefasst werden?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("ost_west"), label = "Nein", inline = TRUE),
      tags$span("Ja")
    ),
    # shinyWidgets::radioGroupButtons(
    #   inputId = ns("ost_west"),
    #   choices = c("Ja", "Nein"),
    #   selected = "Nein"
    # ),
    conditionalPanel(condition = "input.ost_west == false",
                     ns = ns,
    p("Wähle ein oder mehrere Bundesländer:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_arbeitsmarkt_verlauf"),
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
      options = list(`actions-box` = TRUE,
                     `deselect-all-text` = "Keins auswählen",
                     `select-all-text` = "Alle auswählen"),
      selected = c("Hessen", "Hamburg")
    )),
    conditionalPanel(condition = "input.ost_west != false",
                     ns = ns)
  )
}

#' beruf_arbeitsmarkt_verlauf Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_verlauf, {
      r$date_arbeitsmarkt_verlauf <- input$date_arbeitsmarkt_verlauf
    })

    observeEvent(input$indikator_arbeitsmarkt_verlauf, {
      r$indikator_arbeitsmarkt_verlauf <- input$indikator_arbeitsmarkt_verlauf
    })

    observeEvent(input$topic_arbeitsmarkt_verlauf, {
      r$topic_arbeitsmarkt_verlauf <- input$topic_arbeitsmarkt_verlauf
    })

    observeEvent(input$states_arbeitsmarkt_verlauf, {
      r$states_arbeitsmarkt_verlauf <- input$states_arbeitsmarkt_verlauf
    })

    observeEvent(input$anforderungsniveau_arbeitsmarkt_verlauf, {
      r$anforderungsniveau_arbeitsmarkt_verlauf <- input$anforderungsniveau_arbeitsmarkt_verlauf
    })

    observeEvent(input$ost_west, {
      r$ost_west <- input$ost_west
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_verlauf_ui("beruf_arbeitsmarkt_verlauf_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_verlauf_server("beruf_arbeitsmarkt_verlauf_1")
