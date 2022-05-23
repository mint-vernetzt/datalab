#' beruf_arbeitsmarkt_verlauf_bl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_verlauf_bl_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_verlauf_bl"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = c("2015", "2020")
    ),
    p("Wähle ein Anforderungsniveau:"),
    shinyWidgets::pickerInput(
      inputId = ns("anforderungsniveau_arbeitsmarkt_verlauf_bl"),
      choices = c("Gesamt", "Fachkraft", "Spezialist", "Experte")
    ),
    p("Wähle ob MINT oder alle anderen Fachbereiche dargestellt werden sollen:"),
    shinyWidgets::pickerInput(
      inputId = ns("topic_arbeitsmarkt_verlauf_bl"),
      choices = c("MINT", "andere Berufszweige"),
      selected = "MINT"
    ),
    p("Wähle ein Bundesländer:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_arbeitsmarkt_verlauf_bl"),
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
                  "Thüringen",
                  "Osten",
                  "Westen"),
      selected = "Hessen"
    )
  )
}

#' beruf_arbeitsmarkt_verlauf_bl Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_verlauf_bl_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_verlauf_bl, {
      r$date_arbeitsmarkt_verlauf_bl <- input$date_arbeitsmarkt_verlauf_bl
    })


    observeEvent(input$topic_arbeitsmarkt_verlauf_bl, {
      r$topic_arbeitsmarkt_verlauf_bl <- input$topic_arbeitsmarkt_verlauf_bl
    })

    observeEvent(input$states_arbeitsmarkt_verlauf_bl, {
      r$states_arbeitsmarkt_verlauf_bl <- input$states_arbeitsmarkt_verlauf_bl
    })

    observeEvent(input$anforderungsniveau_arbeitsmarkt_verlauf_bl, {
      r$anforderungsniveau_arbeitsmarkt_verlauf_bl <- input$anforderungsniveau_arbeitsmarkt_verlauf_bl
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_verlauf_bl_ui("beruf_arbeitsmarkt_verlauf_bl_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_verlauf_bl_server("beruf_arbeitsmarkt_verlauf_bl_1")
