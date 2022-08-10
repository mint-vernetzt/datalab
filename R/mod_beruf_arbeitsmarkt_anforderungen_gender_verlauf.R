#' beruf_arbeitsmarkt_anforderungen_gender_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_anforderungen_gender_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Zeitraums:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_anforderungen_gender_verlauf"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = c("2015", "2020")
    ),
    p("Auswahl  des Anforderungsniveaus:"),
    shinyWidgets::pickerInput(
      inputId = ns("level_arbeitsmarkt_anforderungen_gender_verlauf"),
      choices = c("Gesamt", "Fachkraft", "Spezialist", "Experte"),
      selected = "Gesamt"
    ),
    p("Auswahl des Bundeslands:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_arbeitsmarkt_anforderungen_gender_verlauf"),
      choices = c("Deutschland",
                  "Baden-Württemberg",
                  "Berlin",
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
                  "Westen",
                  "Osten"),
      multiple = FALSE,
      selected = c("Hessen")
    )
  )
}

#' beruf_arbeitsmarkt_anforderungen_gender_verlauf Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_anforderungen_gender_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_anforderungen_gender_verlauf, {
      r$date_arbeitsmarkt_anforderungen_gender_verlauf <- input$date_arbeitsmarkt_anforderungen_gender_verlauf
    })

    observeEvent(input$level_arbeitsmarkt_anforderungen_gender_verlauf, {
      r$level_arbeitsmarkt_anforderungen_gender_verlauf <- input$level_arbeitsmarkt_anforderungen_gender_verlauf
    })

    observeEvent(input$states_arbeitsmarkt_anforderungen_gender_verlauf, {
      r$states_arbeitsmarkt_anforderungen_gender_verlauf <- input$states_arbeitsmarkt_anforderungen_gender_verlauf
    })


  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_anforderungen_gender_verlauf_ui("beruf_arbeitsmarkt_anforderungen_gender_verlauf_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_anforderungen_gender_verlauf_server("beruf_arbeitsmarkt_anforderungen_gender_verlauf_1")
