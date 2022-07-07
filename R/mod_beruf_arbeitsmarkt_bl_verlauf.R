#' beruf_arbeitsmarkt_bl_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_bl_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Wählen Sie einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_beruf_arbeitsmarkt_bl_verlauf"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = c("2015", "2020")
    ),
    p("Wählen Sie eine Beschäftigungsform der Arbeitnehmer*innen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("indikator_beruf_arbeitsmarkt_bl_verlauf"),
      choices = c("Auszubildende", "Beschäftigte"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Wählen Sie ein Anforderungsniveau:"),
    shinyWidgets::pickerInput(
      inputId = ns("anforderungsniveau_beruf_arbeitsmarkt_bl_verlauf"),
      choices = c("Gesamt", "Fachkraft", "Spezialist", "Experte"),
      selected = "Gesamt"
    ),
    p("Auswahl Bundesland oder Region (Mehrfachauswahl möglich):"),
    shinyWidgets::pickerInput(
      inputId = ns("states_beruf_arbeitsmarkt_bl_verlauf"),
      choices = list(
        Regionen = c("Deutschland", "Westen", "Osten"),
        Bundesländer = c( "Bayern",
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
                          "Thüringen")),
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `deselect-all-text` = "Alle abwählen",
                     `select-all-text` = "Alle auswählen"),
      selected = c("Hessen", "Hamburg")
    )
  )
}

#' beruf_arbeitsmarkt_bl_verlauf Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_bl_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_beruf_arbeitsmarkt_bl_verlauf, {
      r$date_beruf_arbeitsmarkt_bl_verlauf <- input$date_beruf_arbeitsmarkt_bl_verlauf
    })

    observeEvent(input$indikator_beruf_arbeitsmarkt_bl_verlauf, {
      r$indikator_beruf_arbeitsmarkt_bl_verlauf <- input$indikator_beruf_arbeitsmarkt_bl_verlauf
    })

    observeEvent(input$anforderungsniveau_beruf_arbeitsmarkt_bl_verlauf, {
      r$anforderungsniveau_beruf_arbeitsmarkt_bl_verlauf <- input$anforderungsniveau_beruf_arbeitsmarkt_bl_verlauf
    })

    observeEvent(input$states_beruf_arbeitsmarkt_bl_verlauf, {
      r$states_beruf_arbeitsmarkt_bl_verlauf <- input$states_beruf_arbeitsmarkt_bl_verlauf
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_bl_verlauf_ui("beruf_arbeitsmarkt_bl_verlauf_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_bl_verlauf_server("beruf_arbeitsmarkt_bl_verlauf_1")
