#' beruf_arbeitsmarkt_überblick_fächer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_überblick_fächer_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl des Bundeslands:"),
    shinyWidgets::pickerInput(
      inputId = ns("state_arbeitsmarkt_überblick_fächer"),
      choices = c("Baden-Württemberg",
                  "Bayern",
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
                  "Thüringen"),
      multiple = FALSE,
      selected = "Sachsen-Anhalt"
    ),
    p("Auswahl der Beschäftigungsform:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_arbeitsmarkt_überblick_fächer"),
      choices = c("Beschäftigte",
                  "Auszubildende",
                  "Auszubildende (1. Jahr)",
                  "ausländische Beschäftigte",
                  "ausländische Auszubildende"),
      multiple = FALSE,
      selected = "Beschäftigte")
    )

}

#' beruf_arbeitsmarkt_überblick_fächer Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_überblick_fächer_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$state_arbeitsmarkt_überblick_fächer, {
      r$state_arbeitsmarkt_überblick_fächer <- input$state_arbeitsmarkt_überblick_fächer
    })

    observeEvent(input$indikator_arbeitsmarkt_überblick_fächer, {
      r$indikator_arbeitsmarkt_überblick_fächer <- input$indikator_arbeitsmarkt_überblick_fächer
    })


  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_überblick_fächer_ui

## To be copied in the server
# mod_beruf_arbeitsmarkt_überblick_fächer_server
