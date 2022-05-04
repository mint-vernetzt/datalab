#' beruf_arbeitsmarkt_einstieg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_einstieg_ui <- function(id){
  ns <- NS(id)

  tagList(
    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_einstieg"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019"),
      selected = c("2015", "2019")
    ),
    p("Wähle den Status der Arbeitnehmer*innen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("indikator_arbeitsmarkt_einstieg"),
      choices = c("Beschäftigte", "Auszubildende"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Wähle ein oder mehrere Vergleichspunkte:"),
    shinyWidgets::pickerInput(
      inputId = ns("geschlecht_arbeitsmarkt_einstieg"),
      choices = c("Gesamt", "Frauen", "Männer"),
      multiple = TRUE,
      selected = "Gesamt"
    ),
    p("Relativ oder Absolut?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("switch_rel_abs"), label = "Relativ", inline = TRUE),
      tags$span("Absolut")
    )
    # shinyWidgets::materialSwitch(
    #   inputId = ns("switch_rel_abs"),
    #   label = "Relativ"
    # )
  )
}

#' beruf_arbeitsmarkt_einstieg Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_einstieg_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_einstieg, {
      r$date_arbeitsmarkt_einstieg <- input$date_arbeitsmarkt_einstieg
    })

    observeEvent(input$indikator_arbeitsmarkt_einstieg, {
      r$indikator_arbeitsmarkt_einstieg <- input$indikator_arbeitsmarkt_einstieg
    })

    observeEvent(input$geschlecht_arbeitsmarkt_einstieg, {
      r$geschlecht_arbeitsmarkt_einstieg <- input$geschlecht_arbeitsmarkt_einstieg
    })

    observeEvent(input$switch_rel_abs, {
      r$switch_rel_abs <- input$switch_rel_abs
    })

  })

}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_einstieg_ui("beruf_arbeitsmarkt_einstieg_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_einstieg_server("beruf_arbeitsmarkt_einstieg_1")
