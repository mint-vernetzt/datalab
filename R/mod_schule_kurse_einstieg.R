#' schule_kurse_einstieg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_einstieg_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_einstieg"),
      label = NULL,
      choices = c("2010", "2011", "2012", "2014", "2015", "2016", "2017",
                  "2018","2019"),
      selected = c("2015", "2019")
    ),
    p("Wähle in welcher Formd der Kurs belegt wurde:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("indikator_kurse_einstieg"),
      choices = c("Grundkurs" = "Grundkurse", "Leistungskurs" = "Leistungskurse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Wähle ein oder mehrere Vergleichspunkte:"),
    shinyWidgets::pickerInput(
      inputId = ns("geschlecht_kurse_einstieg"),
      choices = c("Gesamt", "Frauen", "Männer"),
      multiple = TRUE,
      selected = "Gesamt"
    ),
    p("Absolut oder Relativ?"),
    shinyWidgets::materialSwitch(
      inputId = ns("switch_rel_abs"),
      label = "Relativ"
    )
  )
}

#' schule_kurse_einstieg Server Functions
#'
#' @noRd
mod_schule_kurse_einstieg_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse_einstieg, {
      r$date_kurse_einstieg <- input$date_kurse_einstieg
    })

    observeEvent(input$indikator_kurse_einstieg, {
      r$indikator_kurse_einstieg <- input$indikator_kurse_einstieg
    })

    observeEvent(input$switch_rel_abs, {
      r$switch_rel_abs <- input$switch_rel_abs
    })

    observeEvent(input$geschlecht_kurse_einstieg, {
      r$geschlecht_kurse_einstieg <- input$geschlecht_kurse_einstieg
    })

    observeEvent(input$switch_rel_abs, {
      r$switch_rel_abs <- input$switch_rel_abs
    })


  })
}

## To be copied in the UI
# mod_schule_kurse_einstieg_ui("schule_kurse_einstieg_1")

## To be copied in the server
# mod_schule_kurse_einstieg_server("schule_kurse_einstieg_1")
