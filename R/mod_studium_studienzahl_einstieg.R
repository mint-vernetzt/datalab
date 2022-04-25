#' studium_studienzahl_einstieg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_einstieg_ui <- function(id){
  ns <- NS(id)

  tagList(
    p("Wähle einen Zeitraum:"),
      shinyWidgets::sliderTextInput(
        inputId = ns("date_studierende_einstieg"),
        label = NULL,
        choices = c("2012", "2013", "2014", "2015", "2016", "2017",
                    "2018","2019"),
        selected = c("2015", "2019")
      ),
    p("Wähle den Status der Student*innen:"),
      shinyWidgets::radioGroupButtons(
        inputId = ns("indikator_studierende_einstieg"),
        choices = c("Studierende", "Studienanfänger"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      ),
    p("Wähle ein oder mehrere Vergleichspunkte:"),
      shinyWidgets::pickerInput(
        inputId = ns("geschlecht_studierende_einstieg"),
        choices = c("gesamt", "frauen", "männer"),
        multiple = TRUE,
        selected = "gesamt"
      ),
    p("Soll der Anteil von Lehramt angezeigt werden?"),
      shinyWidgets::materialSwitch(
        inputId = ns("nurLehramt_studierende_einstieg"),
        label = "Nein"
      ),
    p("Absolut oder Relativ?"),
    shinyWidgets::materialSwitch(
      inputId = ns("switch_rel_abs"),
      label = "Relativ"
    )
  )
}

#' studium_studienzahl_einstieg Server Functions
#'
#' @noRd
mod_studium_studienzahl_einstieg_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_studierende_einstieg, {
      r$date_studierende_einstieg <- input$date_studierende_einstieg
    })

    observeEvent(input$indikator_studierende_einstieg, {
      r$indikator_studierende_einstieg <- input$indikator_studierende_einstieg
    })

    observeEvent(input$geschlecht_studierende_einstieg, {
      r$geschlecht_studierende_einstieg <- input$geschlecht_studierende_einstieg
    })

    observeEvent(input$nurLehramt_studierende_einstieg, {
      r$nurLehramt_studierende_einstieg <- input$nurLehramt_studierende_einstieg
    })

    observeEvent(input$switch_rel_abs, {
      r$switch_rel_abs <- input$switch_rel_abs
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_einstieg_ui("studium_studienzahl_einstieg_1")

## To be copied in the server
# mod_studium_studienzahl_einstieg_server("studium_studienzahl_einstieg_1")
