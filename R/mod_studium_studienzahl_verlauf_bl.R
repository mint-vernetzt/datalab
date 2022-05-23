#' studium_studienzahl_verlauf_bl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_verlauf_bl_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studierende_verlauf_bl"),
      label = NULL,
      choices = c("2012", "2013", "2014", "2015", "2016", "2017",
                  "2018","2019"),
      selected = c("2015", "2019")
    ),
    p("Soll nur Lehramt angezeigt werden?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studierende_verlauf_bl"), label = "Nein", inline = TRUE),
      tags$span("Ja")
    ),
    p("Einzelne Fächer oder als MINT zusammengefasst:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("subjects_aggregated_bl"),
      choices = c("einzeln", "aggregiert"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon")),
      selected = "aggregiert"
    ),
    conditionalPanel(condition = "input.subjects_aggregated_bl == 'aggregiert'",
                     ns = ns,
                     p("Wähle ob MINT oder alle anderen Studienfächer dargestellt werden sollen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("topic_studierende_verlauf_bl"),
                       choices = c("MINT", "Alle anderen Studienfächer" = "andere Studiengänge"),
                       selected = "MINT"
                     )),
    conditionalPanel(condition = "input.subjects_aggregated_bl == 'einzeln'",
                     ns = ns,
                     p("Wähle ein Fach:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("subject_selected_bl"),
                       choices = c("Mathematik" = "Mathe", "Ingenieurswesen" = "Ingenieur"),
                       selected = "Mathematik"
                     )),
    p("Wähle ein oder mehrere Bundesländer:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_studierende_verlauf_bl"),
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

#' studium_studienzahl_verlauf_bl Server Functions
#'
#' @noRd
mod_studium_studienzahl_verlauf_bl_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_studierende_verlauf_bl, {
      r$date_studierende_verlauf_bl <- input$date_studierende_verlauf_bl
    })


    observeEvent(input$topic_studierende_verlauf_bl, {
      r$topic_studierende_verlauf_bl <- input$topic_studierende_verlauf_bl
    })

    observeEvent(input$states_studierende_verlauf_bl, {
      r$states_studierende_verlauf_bl <- input$states_studierende_verlauf_bl
    })

    observeEvent(input$nurLehramt_studierende_verlauf_bl, {
      r$nurLehramt_studierende_verlauf_bl <- input$nurLehramt_studierende_verlauf_bl
    })

    observeEvent(input$subjects_aggregated_bl, {
      r$subjects_aggregated_bl <- input$subjects_aggregated_bl
    })

    observeEvent(input$subject_selected_bl, {
      r$subject_selected_bl <- input$subject_selected_bl
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_verlauf_bl_ui("studium_studienzahl_verlauf_bl_1")

## To be copied in the server
# mod_studium_studienzahl_verlauf_bl_server("studium_studienzahl_verlauf_bl_1")
