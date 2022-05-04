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
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studierende_verlauf"), label = "Nein", inline = TRUE),
      tags$span("Ja")
    ),
    # shinyWidgets::radioGroupButtons(
    #   inputId = ns("nurLehramt_studierende_verlauf"),
    #   choices = c("Ja", "Nein"),
    #   selected = "Nein"
    # ),
    p("Einzelne Fächer oder als MINT zusammengefasst:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("subjects_aggregated"),
      choices = c("einzeln", "aggregiert"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon")),
      selected = "aggregiert"
    ),
    conditionalPanel(condition = "input.subjects_aggregated == 'aggregiert'",
                     ns = ns,
    p("Wähle ob MINT oder alle anderen Studienfächer dargestellt werden sollen:"),
    shinyWidgets::pickerInput(
      inputId = ns("topic_studierende_verlauf"),
      choices = c("MINT", "Alle anderen Studienfächer" = "andere Studiengänge"),
      selected = "MINT"
    )),
    conditionalPanel(condition = "input.subjects_aggregated == 'einzeln'",
                     ns = ns,
                     p("Wähle ein Fach:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("subject_selected"),
                       choices = c("Mathematik" = "Mathe", "Ingenieurswesen" = "Ingenieur"),
                       selected = "Mathematik"
                     )),
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
      options = list(`actions-box` = TRUE,
                     `deselect-all-text` = "Keins auswählen",
                     `select-all-text` = "Alle auswählen"),
      selected = c("Hessen", "Hamburg")
    )),
    conditionalPanel(condition = "input.ost_west != false",
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

    observeEvent(input$subject_selected, {
      r$subject_selected <- input$subject_selected
    })

    observeEvent(input$subjects_aggregated, {
      r$subjects_aggregated <- input$subjects_aggregated
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_verlauf_ui("studium_studienzahl_verlauf_1")

## To be copied in the server
# mod_studium_studienzahl_verlauf_server("studium_studienzahl_verlauf_1")
