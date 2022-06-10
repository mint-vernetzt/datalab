#' studium_studienzahl_verlauf_bl_subject UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_verlauf_bl_subject_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_verlauf_subject_bl"),
      label = NULL,
      choices = c("2010","2011", "2012", "2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = c("2015", "2020")
    ),
    p("Soll nur Lehramt angezeigt werden?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studierende_verlauf_bl_subject"), label = "Nein", inline = TRUE),
      tags$span("Ja"),
      p("Wähle eine Hochschulform:"),
      conditionalPanel(condition = "input.nurLehramt_studierende_verlauf_bl_subject == false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studierende_verlauf_1"),
                         choices = c("insgesamt", "Uni", "FH")
                       )),
      conditionalPanel(condition = "input.nurLehramt_studierende_verlauf_bl_subject != false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studierende_verlauf_2"),
                         choices = "Uni"
                       ))
    ),
    p("Wähle den Status der Student*innen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("topic_selected_subject_bl"),
      choices = c("Studierende", "Studienanfänger"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Wähle ein oder mehrere Fächer:"),
    shinyWidgets::pickerInput(
      inputId = ns("subject_selected_bl"),
      choices = c("Mathematik" = "Mathe", "Ingenieurswesen" = "Ingenieur",
                  "MINT aggregiert" = "MINT"),
      selected = c("MINT (aggregiert)", "Ingenieur"),
      multiple = TRUE
    ),
    p("Wähle ein Bundesland:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_verlauf_subject_bl"),
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
                  "Westen",
                  "Osten"),
      selected = "Hessen"
    )
  )
}

#' studium_studienzahl_verlauf_bl_subject Server Functions
#'
#' @noRd
mod_studium_studienzahl_verlauf_bl_subject_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_verlauf_subject_bl, {
      r$date_verlauf_subject_bl <- input$date_verlauf_subject_bl
    })

    observeEvent(input$topic_selected_subject_bl, {
      r$topic_selected_subject_bl <- input$topic_selected_subject_bl
    })

    observeEvent(input$subject_selected_bl, {
      r$subject_selected_bl <- input$subject_selected_bl
    })

    observeEvent(input$states_verlauf_subject_bl, {
      r$states_verlauf_subject_bl <- input$states_verlauf_subject_bl
    })

    observeEvent(input$nurLehramt_studierende_verlauf_bl_subject, {
      r$nurLehramt_studierende_verlauf_bl_subject <- input$nurLehramt_studierende_verlauf_bl_subject
    })

    observeEvent(input$hochschulform_studierende_verlauf_1, {
      r$hochschulform_studierende_verlauf_1 <- input$hochschulform_studierende_verlauf_1
    })

    observeEvent(input$hochschulform_studierende_verlauf_2, {
      r$hochschulform_studierende_verlauf_2 <- input$hochschulform_studierende_verlauf_2
    })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_verlauf_bl_subject_ui("studium_studienzahl_verlauf_bl_subject_1")

## To be copied in the server
# mod_studium_studienzahl_verlauf_bl_subject_server("studium_studienzahl_verlauf_bl_subject_1")
