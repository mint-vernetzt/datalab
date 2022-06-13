#' studium_studienzahl_bl_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_bl_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studium_studienzahl_bl_verlauf"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = c("2015", "2020")
    ),
    p("Soll nur Lehramt angezeigt werden?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studium_studienzahl_bl_verlauf"), label = "Nein", inline = TRUE),
      tags$span("Ja"),
      p("Wähle eine Hochschulform:"),
      conditionalPanel(condition = "input.nurLehramt_studium_studienzahl_bl_verlauf == false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studium_studienzahl_bl_verlauf1"),
                         choices = c("insgesamt", "Uni", "FH")
                       )),
      conditionalPanel(condition = "input.nurLehramt_studium_studienzahl_bl_verlauf != false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studium_studienzahl_bl_verlauf2"),
                         choices = "Uni"
                       )),
      p("Wähle ein Fach:"),
      shinyWidgets::pickerInput(
        inputId = ns("subject_studium_studienzahl_bl_verlauf"),
        choices = c("MINT (aggregiert)","Mathe", "Ingenieur"),
        selected = "MINT (aggregiert)"
      )
    ),
    p("Wähle den Status der Student*innen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("level_studium_studienzahl_bl_verlauf"),
      choices = c("Studierende", "Studienanfänger"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Wähle ein oder mehrere Bundesländer:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_studium_studienzahl_bl_verlauf"),
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
      selected = c("Hessen", "Berlin"),
      options = list(`actions-box` = TRUE,
                     `deselect-all-text` = "Alle abwählen",
                     `select-all-text` = "Alle auswählen"),
      multiple = TRUE
    )

  )
}

#' studium_studienzahl_bl_verlauf Server Functions
#'
#' @noRd
mod_studium_studienzahl_bl_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_studium_studienzahl_bl_verlauf, {
      r$date_studium_studienzahl_bl_verlauf <- input$date_studium_studienzahl_bl_verlauf
    })

    observeEvent(input$nurLehramt_studium_studienzahl_bl_verlauf, {
      r$nurLehramt_studium_studienzahl_bl_verlauf <- input$nurLehramt_studium_studienzahl_bl_verlauf
    })

    observeEvent(input$hochschulform_studium_studienzahl_bl_verlauf1, {
      r$hochschulform_studium_studienzahl_bl_verlauf1 <- input$hochschulform_studium_studienzahl_bl_verlauf1
    })

    observeEvent(input$hochschulform_studium_studienzahl_bl_verlauf2, {
      r$hochschulform_studium_studienzahl_bl_verlauf2 <- input$hochschulform_studium_studienzahl_bl_verlauf2
    })

    observeEvent(input$subject_studium_studienzahl_bl_verlauf, {
      r$subject_studium_studienzahl_bl_verlauf <- input$subject_studium_studienzahl_bl_verlauf
    })

    observeEvent(input$level_studium_studienzahl_bl_verlauf, {
      r$level_studium_studienzahl_bl_verlauf <- input$level_studium_studienzahl_bl_verlauf
    })

    observeEvent(input$states_studium_studienzahl_bl_verlauf, {
      r$states_studium_studienzahl_bl_verlauf <- input$states_studium_studienzahl_bl_verlauf
    })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_bl_verlauf_ui("studium_studienzahl_bl_verlauf_1")

## To be copied in the server
# mod_studium_studienzahl_bl_verlauf_server("studium_studienzahl_bl_verlauf_1")
