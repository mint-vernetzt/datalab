#' studium_studienzahl_verlauf_bl_subject_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_verlauf_bl_subject_gender_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_verlauf_bl_subject_gender"),
      label = NULL,
      choices = c("2010","2011", "2012", "2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = c("2015", "2020")
    ),
    p("Soll nur Lehramt angezeigt werden?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studierende_verlauf_bl_subject_gender"), label = "Nein", inline = TRUE),
      tags$span("Ja"),
      p("Wähle eine Hochschulform:"),
      conditionalPanel(condition = "input.nurLehramt_studierende_verlauf_bl_subject_gender == false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studierende_verlauf_bl_subject_gender_1"),
                         choices = c("insgesamt", "Uni", "FH")
                       )),
      conditionalPanel(condition = "input.nurLehramt_studierende_verlauf_bl_subject_gender != false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studierende_verlauf_bl_subject_gender_2"),
                         choices = "Uni"
                       ))
    ),
    # p("Wähle in welcher Form der Kurs belegt wurde:"),
    # shinyWidgets::radioGroupButtons(
    #   inputId = ns("level_studierende_verlauf_bl_subject_gender"),
    #   choices = c("Studierende", "Studienanfänger"),
    #   justified = TRUE,
    #   checkIcon = list(yes = icon("ok",
    #                               lib = "glyphicon"))
    # ),
    p("Wähle ein Fach:"),
    shinyWidgets::pickerInput(
      inputId = ns("subject_verlauf_bl_subject_gender"),
      choices = c("Mathematik" = "Mathe", "Ingenieurswesen" = "Ingenieur",
                  "MINT aggregiert" = "MINT"),
      selected = "Mathe",
      multiple = FALSE
    ),
    p("Wähle ein Bundesland:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_verlauf_bl_subject_gender"),
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

#' studium_studienzahl_verlauf_bl_subject_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_verlauf_bl_subject_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_verlauf_bl_subject_gender, {
      r$date_verlauf_bl_subject_gender <- input$date_verlauf_bl_subject_gender
    })

    observeEvent(input$nurLehramt_studierende_verlauf_bl_subject_gender, {
      r$nurLehramt_studierende_verlauf_bl_subject_gender <- input$nurLehramt_studierende_verlauf_bl_subject_gender
    })

    observeEvent(input$hochschulform_studierende_verlauf_bl_subject_gender_1, {
      r$hochschulform_studierende_verlauf_bl_subject_gender_1 <- input$hochschulform_studierende_verlauf_bl_subject_gender_1
    })

    observeEvent(input$hochschulform_studierende_verlauf_bl_subject_gender_2, {
      r$hochschulform_studierende_verlauf_bl_subject_gender_2 <- input$hochschulform_studierende_verlauf_bl_subject_gender_2
    })

    observeEvent(input$subject_verlauf_bl_subject_gender, {
      r$subject_verlauf_bl_subject_gender <- input$subject_verlauf_bl_subject_gender
    })

    observeEvent(input$states_verlauf_bl_subject_gender, {
      r$states_verlauf_bl_subject_gender <- input$states_verlauf_bl_subject_gender
    })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_verlauf_bl_subject_gender_ui("studium_studienzahl_verlauf_bl_subject_gender_1")

## To be copied in the server
# mod_studium_studienzahl_verlauf_bl_subject_gender_server("studium_studienzahl_verlauf_bl_subject_gender_1")
