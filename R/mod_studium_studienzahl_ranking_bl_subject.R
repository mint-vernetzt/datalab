#' studium_studienzahl_ranking_bl_subject UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_ranking_bl_subject_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl des Zeitraums:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_ranking_subject_bl"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    ),
    p("Nur Lehramt azeigen:"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studierende_ranking_bl_subject"), label = "Nein", inline = TRUE),
      tags$span("Ja"),
      p("Auswahl der Hochschulform:"),
      conditionalPanel(condition = "input.nurLehramt_studierende_ranking_bl_subject == false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studierende_ranking_bl_1"),
                         choices = c("Alle Hochschulen"="insgesamt", "Universität" = "Uni", "Fachhochschule" = "FH")
                       )),
      conditionalPanel(condition = "input.nurLehramt_studierende_ranking_bl_subject != false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studierende_ranking_bl_2"),
                         choices = "Uni"
                       ))
    ),
    p("Status der Student:innen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("topic_selected_subject_bl"),
      choices = c("Studienanfänger:innen"="Studienanfänger:innen", "Studierende"),
      direction = "vertical",
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Auswahl des Bundeslands:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_ranking_subject_bl"),
      choices = c("Deutschland",
                  "Baden-Württemberg",
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
                  "Thüringen"
                  ,
                  "Westen",
                  "Osten"
                  ),
      selected = "Hessen"
    )
  )
}

#' studium_studienzahl_ranking_bl_subject Server Functions
#'
#' @noRd
mod_studium_studienzahl_ranking_bl_subject_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_ranking_subject_bl, {
      r$date_ranking_subject_bl <- input$date_ranking_subject_bl
    })

    observeEvent(input$topic_selected_subject_bl, {
      r$topic_selected_subject_bl <- input$topic_selected_subject_bl
    })

    # observeEvent(input$subject_selected_bl, {
    #   r$subject_selected_bl <- input$subject_selected_bl
    # })

    observeEvent(input$states_ranking_subject_bl, {
      r$states_ranking_subject_bl <- input$states_ranking_subject_bl
    })

    observeEvent(input$nurLehramt_studierende_ranking_bl_subject, {
      r$nurLehramt_studierende_ranking_bl_subject <- input$nurLehramt_studierende_ranking_bl_subject
    })

    observeEvent(input$hochschulform_studierende_ranking_bl_1, {
      r$hochschulform_studierende_ranking_bl_1 <- input$hochschulform_studierende_ranking_bl_1
    })

    observeEvent(input$hochschulform_studierende_ranking_bl_2, {
      r$hochschulform_studierende_ranking_bl_2 <- input$hochschulform_studierende_ranking_bl_2
    })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_ranking_bl_subject_ui("studium_studienzahl_ranking_bl_subject_1")

## To be copied in the server
# mod_studium_studienzahl_ranking_bl_subject_server("studium_studienzahl_ranking_bl_subject_1")
