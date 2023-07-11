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

    p("Auswahl des Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("rank_y"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021"),
      selected = c( "2021")

    ),
    p("Auswahl des Indikators:"),
    shinyWidgets::pickerInput(
      inputId = ns("rank_l"),
      choices = c("Auländische Studienanfänger:innen (1. Hochschulsemester)",
                  "Studienanfänger:innen (1. Fachsemester)",
                  "Studierende",
                  "Ausländische Studierende",
                  "Studienanfänger:innen (1. Hochschulsemester)",
                  "Studierende (Nur Lehramt)"
      ),
      selected = "Studierende"),
    # p("Nur Lehramt azeigen:"),
    # tags$div(
    #   shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studierende_ranking_bl_subject"), label = "Nein", inline = TRUE),
    #   tags$span("Ja"),
    #   p("Auswahl der Hochschulform:"),
    #   conditionalPanel(condition = "input.nurLehramt_studierende_ranking_bl_subject == false",
    #                    ns = ns,
    #                    shinyWidgets::pickerInput(
    #                      inputId = ns("hochschulform_studierende_ranking_bl_1"),
    #                      choices = c("Alle Hochschulen"="insgesamt", "Universität" = "Uni", "Fachhochschule" = "FH")
    #                    )),
    #   conditionalPanel(condition = "input.nurLehramt_studierende_ranking_bl_subject != false",
    #                    ns = ns,
    #                    shinyWidgets::pickerInput(
    #                      inputId = ns("hochschulform_studierende_ranking_bl_2"),
    #                      choices = "Uni"
    #                    ))
    # ),
    # p("Status der Student:innen:"),
    # shinyWidgets::radioGroupButtons(
    #   inputId = ns("topic_selected_subject_bl"),
    #   choices = c("Studienanfänger:innen"="Studienanfänger:innen", "Studierende"),
    #   direction = "vertical",
    #   justified = TRUE,
    #   checkIcon = list(yes = icon("ok",
    #                               lib = "glyphicon"))
    # ),
    p("Auswahl des Bundeslands:"),
    shinyWidgets::pickerInput(
      inputId = ns("rank_states"),
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
    ))

}

#' studium_studienzahl_ranking_bl_subject Server Functions
#'
#' @noRd
mod_studium_studienzahl_ranking_bl_subject_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$rank_l, {
      r$rank_l <- input$rank_l
    })

    observeEvent(input$rank_states, {
      r$rank_states <- input$rank_states
    })

    # observeEvent(input$subject_selected_bl, {
    #   r$subject_selected_bl <- input$subject_selected_bl
    # })

    observeEvent(input$rank_y, {
      r$rank_y <- input$rank_y
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
