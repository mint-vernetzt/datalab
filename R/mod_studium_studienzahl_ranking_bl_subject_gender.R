#' studium_studienzahl_ranking_bl_subject_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_ranking_bl_subject_gender_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Wählen Sie ein Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studium_ranking_bl_subject_gender"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    ),
    p("Soll nur Lehramt angezeigt werden?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studium_ranking_bl_subject_gender"), label = "Nein", inline = TRUE),
      tags$span("Ja"),
      p("Wählen Sie eine Hochschulform:"),
      conditionalPanel(condition = "input.nurLehramt_studium_ranking_bl_subject_gender == false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studium_ranking_bl_subject_gender_1"),
                         choices = c("insgesamt", "Uni", "FH")
                       )),
      conditionalPanel(condition = "input.nurLehramt_studium_ranking_bl_subject_gender != false",
                       ns = ns,
                       shinyWidgets::pickerInput(
                         inputId = ns("hochschulform_studium_ranking_bl_subject_gender_2"),
                         choices = "Uni"
                       ))
    ),
    p("Auswahl Bundesland oder Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_studium_ranking_bl_subject_gender"),
      choices = list(
        Regionen = c("Deutschland", "Westen", "Osten"),
        Bundesländer = c( "Bayern",
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
                          "Thüringen")),
      selected = "Hessen"
    )
  )
}

#' studium_studienzahl_ranking_bl_subject_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_ranking_bl_subject_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_studium_ranking_bl_subject_gender, {
      r$date_studium_ranking_bl_subject_gender <- input$date_studium_ranking_bl_subject_gender
    })

    observeEvent(input$nurLehramt_studium_ranking_bl_subject_gender, {
      r$nurLehramt_studium_ranking_bl_subject_gender <- input$nurLehramt_studium_ranking_bl_subject_gender
    })

    observeEvent(input$hochschulform_studium_ranking_bl_subject_gender_1, {
      r$hochschulform_studium_ranking_bl_subject_gender_1 <- input$hochschulform_studium_ranking_bl_subject_gender_1
    })

    observeEvent(input$hochschulform_studium_ranking_bl_subject_gender_2, {
      r$hochschulform_studium_ranking_bl_subject_gender_2 <- input$hochschulform_studium_ranking_bl_subject_gender_2
    })

    observeEvent(input$states_studium_ranking_bl_subject_gender, {
      r$states_studium_ranking_bl_subject_gender <- input$states_studium_ranking_bl_subject_gender
    })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_ranking_bl_subject_gender_ui("studium_studienzahl_ranking_bl_subject_gender_1")

## To be copied in the server
# mod_studium_studienzahl_ranking_bl_subject_gender_server("studium_studienzahl_ranking_bl_subject_gender_1")
