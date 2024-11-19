#' studium_studienzahl_bl_map_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_ausl_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studium_studienzahl_ausl"),
      label = NULL,
      choices = 2013:2023,
      selected = 2023
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_studium_studienzahl_ausl"),
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
                  "Thüringen",
                  "Westdeutschland (o. Berlin)",
                  "Ostdeutschland (inkl. Berlin)"
      ),selected = "Deutschland"
    ),
    p("Studierendengruppe:"),
    shinyWidgets::pickerInput(
      inputId = ns("status_ausl"),
      choices = c("Studierende",
                  "Studienanfänger:innen (1. Hochschulsemester)",
                  "Absolvent:innen"
      ),
      selected = "Studierende"
    ),
    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_studium_studienzahl_ausl"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Betrachtungsebene:"),
    shinyWidgets::pickerInput(
      inputId = ns("ebene_ausl"),
      choices = c("MINT-Fächer",
                  "Fachbereiche"
      ),
      selected = "Fachbereiche"
    ),
    br(),
    shinyBS::bsPopover(id="ih_studium_international_1", title="",
                       content = paste0("Diese Grafik zeigt z. B., dass im Jahr 2020 in Ostdeutschland ca. 39 % der Elektortechnik- und Informationstechnik-Studierenden internationale Studierende waren. Wählt man als Betrachtung die Anzahl der Studierenden, sieht man das diese 39 % 5.391 internationale Studierende in Elektrotechnik und Informationstechnik ausmachen."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_international_1")
  )
}

#' studium_studienzahl_bl_map_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_ausl_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_studium_studienzahl_ausl, {
      r$date_studium_studienzahl_ausl <- input$date_studium_studienzahl_ausl
    })

    observeEvent(input$status_ausl, {
      r$status_ausl <- input$status_ausl
    })

    observeEvent(input$states_studium_studienzahl_ausl, {
      r$states_studium_studienzahl_ausl <- input$states_studium_studienzahl_ausl
    })

    observeEvent(input$abs_zahlen_studium_studienzahl_ausl, {
      r$abs_zahlen_studium_studienzahl_ausl <- input$abs_zahlen_studium_studienzahl_ausl
    })

    observeEvent(input$ebene_ausl, {
      r$ebene_ausl <- input$ebene_ausl
    })

    # observeEvent(input$abs_zahlen_studium_studienzahl_ausl, {
    #   r$abs_zahlen_studium_studienzahl_ausl <- input$abs_zahlen_studium_studienzahl_ausl
    # })

    # observeEvent(input$status_ausl, {
    #   r$status_ausl <- input$status_ausl
    # })

    # observeEvent(input$subject_studium_studienzahl_bl_gender_map, {
    #   r$subject_studium_studienzahl_bl_gender_map <- input$subject_studium_studienzahl_bl_gender_map
    # })
    #
    # observeEvent(input$level_studium_choice_gender, {
    #   r$level_studium_choice_gender <- input$level_studium_choice_gender
    # })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_bl_map_gender_ui("studium_studienzahl_bl_map_gender_1")

## To be copied in the server
# mod_studium_studienzahl_bl_map_gender_server("studium_studienzahl_bl_map_gender_1")
