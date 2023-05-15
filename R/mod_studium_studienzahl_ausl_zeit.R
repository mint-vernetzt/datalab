#' studium_studienzahl_bl_map_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_ausl_zeit_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_studium_studienzahl_ausl_zeit"),
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
      ),selected = "Nordrhein-Westfalen"
    ),
    p("Fach/Fächergruppe:"),
    shinyWidgets::pickerInput(
      inputId = ns("fach_studium_studienzahl_ausl_zeit"),
      choices = c("Geisteswissenschaften","Sport","Rechts-, Wirtschafts- und Sozialwissenschaften","Weitere naturwissenschaftliche und mathematische Fächer","Mathematik","Physik, Astronomie","Chemie","Pharmazie","Biologie","Humanmedizin/Gesundheitswissenschaften","Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin","Weitere ingenieurwissenschaftliche Fächer","Maschinenbau/Verfahrenstechnik","Elektrotechnik und Informationstechnik","Verkehrstechnik, Nautik","Architektur, Innenarchitektur","Raumplanung","Bauingenieurwesen","Vermessungswesen","Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt","Informatik","Materialwissenschaft und Werkstofftechnik","Kunst, Kunstwissenschaft","Außerhalb der Studienbereichsgliederung/Sonstige Fächer","Geowissenschaften und Geographie","Naturwissenschaften","Ingenieurwissenschaften ohne Informatik","MINT","Nicht MINT"
      ),selected = "MINT"
    ),
    p("Status der Studierenden:"),
    shinyWidgets::pickerInput(
      inputId = ns("status_ausl_zeit"),
      choices = c("Studierende",
                  "Studienanfänger:innen (1. Hochschulsemester)"
      ),
      selected = "Studierende"
    ),
    p("Betrachtung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_studium_studienzahl_ausl_zeit"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))

    )
  )
}

#' studium_studienzahl_bl_map_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_ausl_zeit_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$states_studium_studienzahl_ausl_zeit, {
      r$states_studium_studienzahl_ausl_zeit <- input$states_studium_studienzahl_ausl_zeit
    })

    observeEvent(input$status_ausl_zeit, {
      r$status_ausl_zeit <- input$status_ausl_zeit
    })

    observeEvent(input$fach_studium_studienzahl_ausl_zeit, {
      r$fach_studium_studienzahl_ausl_zeit <- input$fach_studium_studienzahl_ausl_zeit
    })

    observeEvent(input$abs_zahlen_studium_studienzahl_ausl_zeit, {
      r$abs_zahlen_studium_studienzahl_ausl_zeit <- input$abs_zahlen_studium_studienzahl_ausl_zeit
    })

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
