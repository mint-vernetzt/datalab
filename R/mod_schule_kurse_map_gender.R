#' schule_kurse_map_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_map_gender_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_map_gender"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    ),
    p("Wähle ein Fach:"),
    shinyWidgets::pickerInput(
      inputId = ns("subject_map_gender"),
      choices = c("MINT (aggregiert)","Mathematik", "Informatik", "Physik", "Chemie",
                  "Biologie", "andere Fächer (aggregiert)", "Deutsch", "Fremdsprachen", "Gesellschaftswissenschaften",
                  "Kunst/Gestaltung/Werken", "Ethik/Philosophie", "Religion, ev.", "Religion, kath.",
                  "Sport", "Musik"),
      selected = "MINT (aggregiert)"
    ),

    p("Wähle in welcher Form der Kurs belegt wurde:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("indikator_map_gender"),
      choices = c("Grundkurse", "Leistungskurse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    )

  )
}

#' schule_kurse_map_gender Server Functions
#'
#' @noRd
mod_schule_kurse_map_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_map_gender, {
      r$date_map_gender <- input$date_map_gender
    })

    observeEvent(input$subject_map_gender, {
      r$subject_map_gender <- input$subject_map_gender
    })

    observeEvent(input$indikator_map_gender, {
      r$indikator_map_gender <- input$indikator_map_gender
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_map_gender_ui("schule_kurse_map_gender_1")

## To be copied in the server
# mod_schule_kurse_map_gender_server("schule_kurse_map_gender_1")
