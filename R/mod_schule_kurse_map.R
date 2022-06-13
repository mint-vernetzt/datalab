#' schule_kurse_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_map"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    ),
    p("Wähle ein Fach:"),
    shinyWidgets::pickerInput(
      inputId = ns("subject_map"),
      choices = c("MINT (aggregiert)","Mathematik", "Informatik", "Physik", "Chemie",
                  "Biologie", "andere Fächer (aggregiert)", "Deutsch", "Fremdsprachen", "Gesellschaftswissenschaften",
                  "Kunst/Gestaltung/Werken", "Ethik/Philosophie", "Religion, ev.", "Religion, kath.",
                  "Sport", "Musik"),
      selected = "MINT (aggregiert)"
    )
  )
}

#' schule_kurse_map Server Functions
#'
#' @noRd
mod_schule_kurse_map_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_map, {
      r$date_map <- input$date_map
    })

    observeEvent(input$subject_map, {
      r$subject_map <- input$subject_map
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_map_ui("schule_kurse_map_1")

## To be copied in the server
# mod_schule_kurse_map_server("schule_kurse_map_1")
