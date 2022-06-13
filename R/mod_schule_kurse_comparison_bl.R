#' schule_kurse_comparison_bl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_comparison_bl_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_comparison_bl"),
      label = NULL,
      choices = c("2013","2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = "2020"),

    p("Wähle in welcher Form der Kurs belegt wurde:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("indikator_comparison_bl"),
      choices = c("Grundkurse", "Leistungskurse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),

    p("Wähle ein Fach:"),
    shinyWidgets::pickerInput(
      inputId = ns("subject_comparison_bl"),
      choices = c("MINT (aggregiert)","Mathematik", "Informatik", "Physik", "Chemie",
                  "Biologie", "andere Fächer (aggregiert)", "Deutsch", "Fremdsprachen", "Gesellschaftswissenschaften",
                  "Kunst/Gestaltung/Werken", "Ethik/Philosophie", "Religion, ev.", "Religion, kath.",
                  "Sport", "Musik"),
      selected = "MINT (aggregiert)"
    )

  )
}

#' schule_kurse_comparison_bl Server Functions
#'
#' @noRd
mod_schule_kurse_comparison_bl_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_comparison_bl, {
      r$date_comparison_bl <- input$date_comparison_bl
    })

    observeEvent(input$subject_comparison_bl, {
      r$subject_comparison_bl <- input$subject_comparison_bl
    })

    observeEvent(input$indikator_comparison_bl, {
      r$indikator_comparison_bl <- input$indikator_comparison_bl
    })


  })
}

## To be copied in the UI
# mod_schule_kurse_comparison_bl_ui("schule_kurse_comparison_bl_1")

## To be copied in the server
# mod_schule_kurse_comparison_bl_server("schule_kurse_comparison_bl_1")
