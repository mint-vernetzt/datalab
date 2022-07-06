#' schule_kurse_comparison_subjects UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_comparison_subjects_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_comparison_subject"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = "2020"
    ),
    p("Auswahl Leistungskurs oder Grundkurs:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("indikator_comparison_subject"),
      choices = c("Grundkurse", "Leistungskurse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Auswahl Bundesland:"),
    shinyWidgets::pickerInput(
      inputId = ns("state_comparison_subject"),
      choices = c("Deutschland",
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
                  "Westen",
                  "Osten"),
      selected = "Hessen"
    )

  )
}

#' schule_kurse_comparison_subjects Server Functions
#'
#' @noRd
mod_schule_kurse_comparison_subjects_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_comparison_subject, {
      r$date_comparison_subject <- input$date_comparison_subject
    })

    observeEvent(input$indikator_comparison_subject, {
      r$indikator_comparison_subject <- input$indikator_comparison_subject
    })

    observeEvent(input$state_comparison_subject, {
      r$state_comparison_subject <- input$state_comparison_subject
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_comparison_subjects_ui("schule_kurse_comparison_subjects_1")

## To be copied in the server
# mod_schule_kurse_comparison_subjects_server("schule_kurse_comparison_subjects_1")
