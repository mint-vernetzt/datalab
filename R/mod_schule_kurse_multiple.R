#' schule_kurse_multiple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_multiple_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    ),
    p("Wähle in welcher Form der Kurs belegt wurde:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("indikator_kurse_gender"),
      choices = c("Grundkurse", "Leistungskurse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    )
  )
}

#' schule_kurse_multiple Server Functions
#'
#' @noRd
mod_schule_kurse_multiple_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse, {
      r$date_kurse <- input$date_kurse
    })

    observeEvent(input$indikator_kurse_gender, {
      r$indikator_kurse_gender <- input$indikator_kurse_gender
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_multiple_ui("schule_kurse_multiple_1")

## To be copied in the server
# mod_schule_kurse_multiple_server("schule_kurse_multiple_1")
