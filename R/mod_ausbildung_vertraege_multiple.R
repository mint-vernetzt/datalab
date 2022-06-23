#' ausbildung_vertraege_multiple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ausbildung_vertraege_multiple_ui <- function(id){
  ns <- NS(id)

  load(file = system.file(package="datalab","data/data_naa.rda"))

  tagList(
    p("Wählen Sie einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_ausbildungsvertraege"),
      label = NULL,
      choices = c(2017, 2020),
    ),
    p("Wählen Sie den Bereich der neuen Auszubildenden:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_ausbildungsvertraege"),
      choices = unique(data_naa$fachbereich),
      selected = "Informatik-Fachkräfte"
    )
  )
}

#' ausbildung_vertraege_multiple Server Functions
#'
#' @noRd
mod_ausbildung_vertraege_multiple_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_ausbildungsvertraege, {
      r$date_ausbildungsvertraege <- input$date_ausbildungsvertraege
    })

    observeEvent(input$indikator_ausbildungsvertraege, {
      r$indikator_ausbildungsvertraege <- input$indikator_ausbildungsvertraege
    })

  })
}

## To be copied in the UI
# mod_ausbildung_vertraege_multiple_ui("ausbildung_vertraege_multiple_1")

## To be copied in the server
# mod_ausbildung_vertraege_multiple_server("ausbildung_vertraege_multiple_1")
