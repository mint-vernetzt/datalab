#' home_start_multiple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_multiple_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Zeitraums:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_start_multiple"),
      label = NULL,
      choices = c("2013", "2014","2015", "2016", "2017",
                  "2018","2019", "2020", "2021"),
      selected = c("2015", "2021")
    ),
    p("Auswahl eines oder mehrerer Bereiche:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_start_multiple_1"),
      choices = c("Schüler:innen Leistungskurse", "Studierende",
                  "Auszubildende", "Beschäftigte"),
      selected = c("Schüler:innen Leistungskurse", "Beschäftigte"),
      # options =  list(
      #   "max-options" = 3,
      #   "max-options-text" = "Bitte nur maximal 3 Bereiche auswählen"
      # ),
      multiple = T
    ),
    p("Betrachtung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_start_multiple"),
      choices = c("Relativ", "Absolut"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    )
  )


}

#' home_start_multiple Server Functions
#'
#' @noRd
mod_home_start_multiple_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_start_multiple, {
      r$date_start_multiple <- input$date_start_multiple
    })

    observeEvent(input$abs_zahlen_start_multiple, {
      r$abs_zahlen_start_multiple <- input$abs_zahlen_start_multiple
    })

    observeEvent(input$indikator_start_multiple_1, {
      r$indikator_start_multiple_1 <- input$indikator_start_multiple_1
    })

  })
}

## To be copied in the UI
# mod_home_start_multiple_ui("home_start_multiple_1")

## To be copied in the server
# mod_home_start_multiple_server("home_start_multiple_1")
