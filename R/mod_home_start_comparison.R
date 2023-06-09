#' home_start_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_comparison_ui <- function(id){
  ns <- NS(id)

  tagList(
    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_start_comparison"),
      label = NULL,
      choices = c("2013", "2014","2015", "2016", "2017",
                  "2018","2019", "2020", "2021"),
      selected = c("2015", "2020")
    ),
    p("Bereich:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_start_comparison"),
      choices = c("Schülerinnen Leistungskurse", "Studierende",
                  "Auszubildende", "Beschäftigte"),
      selected = c("Schülerinnen Leistungskurse", "Beschäftigte"),
      # brauchts nicht, gibt nur 4
      # options =  list(
      #   "max-options" = 3,
      #   "max-options-text" = "Bitte nur maximal 3 Bereiche auswählen"
      # ),
      multiple = TRUE

    ),
    p("Betrachtung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_start_comparison"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    )
  )

}

#' home_start_comparison Server Functions
#'
#' @noRd
mod_home_start_comparison_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_start_comparison, {
      r$date_start_comparison <- input$date_start_comparison
    })

    observeEvent(input$indikator_start_comparison, {
      r$indikator_start_comparison <- input$indikator_start_comparison
    })

    observeEvent(input$abs_zahlen_start_comparison, {
      r$abs_zahlen_start_comparison <- input$abs_zahlen_start_comparison
    })


  })
}

## To be copied in the UI
# mod_home_start_comparison_ui("home_start_comparison_1")

## To be copied in the server
# mod_home_start_comparison_server("home_start_comparison_1")
