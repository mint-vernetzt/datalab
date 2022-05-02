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
    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_start_comparison"),
      label = NULL,
      choices = c("2015", "2016", "2017",
                  "2018","2019"),
      selected = c("2015", "2019")
    ),
    p("Wähle ein odere mehere Indikatoren:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_start_comparison"),
      choices = c("Beschäftigte", "Auszubildende", "Habilitationen", "Leistungskurse",
      "Promotionen (angestrebt)", "Studienanfänger", "Studierende"),
      selected = c("Studierende", "Beschäftigte"),
      multiple = TRUE

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


  })
}

## To be copied in the UI
# mod_home_start_comparison_ui("home_start_comparison_1")

## To be copied in the server
# mod_home_start_comparison_server("home_start_comparison_1")
