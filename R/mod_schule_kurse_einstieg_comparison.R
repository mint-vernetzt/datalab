#' schule_kurse_einstieg_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_einstieg_comparison_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitpunkt:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_einstieg_comparison"),
      label = NULL,
      choices = c("2015","2016","2017", "2018", "2019", "2020"),
      selected = "2020"
    )
  )
}

#' schule_kurse_einstieg_comparison Server Functions
#'
#' @noRd
mod_schule_kurse_einstieg_comparison_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse_einstieg_comparison, {
      r$date_kurse_einstieg_comparison <- input$date_kurse_einstieg_comparison
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_einstieg_comparison_ui("schule_kurse_einstieg_comparison_1")

## To be copied in the server
# mod_schule_kurse_einstieg_comparison_server("schule_kurse_einstieg_comparison_1")