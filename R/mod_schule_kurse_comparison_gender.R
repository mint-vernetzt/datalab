#' schule_kurse_comparison_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_comparison_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitpunkt:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_comparison_gender"),
      label = NULL,
      choices = c("2013","2014","2015","2016","2017", "2018", "2019", "2020"),
      selected = "2020"
    )
  )
}

#' schule_kurse_comparison_gender Server Functions
#'
#' @noRd
mod_schule_kurse_comparison_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse_comparison_gender, {
      r$date_kurse_comparison_gender <- input$date_kurse_comparison_gender
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_comparison_gender_ui("schule_kurse_comparison_gender_1")

## To be copied in the server
# mod_schule_kurse_comparison_gender_server("schule_kurse_comparison_gender_1")
