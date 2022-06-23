#' schule_kurse_pie_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_pie_gender_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Wählen Sie einen Zeitpunkt:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_pie_gender"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = "2020")
  )
}

#' schule_kurse_pie_gender Server Functions
#'
#' @noRd
mod_schule_kurse_pie_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse_pie_gender, {
      r$date_kurse_pie_gender <- input$date_kurse_pie_gender
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_pie_gender_ui("schule_kurse_pie_gender_1")

## To be copied in the server
# mod_schule_kurse_pie_gender_server("schule_kurse_pie_gender_1")
