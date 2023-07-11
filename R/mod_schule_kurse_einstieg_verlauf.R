#' schule_kurse_einstieg_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_einstieg_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_einstieg_verlauf"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021"),
      selected = c("2016", "2021")
    ),
    p("Betrachtung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_kurse_einstieg_verlauf"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    )
  )


}

#' schule_kurse_einstieg_verlauf Server Functions
#'
#' @noRd
mod_schule_kurse_einstieg_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse_einstieg_verlauf, {
      r$date_kurse_einstieg_verlauf <- input$date_kurse_einstieg_verlauf
    })

    observeEvent(input$abs_zahlen_kurse_einstieg_verlauf, {
      r$abs_zahlen_kurse_einstieg_verlauf <- input$abs_zahlen_kurse_einstieg_verlauf
    })



  })
}

## To be copied in the UI
# mod_schule_kurse_einstieg_verlauf_ui("schule_kurse_einstieg_verlauf_1")

## To be copied in the server
# mod_schule_kurse_einstieg_verlauf_server("schule_kurse_einstieg_verlauf_1")
