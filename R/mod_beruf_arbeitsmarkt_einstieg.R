#' beruf_arbeitsmarkt_einstieg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_einstieg_ui <- function(id){
  ns <- NS(id)

  tagList(
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_einstieg"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021"),
      selected = "2021",
      dragRange = TRUE
    )
  )
}

#' beruf_arbeitsmarkt_einstieg Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_einstieg_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_einstieg, {
      r$date_arbeitsmarkt_einstieg <- input$date_arbeitsmarkt_einstieg
    })

  })

}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_einstieg_ui("beruf_arbeitsmarkt_einstieg_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_einstieg_server("beruf_arbeitsmarkt_einstieg_1")
