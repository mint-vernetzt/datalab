#' home_start_comparison_mint UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_comparison_mint_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitpunkt:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_start_comparison_mint"),
      label = NULL,
      choices = c("2013", "2014","2015","2016","2017", "2018", "2019", "2020"),
      selected = "2020"
    )
  )
}

#' home_start_comparison_mint Server Functions
#'
#' @noRd
mod_home_start_comparison_mint_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_start_comparison_mint, {
      r$date_start_comparison_mint <- input$date_start_comparison_mint
    })

  })
}

## To be copied in the UI
# mod_home_start_comparison_mint_ui("home_start_comparison_mint_1")

## To be copied in the server
# mod_home_start_comparison_mint_server("home_start_comparison_mint_1")
