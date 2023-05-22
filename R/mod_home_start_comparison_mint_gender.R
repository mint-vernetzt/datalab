#' home_start_comparison_mint_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_comparison_mint_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_start_comparison_mint_gender"),
      label = NULL,
      choices = c("2013", "2014","2015","2016","2017", "2018", "2019", "2020", "2021"),
      selected = "2020"
    )
  )
}

#' home_start_comparison_mint_gender Server Functions
#'
#' @noRd
mod_home_start_comparison_mint_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_start_comparison_mint_gender, {
      r$date_start_comparison_mint_gender <- input$date_start_comparison_mint_gender
    })

  })
}

## To be copied in the UI
# mod_home_start_comparison_mint_gender_ui("home_start_comparison_mint_gender_1")

## To be copied in the server
# mod_home_start_comparison_mint_gender_server("home_start_comparison_mint_gender_1")
