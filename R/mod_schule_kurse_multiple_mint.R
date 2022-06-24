#' schule_kurse_multiple_mint UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_multiple_mint_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Wählen Sie ein Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_mint"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    )

  )
}

#' schule_kurse_multiple_mint Server Functions
#'
#' @noRd
mod_schule_kurse_multiple_mint_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse_mint, {
      r$date_kurse_mint <- input$date_kurse_mint
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_multiple_mint_ui("schule_kurse_multiple_mint_1")

## To be copied in the server
# mod_schule_kurse_multiple_mint_server("schule_kurse_multiple_mint_1")
