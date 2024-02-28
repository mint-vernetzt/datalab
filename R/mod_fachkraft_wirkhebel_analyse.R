#' fachkraft_wirkhebel_analyse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_wirkhebel_analyse_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("fachkraft_item_wirkhebel_analyse"),
      label = NULL,
      choices = 2023:2037,
      selected = "2023"
    ),
  )
}

#' fachkraft_wirkhebel_analyse Server Functions
#'
#' @noRd
mod_fachkraft_wirkhebel_analyse_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$fachkraft_item_wirkhebel_analyse, {
      r$fachkraft_item_wirkhebel_analyse <- input$fachkraft_item_wirkhebel_analyse
    })

  })
}

## To be copied in the UI
# mod_fachkraft_wirkhebel_analyse_ui("fachkraft_wirkhebel_analyse_1")

## To be copied in the server
# mod_fachkraft_wirkhebel_analyse_server("fachkraft_wirkhebel_analyse_1")

