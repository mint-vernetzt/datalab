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
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_start_comparison_mint"),
      label = NULL,
      choices = c("2013", "2014","2015","2016","2017", "2018", "2019", "2020", "2021", "2022"),
      selected = "2022"
    ),
    br(),
    shinyBS::bsPopover(id="ih_alle_mint_3", title="",
                       content = paste0("Diese Übersicht zeigt beispielsweise, dass deutschlandweit 2021 37 % der Studierenden ein MINT-Fach studieren. Dagegen studieren 63 % in einem anderen Fachbereich. Über alle Bildungsbereiche hinweg ist der MINT-Anteil der Beschäftigten mit 23 % 2021 am geringsten."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_alle_mint_3")
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
