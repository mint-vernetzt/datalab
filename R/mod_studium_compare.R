#' studium_compare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_compare_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
    mod_studium_compare_choice_ui("mod_studium_compare_choice_ui_1"),
    #shiny::submitButton("Update View", icon("refresh"))
    ),
    fluidRow(
      shiny::column(width = 12, plotOutput(ns("plot_compare"))
      )
    )
  )
}

#' studium_compare Server Functions
#'
#' @noRd
mod_studium_compare_server <- function(id, data, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot_compare <- renderPlot({
      comparer_plot(data,r)

    })
  })
}

## To be copied in the UI
# mod_studium_compare_ui("studium_compare_1")

## To be copied in the server
# mod_studium_compare_server("studium_compare_1")
