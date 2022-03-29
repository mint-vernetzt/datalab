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
    ),
    br(),br(),br(),br(),
    fluidRow(
      shiny::column(width = 12, plotOutput(ns("plot_compare"))
      )
    )
  )
}

#' studium_compare Server Functions
#'
#' @noRd
mod_studium_compare_server <- function(id, data, r, r_abschluss,
                                       r_studienzahl, r_habil){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


        output$plot_compare <- renderPlot({
          comparer_plot(data,r, isolate(r_abschluss),
                        isolate(r_studienzahl), isolate(r_habil))
    })
  })
}

## To be copied in the UI
# mod_studium_compare_ui("studium_compare_1")

## To be copied in the server
# mod_studium_compare_server("studium_compare_1")
