#' schule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(width = 5,
               h2("Welcome!")),
        br(),br(),br()
      ),
      fluidRow(
        column(width = 10,
               shinydashboard::box(
                 background = "blue",
                 h2("Hier werden nur Schuldaten angezeigt!"),
                 title = "Congrats!"),
               br(), br(), br(), br()
        )),
      fluidRow(
        shinydashboard::box(
          title = "Data Table Schule",
          width = 6,
          DT::dataTableOutput(ns('data_table'))),
        shinydashboard::box(
          title = "Line-Plot Schule",
          width = 6,
          plotOutput(ns("plot"))
        )
      )
    )
  )
}

#' schule Server Functions
#'
#' @noRd
mod_schule_server <- function(id, filter_name){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$data_table <- DT::renderDT({
      utils::head(data %>% dplyr::filter(.data$bereich == filter_name))
    })
  })
}

## To be copied in the UI
# mod_schule_ui("schule_1")

## To be copied in the server
# mod_schule_server("schule_1")
