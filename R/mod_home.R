#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
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
                 h1("H1"),
                 h2("H2"),
                 h3("H3"),
                 h4("H4"),
                 title = "Congrats!"),
               br(), br(), br(), br()
        )),
      fluidRow(
        shinydashboard::box(
          title = "New Data Table",
          width = 12,
          DT::dataTableOutput(ns('new_table')))),

      fluidRow(
        shinydashboard::box(
          title = "Data Table",
          width = 6,
          DT::dataTableOutput(ns('data_table'))),
        shinydashboard::box(
          title = "Line-Plot",
          width = 6,
          plotOutput(ns("plot"))
        ),
        shinydashboard::box(
          title = "Text-Box",
          width = 6,
          p("Hier steht Text aus der UI"),
          em("Das ist Italic"),
          textOutput(ns("text"))
        )

      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, data, data_new){
  moduleServer( id, function(input, output, session){
  ns <- session$ns

  output$text <- renderPrint({
    "... und hier aus'm Server"
  })

  output$data_table <- DT::renderDT({
    utils::head(data)
  })

  output$new_table <- DT::renderDT({
    utils::head(data_new)
  })

  output$plot <- renderPlot({
    shinipsum::random_ggplot(type = "line")
  })
  })
}


## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")

