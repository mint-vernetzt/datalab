#' ausbildung_vertraege UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ausbildung_vertraege_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        title = "Lorem Ipsum",
        width = 12,
        shiny::sidebarPanel(
          mod_ausbildung_vertraege_multiple_ui("mod_ausbildung_vertraege_multiple_ui_1")),
        shiny::mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Anteil", plotOutput(ns("plot_waffle"))),
                      tabPanel("Absolut", plotOutput(ns("plot_absolut"))),
                      tabPanel("Ranking", plotOutput(ns("plot_ranking"))),
                      tabPanel("Map", highcharter::highchartOutput(ns("plot_map_vertraege"))))

        )
      )
    ),
    fluidRow(
      shinydashboard::box(
        title = "Lorem Ipsum",
        width = 12,
        shiny::sidebarPanel(
          mod_ausbildung_vertraege_verlauf_ui("mod_ausbildung_vertraege_verlauf_ui_1")),
        shiny::mainPanel(plotOutput(ns("plot_verlauf")))
      )
    )
  )
}

#' ausbildung_vertraege Server Functions
#'
#' @noRd
mod_ausbildung_vertraege_server <- function(id, data_ausbildungsvertraege, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot_waffle <- renderPlot({
      ausbildungsvertraege_waffle(data_ausbildungsvertraege, r)
    })

    output$plot_absolut <- renderPlot({
      ausbildungsvertraege_absolut(data_ausbildungsvertraege, r)
    })

    output$plot_map_vertraege <- highcharter::renderHighchart({
      vertraege_map(data_ausbildungsvertraege, r)
    })

    output$plot_ranking <- renderPlot({
      vertraege_ranking(data_ausbildungsvertraege, r)
    })

    output$plot_verlauf <- renderPlot({
      vertraege_verlauf(data_ausbildungsvertraege, r)
    })

  })
}

## To be copied in the UI
# mod_ausbildung_vertraege_ui("ausbildung_vertraege_1")

## To be copied in the server
# mod_ausbildung_vertraege_server("ausbildung_vertraege_1")
