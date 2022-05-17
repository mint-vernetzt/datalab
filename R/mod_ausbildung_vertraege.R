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
  tagList(    fluidRow(
    shinydashboard::box(
      title = "Box 1",
      width = 12,
      p(style = "text-align: justify; font-size = 16px",
        "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor
        invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua"),
      br(),
      p(style = "text-align: justify; font-size = 16px",
        span("17%", style = "color:#b16fab; font-size: 50px"),
        "Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
        sed diam nonumy eirmod tempor invidunt ut labore et dolore magna
        aliquyam erat, sed diam voluptua."),
      br(),
      p(style = "text-align: justify; font-size = 16px",
        span("38%", style = "color:#f5adac; font-size: 50px"),
        "Lorem ipsum dolor sit amet, consetetur sadipscing elitr,
        sed diam nonumy eirmod tempor invidunt ut labore et dolore magna
        aliquyam erat, sed diam voluptua.")
    )),
    fluidRow(
      shinydashboard::box(
        title = "Box 2",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        shiny::sidebarPanel(
          mod_ausbildung_vertraege_multiple_ui("mod_ausbildung_vertraege_multiple_ui_1")),
        shiny::mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Anteil", br(), plotOutput(ns("plot_waffle"))),
                      tabPanel("Absolut", br(), plotOutput(ns("plot_absolut"))),
                      tabPanel("Ranking", br(), plotOutput(ns("plot_ranking"))),
                      tabPanel("Karte", br(), highcharter::highchartOutput(ns("plot_map_vertraege"))),
                      tabPanel("Datensatz", div(DT::dataTableOutput(ns("data_table_mix")),
                                                style = "font-size: 75%; width: 75%")))

        )
      )
    ),
    fluidRow(
      shinydashboard::box(
        title = "Box 3",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        shiny::sidebarPanel(
          mod_ausbildung_vertraege_verlauf_ui("mod_ausbildung_vertraege_verlauf_ui_1")),
        shiny::mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Ranking", br(), plotOutput(ns("plot_verlauf"))),
                      tabPanel("Datensatz", div(DT::dataTableOutput(ns("data_table_verlauf")),
                                                style = "font-size: 75%; width: 75%"))
                      ))
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

    output$data_table_mix <- DT::renderDT({
      data_mix_vertraege(data_ausbildungsvertraege, r)
    })

    output$data_table_verlauf <- DT::renderDT({
      data_verlauf_vertraege(data_ausbildungsvertraege, r)
    })

  })
}

## To be copied in the UI
# mod_ausbildung_vertraege_ui("ausbildung_vertraege_1")

## To be copied in the server
# mod_ausbildung_vertraege_server("ausbildung_vertraege_1")
