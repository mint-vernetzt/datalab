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
        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #666666;}
                             .butt{border-color:#FFFFFF}")),
        shiny::sidebarPanel(
          mod_ausbildung_vertraege_multiple_ui("mod_ausbildung_vertraege_multiple_ui_1")),
        shiny::mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Anteil", br(), plotOutput(ns("plot_waffle")),
                               shiny::downloadButton(ns("download_waffle"), label = "",
                                                     class = "butt",
                                                     icon = shiny::icon("download"))),
                      tabPanel("Absolut", br(), plotOutput(ns("plot_absolut")),
                               shiny::downloadButton(ns("download_absolut"), label = "",
                                                     class = "butt",
                                                     icon = shiny::icon("download"))),
                      tabPanel("Ranking", br(), plotOutput(ns("plot_ranking")),
                               shiny::downloadButton(ns("download_ranking"), label = "",
                                                     class = "butt",
                                                     icon = shiny::icon("download"))),
                      tabPanel("Karte", br(), highcharter::highchartOutput(ns("plot_map_vertraege"))),
                      tabPanel("Datensatz", div(DT::dataTableOutput(ns("data_table_mix")),
                                                style = "font-size: 75%; width: 75%"),
                               shiny::downloadButton(ns("data_table_mix_box1"), label = "",
                                                     class = "butt",
                                                     icon = shiny::icon("download"))))

        )
      )
    ),
    fluidRow(
      shinydashboard::box(
        title = "Box 3",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        tags$head(tags$style(".butt{background-color:#FFFFFF;} .butt{color: #666666;}
                             .butt{border-color:#FFFFFF}")),
        shiny::sidebarPanel(
          mod_ausbildung_vertraege_verlauf_ui("mod_ausbildung_vertraege_verlauf_ui_1")),
        shiny::mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Ranking", br(), plotOutput(ns("plot_verlauf")),
                               shiny::downloadButton(ns("download_verlauf"), label = "",
                                                     class = "butt",
                                                     icon = shiny::icon("download"))),
                      tabPanel("Datensatz", div(DT::dataTableOutput(ns("data_table_verlauf")),
                                                style = "font-size: 75%; width: 75%"),
                               shiny::downloadButton(ns("download_data_box3"), label = "",
                                                     class = "butt",
                                                     icon = shiny::icon("download")))
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

    plot_waffle_react <- reactive({
      ausbildungsvertraege_waffle(data_ausbildungsvertraege, r)
    })

    output$plot_waffle <- renderPlot({
      plot_waffle_react()
    })

    plot_absolut_react <- reactive({
      ausbildungsvertraege_absolut(data_ausbildungsvertraege, r)
    })


    output$plot_absolut <- renderPlot({
      plot_absolut_react()
    })

    output$plot_map_vertraege <- highcharter::renderHighchart({
      vertraege_map(data_ausbildungsvertraege, r)
    })

    plot_ranking_react <- reactive({
      vertraege_ranking(data_ausbildungsvertraege, r)
    })

    output$plot_ranking <- renderPlot({
      plot_ranking_react()
    })

    plot_verlauf_react <- reactive({
      vertraege_verlauf(data_ausbildungsvertraege, r)
    })

    output$plot_verlauf <- renderPlot({
      plot_verlauf_react()
    })

    data_table_mix_react <- reactive({
      data_mix_vertraege(data_ausbildungsvertraege, r)
    })

    output$data_table_mix <- DT::renderDT({
      data_table_mix_react()
    })

    data_table_verlauf_react <- reactive({
      data_verlauf_vertraege(data_ausbildungsvertraege, r)
    })

    output$data_table_verlauf <- DT::renderDT({
      data_table_verlauf_react()
    })


    output$download_waffle <- shiny::downloadHandler(
      filename = function() {
        paste("plot_naa", "png", sep = ".")
      },
      content = function(file){
        ggplot2::ggsave(file, plot = plot_waffle_react(), device = "png",
                        dpi = 300, width = 10, height = 6)
      }
    )

    output$download_absolut <- shiny::downloadHandler(
      filename = function() {
        paste("plot_naa", "png", sep = ".")
      },
      content = function(file){
        ggplot2::ggsave(file, plot = plot_absolut_react(), device = "png",
                        dpi = 300, width = 10, height = 6)
      }
    )

    output$download_data_box1 <- shiny::downloadHandler(
      filename = function() {
        paste("data_naa", "csv", sep = ".")
      },
      content = function(file){
        write.csv(data_table_mix_react(), file)
      }
    )

    output$download_data_box3 <- shiny::downloadHandler(
      filename = function() {
        paste("data_naa", "csv", sep = ".")
      },
      content = function(file){
        write.csv(data_table_verlauf_react(), file)
      }
    )

    output$download_ranking <- shiny::downloadHandler(
      filename = function() {
        paste("plot_naa", "png", sep = ".")
      },
      content = function(file){
        ggplot2::ggsave(file, plot = plot_ranking_react(), device = "png",
                        dpi = 300, width = 10, height = 6)}
    )

    output$download_verlauf <- shiny::downloadHandler(
      filename = function() {
        paste("plot_naa", "png", sep = ".")
      },
      content = function(file){
        ggplot2::ggsave(file, plot = plot_verlauf_react(), device = "png",
                        dpi = 300, width = 10, height = 6)}
    )

  })
}

## To be copied in the UI
# mod_ausbildung_vertraege_ui("ausbildung_vertraege_1")

## To be copied in the server
# mod_ausbildung_vertraege_server("ausbildung_vertraege_1")
