#' beruf_arbeitsmarkt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        title = "Lorem Ipsum",
        width = 12,
        shiny::sidebarPanel(
          tags$style(".well {background-color:#FFFFFF;}"),
          tags$head(tags$style(HTML(".small-box {height: 140px}"))),
          mod_beruf_arbeitsmarkt_einstieg_ui("mod_beruf_arbeitsmarkt_einstieg_ui_1")),
        shinydashboard::valueBoxOutput(ns("valueBox_einstieg_mint")),
        shinydashboard::valueBoxOutput(ns("valueBox_einstieg_rest")),

        shiny::mainPanel(

          tabsetPanel(type = "tabs",
                      tabPanel("Balkendiagramm", plotly::plotlyOutput(ns("plot_einstieg_bar"))),
                      tabPanel("Datensatz", div(DT::dataTableOutput(ns("data_table_einstieg")),
                                                style = "font-size: 75%; width: 75%"))))
    )),
    fluidRow(
      shinydashboard::box(
        title = "Lorem Ipsum",
        width = 12,
        shiny::sidebarPanel(
          mod_beruf_arbeitsmarkt_multiple_ui("mod_beruf_arbeitsmarkt_multiple_ui_1")),
        shiny::mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Anteil", plotOutput(ns("plot_waffle"))),
                      tabPanel("Absolut", plotOutput(ns("plot_absolut"))),
                      tabPanel("Karte", highcharter::highchartOutput(ns("plot_map_arbeitsmarkt")))),
          br(),br(),

        ))),
    fluidRow(
      shinydashboard::box(
        title = "Lorem Ipsum",
        width = 12,
        shiny::sidebarPanel(
          mod_beruf_arbeitsmarkt_verlauf_ui("mod_beruf_arbeitsmarkt_verlauf_ui_1")),
        shiny::mainPanel(highcharter::highchartOutput(ns("plot_verlauf_arbeitsmarkt")))))
  )
}

#' beruf_arbeitsmarkt Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_server <- function(id, data_arbeitsmarkt, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot_einstieg_bar <- plotly::renderPlotly({
      arbeitsmarkt_einstieg_bar(data_arbeitsmarkt,r)

    })

    output$data_table_einstieg <- DT::renderDT({
      data_einstieg_beruf(data_arbeitsmarkt, r)
    })

    output$plot_waffle <- renderPlot({
      arbeitnehmer_waffle(data_arbeitsmarkt, r)
    })

    output$plot_absolut <- renderPlot({
      arbeitsmarkt_absolut(data_arbeitsmarkt, r)
    })


    output$plot_map_arbeitsmarkt <- highcharter::renderHighchart({
      arbeitsmarkt_map(data_arbeitsmarkt,r)
    })

    output$plot_verlauf_arbeitsmarkt <- highcharter::renderHighchart({
      arbeitsmarkt_verlauf(data_arbeitsmarkt,r)
    })

    output$valueBox_einstieg_mint <- shinydashboard::renderValueBox({
      res <- box_einstieg_beruf(data_arbeitsmarkt,r)

      value <- tags$p(style = "font-size: 40px;", paste0(res$anteil_mint,"%"))

      if (r$indikator_arbeitsmarkt_einstieg == "Auszubildende"){

        title <- "Auszubildende"

      } else {

        title <- "Beschäftigte"

      }

      text <- paste0("Durschnittlicher Anteil von MINT bei berufstätigen Frauen!")

      text_info <- paste0("Durschnittlicher Anteil von MINT bei berufstätigen Frauen berechnet für
                          den gewählten Zeitraum und abhängig von den gewählten Filter.")

      valueBox2(
        value, title, #icon = icon("graduation-cap"),
        subtitle = text,
        color = "navy",
        width = 6,
        icon = shiny::icon("building"),
        info = text_info,
        type = "MINT"
      )
    })

    output$valueBox_einstieg_rest <- shinydashboard::renderValueBox({
      res <- box_einstieg_beruf(data_arbeitsmarkt,r)

      value <- tags$p(style = "font-size: 40px;", paste0(res$anteil_rest,"%"))

      if (r$indikator_arbeitsmarkt_einstieg == "Auszubildende"){

        title <- "Auszubildende"

      } else {

        title <- "Beschäftigte"

      }


      text <- paste0("Durschnittlicher Anteil von allen anderen Berufszweigen bei berufstätigen Frauen!")

      text_info <- paste0("Durschnittlicher Anteil von allen anderen Berufszweigen bei berufstätigen Frauen berechnet für
                          den gewählten Zeitraum und abhängig von den gewählten Filter.")

      valueBox2(
        value, title, #icon = icon("graduation-cap"),
        subtitle = text,
        color = "navy",
        icon = shiny::icon("building"),
        width = 6,
        info = text_info
      )
    })


  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_ui("beruf_arbeitsmarkt_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_server("beruf_arbeitsmarkt_1")
