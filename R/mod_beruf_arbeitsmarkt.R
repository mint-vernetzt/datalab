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
                      tabPanel("Balkendiagramm", highcharter::highchartOutput(ns("plot_einstieg_bar"))),
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
                      tabPanel("Anteil", br(), plotOutput(ns("plot_waffle"))),
                      tabPanel("Absolut", br(), plotOutput(ns("plot_absolut"))),
                      tabPanel("Karte", br(), highcharter::highchartOutput(ns("plot_map_arbeitsmarkt"))),
                      tabPanel("Datensatz", div(DT::dataTableOutput(ns("data_table_mix")),
                                                style = "font-size: 75%; width: 75%"))),
          br(),br(),

        ))),
    fluidRow(
      shinydashboard::box(
        title = "Lorem Ipsum",
        width = 12,
        shiny::sidebarPanel(
          mod_beruf_arbeitsmarkt_verlauf_ui("mod_beruf_arbeitsmarkt_verlauf_ui_1")),
        shiny::mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Verlauf",br(), highcharter::highchartOutput(ns("plot_verlauf_arbeitsmarkt"))),
                      tabPanel("Datensatz", div(DT::dataTableOutput(ns("data_table_verlauf")),
                                                style = "font-size: 75%; width: 75%"))
                      ))))
  )
}

#' beruf_arbeitsmarkt Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_server <- function(id, data_arbeitsmarkt, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot_einstieg_bar <-  highcharter::renderHighchart({
      arbeitsmarkt_einstieg_bar(data_arbeitsmarkt,r)

    })

    output$data_table_einstieg <- DT::renderDT({
      data_einstieg_beruf(data_arbeitsmarkt, r)
    })

    output$data_table_mix <- DT::renderDT({
      data_mix_beruf(data_arbeitsmarkt, r)
    })

    output$data_table_verlauf <- DT::renderDT({
      data_verlauf_beruf(data_arbeitsmarkt, r)
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

      value <- tags$p(style = "font-size: 40px;", paste0(res$anteil_mint_female,"%"))

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
        width = 6,
        icon = shiny::icon("building"),
        info = text_info,
        type = "Frauen"
      )
    })

    output$valueBox_einstieg_rest <- shinydashboard::renderValueBox({
      res <- box_einstieg_beruf(data_arbeitsmarkt,r)

      value <- tags$p(style = "font-size: 40px;", paste0(res$anteil_mint_male,"%"))

      if (r$indikator_arbeitsmarkt_einstieg == "Auszubildende"){

        title <- "Auszubildende"

      } else {

        title <- "Beschäftigte"

      }


      text <- paste0("Durschnittlicher Anteil von MINT bei berufstätigen Männern!")

      text_info <- paste0("Durschnittlicher Anteil von MINT bei berufstätigen Männern berechnet für
                          den gewählten Zeitraum und abhängig von den gewählten Filter.")

      valueBox2(
        value, title, #icon = icon("graduation-cap"),
        subtitle = text,
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
