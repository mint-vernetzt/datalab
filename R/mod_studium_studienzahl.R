#' studium_studienzahl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
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
        tags$style(".well {background-color:#FFFFFF;}"),
        tags$head(tags$style(HTML(".small-box {height: 140px}"))),
        mod_studium_studienzahl_einstieg_ui("mod_studium_studienzahl_einstieg_ui_1")),
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
        title = "Box 3",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        shiny::sidebarPanel(
                    mod_studium_studienzahl_choice_1_ui("mod_studium_studienzahl_choice_ui_1_1")),
      shiny::mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel("Anteil", br(), plotOutput(ns("plot_waffle"))),
                              tabPanel("Absolut", br(), plotOutput(ns("plot_absolut"))),
                              tabPanel("Karte", br(), highcharter::highchartOutput(ns("plot_map_studienzahl"))),
                              tabPanel("Datensatz", div(DT::dataTableOutput(ns("data_table_mix")),
                                                        style = "font-size: 75%; width: 75%"))),
                  br(),br(),
                  ))),
    fluidRow(
      shinydashboard::box(
        title = "Box 4",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        shiny::sidebarPanel(
          mod_studium_studienzahl_verlauf_ui("mod_studium_studienzahl_verlauf_ui_1")),
        shiny::mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Verlauf", br(), highcharter::highchartOutput(ns("plot_verlauf_studienzahl"))),
          tabPanel("Datensatz", div(DT::dataTableOutput(ns("data_table_verlauf")),
                                    style = "font-size: 75%; width: 75%")))

          )))
    # hr(),
    # h4("Studienzahlen im zeitlichen Verlauf vergleichbar"),
    # br(),br(),
    # fluidRow(
    # shiny::column(width = 6,
    # mod_studium_studienzahl_choice_2_ui("mod_studium_studienzahl_choice_ui_2_1"))),
    # br(),br(),
    # fluidRow(
    # shiny::column(width = 10,
    #               tabsetPanel(type = "tabs",
    #                           tabPanel("Balkendiagramm",plotOutput(ns("plot"))),
    #                           tabPanel("Linienplot",plotOutput(ns("plot_line")))))
    # )
  )
}

#' studium_studienzahl Server Functions
#'
#' @noRd
mod_studium_studienzahl_server <- function(id, data_studierende, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    output$plot_einstieg_bar <- highcharter::renderHighchart({
      studienzahl_einstieg_bar(data_studierende,r)

    })

    output$data_table_einstieg <- DT::renderDT({
      data_einstieg(data_studierende, r)
    })

    output$plot_absolut <- renderPlot({
      studienzahl_absolut(data_studierende,r)
    })

    output$plot_waffle <- renderPlot({
      studienzahl_waffle(data_studierende,r)
    })


    output$plot_map_studienzahl <- highcharter::renderHighchart({
      studienzahl_map(data_studierende,r)
    })

    output$plot_verlauf_studienzahl <- highcharter::renderHighchart({
      studienzahl_verlauf(data_studierende,r)
    })



    output$data_table_mix <- DT::renderDT({
      data_mix_studium(data_studierende, r)
    })

    output$data_table_verlauf <- DT::renderDT({
      data_verlauf_studium(data_studierende, r)
    })



    output$valueBox_einstieg_mint <- shinydashboard::renderValueBox({
      res <- box_einstieg_studium(data_studierende,r)

      value <- tags$p(style = "font-size: 40px;", paste0(res$anteil_mint_female,"%"))

      if (r$indikator_studierende_einstieg == "Studierende"){

        title <- "Studierende"

      } else {

        title <- "Studienanfänger"

      }

      text <- paste0("Durschnittlicher Anteil von MINT bei studierenden Frauen!")

      text_info <- paste0("Durschnittlicher Anteil von MINT bei studierenden Frauen berechnet für
                          den gewählten Zeitraum und abhängig von den gewählten Filter")

      valueBox2(
        value, title, #icon = icon("graduation-cap"),
        subtitle = text,
        width = 6,
        icon = shiny::icon("university"),
        info = text_info,
        type = "Frauen"

      )
    })

    output$valueBox_einstieg_rest <- shinydashboard::renderValueBox({
      res <- box_einstieg_studium(data_studierende,r)

      value <- tags$p(style = "font-size: 40px;", paste0(res$anteil_mint_male,"%"))

      if (r$indikator_studierende_einstieg == "Studierende"){

        title <- "Studierende"

      } else {

        title <- "Studienanfänger"

      }


      text <- paste0("Durschnittlicher Anteil von MINT bei studierenden Männer!")

      text_info <- paste0("Durschnittlicher Anteil von MINT bei studierenden Männer berechnet für
                          den gewählten Zeitraum und abhängig von den gewählten Filter")

      valueBox2(
        value, title, #icon = icon("graduation-cap"),
        subtitle = text,
        icon = shiny::icon("university"),
        width = 6,
        info = text_info
      )
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_ui("studium_studienzahl_1")

## To be copied in the server
# mod_studium_studienzahl_server("studium_studienzahl_1")
