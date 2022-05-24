#' schule_kurse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_ui <- function(id){
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
          mod_schule_kurse_einstieg_ui("mod_schule_kurse_einstieg_ui_1")),
        #shinydashboard::valueBoxOutput(ns("valueBox_einstieg_mint")),
        #shinydashboard::valueBoxOutput(ns("valueBox_einstieg_rest")),

        shiny::mainPanel(

          tabsetPanel(type = "tabs",
                      tabPanel("Kuchendiagramm", htmlOutput(ns("plot_einstieg_pie"))),
                      tabPanel("Datensatz", div(DT::dataTableOutput(ns("data_table_einstieg")),
                                                style = "font-size: 75%; width: 75%"))))
      )),
    fluidRow(
      shinydashboard::box(
        title = "Box 3",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        shiny::sidebarPanel(
          mod_schule_kurse_multiple_ui("mod_schule_kurse_multiple_ui_1")),
        shiny::mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Anteil", br(), plotOutput(ns("plot_waffle"))),
                      tabPanel("Absolut", br(), plotOutput(ns("plot_absolut"))),
                      #tabPanel("Ranking 1", br(), plotOutput(ns("plot_ranking_1"))),
                      #tabPanel("Ranking 2", br(), plotOutput(ns("plot_ranking_2"))),
                      tabPanel("Karte", br(), htmlOutput(ns("plot_map_kurse"))),
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
          mod_schule_kurse_ranking_ui("mod_schule_kurse_ranking_ui_1")),
        shiny::mainPanel(
          plotOutput(ns("plot_ranking_2"))
        ))),
    fluidRow(
      shinydashboard::box(
        title = "Box 5",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        shiny::sidebarPanel(
          mod_schule_kurse_verlauf_bl_ui("mod_schule_kurse_verlauf_bl_ui_1")),
        shiny::mainPanel(
          highcharter::highchartOutput(ns("plot_verlauf_kurse_bl"))

        ))),
    fluidRow(
      shinydashboard::box(
        title = "Box 6",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        shiny::sidebarPanel(
          mod_schule_kurse_verlauf_bl_subjects_ui("mod_schule_kurse_verlauf_bl_subjects_ui_1")),
        shiny::mainPanel(
          highcharter::highchartOutput(ns("plot_verlauf_kurse_bl_subjects"))

        ))),
    fluidRow(
      shinydashboard::box(
        title = "Box 7",
        width = 12,
        p("Lorem ipsum dolor sit amet"),
        shiny::sidebarPanel(
          mod_schule_kurse_verlauf_ui("mod_schule_kurse_verlauf_ui_1")),
        shiny::mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Verlauf", br(), highcharter::highchartOutput(ns("plot_verlauf_kurse"))),
                      tabPanel("Datensatz", div(DT::dataTableOutput(ns("data_table_verlauf")),
                                                style = "font-size: 75%; width: 75%")))
        )))
  )
}

#' schule_kurse Server Functions
#'
#' @noRd
mod_schule_kurse_server <- function(id, data_kurse, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot_einstieg_pie <- renderUI({
      kurse_einstieg_pie(data_kurse,r)

    })


    output$data_table_einstieg <- DT::renderDT({
      data_einstieg_kurse(data_kurse, r)
    })

    output$plot_waffle <- renderPlot({
      kurse_waffle(data_kurse,r)
    })

    output$plot_absolut <- renderPlot({
      kurse_absolut(data_kurse,r)
    })

    output$plot_ranking_1 <- renderPlot({
      kurse_ranking(data_kurse,r, type="first")
    })

    output$plot_ranking_2 <- renderPlot({
      kurse_ranking(data_kurse,r, type="other")
    })

    output$plot_map_kurse <- renderUI({
      kurse_map(data_kurse,r)
    })

    output$plot_verlauf_kurse <- highcharter::renderHighchart({
      kurse_verlauf(data_kurse,r)
    })

    output$plot_verlauf_kurse_bl <- highcharter::renderHighchart({
      kurse_verlauf_single_bl(data_kurse,r)
    })

    output$plot_verlauf_kurse_bl_subjects <- highcharter::renderHighchart({
      kurse_verlauf_subjects_bl(data_kurse,r)
    })

    output$data_table_mix <- DT::renderDT({
      data_mix_kurse(data_kurse, r)
    })

    output$data_table_verlauf <- DT::renderDT({
      data_verlauf_kurse(data_kurse, r)
    })

    output$valueBox_einstieg_mint <- shinydashboard::renderValueBox({
      res <- box_einstieg_kurse(data_kurse,r)

      value <- tags$p(style = "font-size: 40px;", paste0(res$anteil_mint_female,"%"))

      if (r$indikator_kurse_einstieg == "Grundkurse"){

        title <- "Grundkurse"

      } else {

        title <- "Leistungskurse"

      }

      text <- paste0("Durschnittlicher Anteil von MINT bei Frauen!")

      text_info <- paste0("Durschnittlicher Anteil von MINT bei Frauen in der Schule berechnet für
                          den gewählten Zeitraum und abhängig von den gewählten Filter.")

      valueBox2(
        value, title,
        subtitle = text,
        icon = shiny::icon("school"),
        width = 6,
        info = text_info,
        type = "Frauen"
      )
    })

    output$valueBox_einstieg_rest <- shinydashboard::renderValueBox({
      res <- box_einstieg_kurse(data_kurse,r)

      value <- tags$p(style = "font-size: 40px;", paste0(res$anteil_mint_male,"%"))

      if (r$indikator_kurse_einstieg == "Grundkurse"){

        title <- "Grundkurse"

      } else {

        title <- "Leistungskurse"

      }


      text <- paste0("Durschnittlicher Anteil von MINT bei Männer!")

      text_info <- paste0("Durschnittlicher Anteil von MINT bei Männer in der Schule berechnet für
                          den gewählten Zeitraum und abhängig von den gewählten Filter.")

      valueBox2(
        value, title,
        subtitle = text,
        icon = shiny::icon("school"),
        width = 6,
        info = text_info
      )
    })


  })
}

## To be copied in the UI
# mod_schule_kurse_ui("schule_kurse_1")

## To be copied in the server
# mod_schule_kurse_server("schule_kurse_1")
