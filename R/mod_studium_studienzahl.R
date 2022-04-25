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
      title = "Lorem Ipsum",
      width = 12,
      shiny::sidebarPanel(
        tags$style(".well {background-color:#FFFFFF;}"),
        mod_studium_studienzahl_einstieg_ui("mod_studium_studienzahl_einstieg_ui_1")),
      shinydashboard::valueBoxOutput(ns("valueBox_einstieg_mint")),
      shinydashboard::valueBoxOutput(ns("valueBox_einstieg_rest")),

      shiny::mainPanel(
        tabsetPanel(type = "tabs",
        tabPanel("Bar Chart", plotly::plotlyOutput(ns("plot_einstieg_bar"))),
        tabPanel("Datensatz", div(DT::dataTableOutput(ns("data_table_einstieg")),
                                  style = "font-size: 75%; width: 75%"))))
      )),
    fluidRow(
      shinydashboard::box(
        title = "Lorem Ipsum",
        width = 12,
        shiny::sidebarPanel(
                    mod_studium_studienzahl_choice_1_ui("mod_studium_studienzahl_choice_ui_1_1")),
      shiny::mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel("Anteil", plotOutput(ns("plot_waffle"))),
                              tabPanel("Absolut", plotOutput(ns("plot_absolut"))),
                              tabPanel("Map", highcharter::highchartOutput(ns("plot_map_studienzahl"))))))),
    fluidRow(
      shinydashboard::box(
        title = "Lorem Ipsum",
        width = 12,
        shiny::sidebarPanel(
          mod_studium_studienzahl_verlauf_ui("mod_studium_studienzahl_verlauf_ui_1")),
        shiny::mainPanel(highcharter::highchartOutput(ns("plot_verlauf_studienzahl"))))),
    hr(),
    h4("Studienzahlen im zeitlichen Verlauf vergleichbar"),
    br(),br(),
    fluidRow(
    shiny::column(width = 6,
    mod_studium_studienzahl_choice_2_ui("mod_studium_studienzahl_choice_ui_2_1"))),
    br(),br(),
    fluidRow(
    shiny::column(width = 10,
                  tabsetPanel(type = "tabs",
                              tabPanel("Balkendiagramm",plotOutput(ns("plot"))),
                              tabPanel("Linienplot",plotOutput(ns("plot_line")))))
    )
  )
}

#' studium_studienzahl Server Functions
#'
#' @noRd
mod_studium_studienzahl_server <- function(id, data, data_studierende, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot <- renderPlot({
      studienzahl_plot(data,r)

    })

    output$plot_einstieg_bar <- plotly::renderPlotly({
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
    studienzahl_verlauf

    output$plot_line <- renderPlot({
      studienzahl_line(data,r)
    })

    output$valueBox_einstieg_mint <- shinydashboard::renderValueBox({
      res <- box_einstieg(data_studierende,r)

      value <- tags$p(style = "font-size: 40px;", paste0(res$anteil_mint,"%"))

      title <- "Studierende"

      text <- paste0("Durschnittlicher Anteil der Frauen an MINT!
                            ")

      text_info <- paste0("Durchschnittlicher Anteil der Frauen an MINT berechnet für
                          den gewählten Zeitraum und abhängig von den gewählten Filter")

      valueBox2(
        value, title, #icon = icon("graduation-cap"),
        subtitle = text,
        color = "navy",
        width = 6,
        info = text_info
      )
    })

    output$valueBox_einstieg_rest <- shinydashboard::renderValueBox({
      res <- box_einstieg(data_studierende,r)

      value <- tags$p(style = "font-size: 40px;", paste0(res$anteil_rest,"%"))

      title <- "Studierende"

      text <- paste0("Durschnittlicher Anteil der Frauen an allen anderen Fächer!")

      text_info <- paste0("Durchschnittlicher Anteil der Frauen an anderen Studienfächer berechnet für
                          den gewählten Zeitraum und abhängig von den gewählten Filter")

      valueBox2(
        value, title, #icon = icon("graduation-cap"),
        subtitle = text,
        color = "navy",
        width = 6,
        info = text_info
      )
    })

    output$box_durchschnitt_male <- shinydashboard::renderValueBox({
      res <- box_dynamic_1(data,r)
      value <- tags$p(style = "font-size: 100px;", paste0(res$male))
      title <- tags$p(style = "font-size: 25px;", paste0("Durchschnitt Männer in ",
                                                         res$year))
      ratio <-  round((as.numeric(res$male) /
        (as.numeric(res$female) + as.numeric(res$male)))*100)

      text <- paste0(ratio, "% Anteil der Männer (durchschn.)")

      text_info <- paste0("Durchschnittlicher Anteil der Männer ",
                          dictionary_title_studium_studentenzahl[[r$indikator_waffle]],
                         " für das Jahr ", r$date_waffle)

      valueBox2(
        value, title, icon = icon("graduation-cap"),
        subtitle = tagList(HTML("&excl;"), text),
        color = "blue",
        width = 4,
        info = text_info
      )
    })






    output$box_durchschnitt_female <- shinydashboard::renderValueBox({
      res <- box_dynamic_1(data,r)
      value <- tags$p(style = "font-size: 15px;", paste0(res$female))
      subtitle <- tags$p(style = "font-size: 15px;", paste0("Durchschnitt an Frauen /n
                                                            im Jahr ", res$year))
      shinydashboard::valueBox(
        value, subtitle, icon = icon("glyphicon-education", lib = "glyphicon"),
        color = "blue"
      )
    })



  })
}

## To be copied in the UI
# mod_studium_studienzahl_ui("studium_studienzahl_1")

## To be copied in the server
# mod_studium_studienzahl_server("studium_studienzahl_1")
