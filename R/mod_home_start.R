#' home_start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_ui <- function(id){
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
        shiny::column(12,
                      mod_home_start_einstieg_ui("mod_home_start_einstieg_ui_1"),br(),br()),

        column(5,
               plotOutput(ns("plot_mint_rest_einstieg_1")
               ), br(), br()),
        column(5, offset = 2,
               plotOutput(ns("plot_mint_rest_einstieg_2")), br(), br()),

      # column(5,
      #        plotOutput(ns("plot_mint_rest_einstieg_3")
      #        ), br(), br()),
      # column(5, offset = 2,
      #        plotOutput(ns("plot_mint_rest_einstieg_4")), br(), br())
    )),
    fluidRow(
      shinydashboard::box(
        title = "Lorem Ipsum",
        width = 12,
        shiny::sidebarPanel(
          mod_home_start_comparison_mint_ui("mod_home_start_comparison_mint_ui_1")),
        shiny::mainPanel(highcharter::highchartOutput(ns("plot_comparison_mint"))

        )
      )
    ),
    fluidRow(
      shinydashboard::box(
        title = "Lorem Ipsum",
        width = 12,
        shiny::sidebarPanel(
          mod_home_start_leaky_ui("mod_home_start_leaky_ui_1")),
        shiny::mainPanel(plotOutput(ns("plot_leaky"))

        )
      )
    ),

    fluidRow(
      shinydashboard::box(
        title = "Lorem Ipsum",
        width = 12,
        shiny::sidebarPanel(
          mod_home_start_comparison_ui("mod_home_start_comparison_ui_1")),
        shiny::mainPanel(
          highcharter::highchartOutput(ns("plot_verlauf_mint"))
        )
      )
    ),
    fluidRow(
      shinydashboard::box(
        title = "Lorem Ipsum",
        width = 12,
        shiny::column(12,
          mod_home_start_multiple_ui("mod_home_start_multiple_ui_1")),
        #fluidRow(
          column(5,
                 plotOutput(ns("plot_mint_rest_1")
          )),
          column(5, offset = 2,
                 plotOutput(ns("plot_mint_rest_2")))
        #)
      )
    )
  )
}

#' home_start Server Functions
#'
#' @noRd
mod_home_start_server <- function(id, data_zentral, data_ausbildungsvertraege ,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$plot_verlauf_mint <- highcharter::renderHighchart({
      home_comparison_line(data_zentral,r)
    })

    output$plot_leaky <- renderPlot({
      home_leaky_pipeline(data_zentral,r)
    })

    output$plot_mint_rest_1 <- renderPlot({
      home_rest_mint_verlauf(data_zentral,r, "first")
    })

    output$plot_mint_rest_2 <- renderPlot({
      home_rest_mint_verlauf(data_zentral,r, "second")
    })

    output$plot_mint_rest_einstieg_1 <- renderPlot({
      home_einsitieg_waffle(data_zentral,r, "first")
    })

    output$plot_mint_rest_einstieg_2 <- renderPlot({
      home_einsitieg_waffle(data_zentral,r, "second")
    })

    output$plot_mint_rest_einstieg_3 <- renderPlot({
      home_einsitieg_waffle_female(data_zentral,r, "first")
    })

    output$plot_mint_rest_einstieg_4 <- renderPlot({
      home_einsitieg_waffle_female(data_zentral,r, "second")
    })

    output$plot_comparison_mint <- highcharter::renderHighchart({
      home_stacked_comparison(data_zentral, data_ausbildungsvertraege, r)
    })

  })
}

## To be copied in the UI
# mod_home_start_ui("home_start_1")

## To be copied in the server
# mod_home_start_server("home_start_1")
