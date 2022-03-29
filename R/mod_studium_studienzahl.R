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
    br(),br(),br(),
    fluidRow(
      shiny::column(width = 6,
                    mod_studium_studienzahl_choice_1_ui("mod_studium_studienzahl_choice_ui_1_1")),
      shiny::column(width = 6,
                    shinydashboard::valueBoxOutput(ns("box_durchschnitt_male")),
                    shinydashboard::valueBoxOutput(ns("box_durchschnitt_female"))
                   )),
    br(),
    fluidRow(
    shiny::column(width = 10,
                  plotOutput(ns("plot_waffle")))),
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
mod_studium_studienzahl_server <- function(id, data, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot <- renderPlot({
      studienzahl_plot(data,r)

    })

    output$plot_waffle <- renderPlot({
      studienzahl_waffle(data,r)
    })

    output$plot_line <- renderPlot({
      studienzahl_line(data,r)
    })

    output$box_durchschnitt_male <- shinydashboard::renderValueBox({
      res <- box_dynamic_1(data,r)
      value <- tags$p(style = "font-size: 15px;", paste0(res$male))
      subtitle <- tags$p(style = "font-size: 15px;", paste0("Durchschnitt an Männer /n
                                                            im Jahr ", res$year))
      shinydashboard::valueBox(
        value, subtitle, icon = icon("glyphicon-education", lib = "glyphicon"),
        color = "blue"
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
