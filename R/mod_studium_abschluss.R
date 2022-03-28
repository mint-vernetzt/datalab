#' studium_abschluss UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_abschluss_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shiny::column(width = 6,
                    mod_studium_abschluss_choice_2_ui("mod_studium_abschluss_choice_ui_2_1")
      ),
      shiny::column(width = 6,
                    shinydashboard::valueBoxOutput(ns("box_abschluss_male")),
                    shinydashboard::valueBoxOutput(ns("box_abschluss_female")))),
    br(), br(),br(), br(),
    fluidRow(
      shiny::column(width = 6,
                    plotOutput(ns("bar_abschluss"))),
      shiny::column(width = 6,
                    plotOutput(ns("waffle_abschluss")))),
    br(), br(),br(), br(),
    fluidRow(
      shiny::column(width = 6,
                    mod_studium_abschluss_choice_1_ui("mod_studium_abschluss_choice_ui_1_1")
      ),
      shiny::column(width = 6,
                    shinydashboard::valueBoxOutput(ns("box_abschluss_male_2")),
                    shinydashboard::valueBoxOutput(ns("box_abschluss_female_2")))),
    br(), br(),br(), br(),
    fluidRow(
      shiny::column(width = 6,
                    plotOutput(ns("bar_abschluss_2"))),
      shiny::column(width = 6,
                    plotOutput(ns("bar_abschluss_aenderung"))))

  )
}

#' studium_abschluss Server Functions
#'
#' @noRd
mod_studium_abschluss_server <- function(id, data, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$bar_abschluss <- renderPlot({
      abschlusszahl_bar(data,r)

    })

    output$waffle_abschluss <- renderPlot({
      abschlusszahl_waffle(data,r)

    })

    output$bar_abschluss_2 <- renderPlot({
      abschluss_bar_2(data,r)

    })

    output$bar_abschluss_aenderung <- renderPlot({
      abschluss_aenderung(data,r)

    })
    output$box_abschluss_male <- shinydashboard::renderValueBox({
      res <- box_dynamic_2(data,r)
      value <- tags$p(style = "font-size: 15px;", paste0(res$male))
      subtitle <- tags$p(style = "font-size: 15px;", paste0("Summe der Abschlüse Männer"))
      shinydashboard::valueBox(
        value, subtitle, icon = icon("glyphicon-education", lib = "glyphicon"),
        color = "blue"
      )
    })

    output$box_abschluss_female <- shinydashboard::renderValueBox({
      res <- box_dynamic_2(data,r)
      value <- tags$p(style = "font-size: 15px;", paste0(res$female))
      subtitle <- tags$p(style = "font-size: 15px;", paste0("Summe der Abschlüse Frauen"))
      shinydashboard::valueBox(
        value, subtitle, icon = icon("glyphicon-education", lib = "glyphicon"),
        color = "blue"
      )
    })

    output$box_abschluss_female_2 <- shinydashboard::renderValueBox({
      res <- box_dynamic_3(data,r)
      value <- tags$p(style = "font-size: 15px;", paste0(res$female))
      subtitle <- tags$p(style = "font-size: 15px;", paste0("Durschnitt an Abschlüssen für die Jahre ",
                                                            res$year_1, " bis ", res$year_2, " (Frauen)"))
      shinydashboard::valueBox(
        value, subtitle, icon = icon("glyphicon-education", lib = "glyphicon"),
        color = "blue"
      )
    })

    output$box_abschluss_male_2 <- shinydashboard::renderValueBox({
      res <- box_dynamic_3(data,r)
      value <- tags$p(style = "font-size: 15px;", paste0(res$male))
      subtitle <- tags$p(style = "font-size: 15px;", paste0("Durschnitt an Abschlüssen für die Jahre ",
                                                            res$year_1, " bis ", res$year_2, " (Männer)"))
      shinydashboard::valueBox(
        value, subtitle, icon = icon("glyphicon-education", lib = "glyphicon"),
        color = "blue"
      )
    })

  })
}

## To be copied in the UI
# mod_studium_abschluss_ui("studium_abschluss_1")

## To be copied in the server
# mod_studium_abschluss_server("studium_abschluss_1")
