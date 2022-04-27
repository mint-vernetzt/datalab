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
        title = "Lorem Ipsum",
        width = 12,
        shiny::sidebarPanel(
          tags$style(".well {background-color:#FFFFFF;}"),
          mod_schule_kurse_einstieg_ui("mod_schule_kurse_einstieg_ui_1")),
        shinydashboard::valueBoxOutput(ns("valueBox_einstieg_mint")),
        shinydashboard::valueBoxOutput(ns("valueBox_einstieg_rest")),

        shiny::mainPanel(

          tabsetPanel(type = "tabs",
                      tabPanel("Bar Chart", plotly::plotlyOutput(ns("plot_einstieg_bar"))),
                      tabPanel("Datensatz", div(DT::dataTableOutput(ns("data_table_einstieg")),
                                                style = "font-size: 75%; width: 75%"))))
      ))
  )
}

#' schule_kurse Server Functions
#'
#' @noRd
mod_schule_kurse_server <- function(id, data_kurse, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot_einstieg_bar <- plotly::renderPlotly({
      kurse_einstieg_bar(data_kurse,r)

    })

    output$data_table_einstieg <- DT::renderDT({
      data_einstieg_kurse(data_kurse, r)
    })

    output$valueBox_einstieg_mint <- shinydashboard::renderValueBox({
      res <- box_einstieg_kurse(data_kurse,r)

      value <- tags$p(style = "font-size: 40px;", paste0(res$anteil_mint,"%"))

      if (r$indikator_kurse_einstieg == "Grundkurse"){

        title <- "Grundkurse"

      } else {

        title <- "Leistungskurse"

      }

      text <- paste0("Durschnittlicher Anteil von MINT bei Frauen!")

      text_info <- paste0("Durschnittlicher Anteil von MINT bei Frauen in der Schule berechnet für
                          den gewählten Zeitraum und abhängig von den gewählten Filter.")

      valueBox2(
        value, title, #icon = icon("graduation-cap"),
        subtitle = text,
        color = "navy",
        width = 6,
        info = text_info,
        type = "MINT"
      )
    })

    output$valueBox_einstieg_rest <- shinydashboard::renderValueBox({
      res <- box_einstieg_kurse(data_kurse,r)

      value <- tags$p(style = "font-size: 40px;", paste0(res$anteil_rest,"%"))

      if (r$indikator_kurse_einstieg == "Grundkurse"){

        title <- "Grundkurse"

      } else {

        title <- "Leistungskurse"

      }


      text <- paste0("Durschnittlicher Anteil von allen anderen Fächern bei Frauen!")

      text_info <- paste0("Durschnittlicher Anteil von allen anderen Fächern bei Frauen in der Schule berechnet für
                          den gewählten Zeitraum und abhängig von den gewählten Filter.")

      valueBox2(
        value, title, #icon = icon("graduation-cap"),
        subtitle = text,
        color = "navy",
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
