#' home_start_einstieg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_einstieg_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_home_einstieg"),
      label = NULL,
      choices = c("2015", "2016", "2017",
                  "2018","2019"),
    ))),
    fluidRow(
      column(4,
             p("Wähle ein Bereich:"),
             shinyWidgets::pickerInput(
               inputId = ns("indikator_start_einstieg_1"),
               choices = c("Beschäftigte", "Auszubildende", "Habilitationen", "Leistungskurse",
                           "Promotionen (angestrebt)", "Studienanfänger", "Studierende"),
               selected = "Auszubildende"
             )
      ),
      column(4, offset = 4,
             p("Wähle ein Bereich:"),
             shinyWidgets::pickerInput(
               inputId = ns("indikator_start_einstieg_2"),
               choices = c("Beschäftigte", "Auszubildende", "Habilitationen", "Leistungskurse",
                           "Promotionen (angestrebt)", "Studienanfänger", "Studierende"),
               selected = "Studienanfänger"
             )
      )
    )
  )
}

#' home_start_einstieg Server Functions
#'
#' @noRd
mod_home_start_einstieg_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_home_einstieg, {
      r$date_home_einstieg <- input$date_home_einstieg
    })

    observeEvent(input$indikator_start_einstieg_1, {
      r$indikator_start_einstieg_1 <- input$indikator_start_einstieg_1
    })

    observeEvent(input$indikator_start_einstieg_2, {
      r$indikator_start_einstieg_2 <- input$indikator_start_einstieg_2
    })

  })
}

## To be copied in the UI
# mod_home_start_einstieg_ui("home_start_einstieg_1")

## To be copied in the server
# mod_home_start_einstieg_server("home_start_einstieg_1")
