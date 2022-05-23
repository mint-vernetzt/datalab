#' schule_kurse_ranking UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_ranking_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitpunkt:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_ranking"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    ),
    p("Wähle in welcher Form der Kurs belegt wurde:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("indikator_kurse_ranking"),
      choices = c("Grundkurse", "Leistungskurse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    )
  )
}

#' schule_kurse_ranking Server Functions
#'
#' @noRd
mod_schule_kurse_ranking_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse_ranking, {
      r$date_kurse_ranking <- input$date_kurse_ranking
    })

    observeEvent(input$indikator_kurse_ranking, {
      r$indikator_kurse_ranking <- input$indikator_kurse_ranking
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_ranking_ui("schule_kurse_ranking_1")

## To be copied in the server
# mod_schule_kurse_ranking_server("schule_kurse_ranking_1")
