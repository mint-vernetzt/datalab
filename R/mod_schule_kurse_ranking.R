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
    p("Wählen Sie ein Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_ranking"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
      selected = 2020
    ),
    p("Wählen Sie ein Bundesland:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_kurse_ranking"),
      choices = c("Berlin",
                  "Brandenburg",
                  "Bremen",
                  "Hamburg",
                  "Hessen",
                  "Mecklenburg-Vorpommern",
                  "Niedersachsen",
                  "Nordrhein-Westfalen",
                  "Rheinland-Pfalz",
                  "Saarland",
                  "Sachsen",
                  "Sachsen-Anhalt",
                  "Schleswig-Holstein",
                  "Thüringen",
                  "Westen",
                  "Osten"),
      selected = "Hessen"
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

    observeEvent(input$states_kurse_ranking, {
      r$states_kurse_ranking <- input$states_kurse_ranking
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_ranking_ui("schule_kurse_ranking_1")

## To be copied in the server
# mod_schule_kurse_ranking_server("schule_kurse_ranking_1")
