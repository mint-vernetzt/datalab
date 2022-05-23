#' home_start_multiple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_multiple_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_start_multiple"),
      label = NULL,
      choices = c("2015", "2016", "2017",
                  "2018","2019"),
      selected = c("2015", "2019")
    ),
    p("Wähle ein Bereich:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_start_multiple_1"),
      choices = c("Beschäftigte", "Auszubildende", "Habilitationen", "Leistungskurse",
                  "Promotionen (angestrebt)", "Studienanfänger", "Studierende"),
      selected = c("Leistungskurse", "Beschäftigte"),
      options = list(`actions-box` = TRUE,
                     `deselect-all-text` = "Alle abwählen",
                     `select-all-text` = "Alle auswählen"),
      multiple = T
    )
  )
}

#' home_start_multiple Server Functions
#'
#' @noRd
mod_home_start_multiple_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_start_multiple, {
      r$date_start_multiple <- input$date_start_multiple
    })

    observeEvent(input$indikator_start_multiple_1, {
      r$indikator_start_multiple_1 <- input$indikator_start_multiple_1
    })

  })
}

## To be copied in the UI
# mod_home_start_multiple_ui("home_start_multiple_1")

## To be copied in the server
# mod_home_start_multiple_server("home_start_multiple_1")