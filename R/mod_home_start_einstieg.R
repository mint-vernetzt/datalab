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
    p("Wählen Sie einen oder mehrere Bereiche:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_start_einstieg_1"),
      choices = c("Leistungskurse",
                  "Studienanfänger", "Studierende",
                  "Auszubildende", "Beschäftigte"),
      selected = c("Leistungskurse", "Beschäftigte"),
      multiple = TRUE,
      options =  list(
        "max-options" = 3,
        "max-options-text" = "Bitte nur maximal 3 Bereiche auswählen"
      )
    )
  )
}

#' home_start_einstieg Server Functions
#'
#' @noRd
mod_home_start_einstieg_server <- function(id, r){
  moduleServer( id, function(input, output, session){


    observeEvent(input$indikator_start_einstieg_1, {
      r$indikator_start_einstieg_1 <- input$indikator_start_einstieg_1
    })

  })
}

## To be copied in the UI
# mod_home_start_einstieg_ui("home_start_einstieg_1")

## To be copied in the server
# mod_home_start_einstieg_server("home_start_einstieg_1")
