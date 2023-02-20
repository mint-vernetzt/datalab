#' schule_kurse_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_map"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
      selected = 2021
    ),
    p("Auswahl des Fachs:"),
    shinyWidgets::pickerInput(
      inputId = ns("subject_map"),
      choices = c("MINT-Fächer (gesamt)",
                  "Mathematik",
                  "Informatik",
                  "Physik",
                  "Chemie",
                  "Biologie",
                  "andere Fächer (gesamt)",
                  "Deutsch",
                  "Fremdsprachen",
                  "Gesellschaftswissenschaften",
                  "Musik/Kunst",
                  "Religion/Ethik",
                  "Sport"),
      selected = "MINT-Fächer (gesamt)"
    )
  )
}

#' schule_kurse_map Server Functions
#'
#' @noRd
mod_schule_kurse_map_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_map, {
      r$date_map <- input$date_map
    })

    observeEvent(input$subject_map, {
      r$subject_map <- input$subject_map
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_map_ui("schule_kurse_map_1")

## To be copied in the server
# mod_schule_kurse_map_server("schule_kurse_map_1")
