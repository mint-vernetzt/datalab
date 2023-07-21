#' mod_ausserschulisch_skf_einrichtungen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ausserschulisch_skf_einrichtungen_ui <- function(id){
  ns <- NS(id)

  tagList(

    p("Auswahl des Zeitraums:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_skf_einrichtungen"),
      label = NULL,
      choices = c("2012", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021", "2022"),
      selected = c("2017", "2022")
    ),

    p("Einrichtungsart:"),
    shinyWidgets::pickerInput(
      inputId = ns("ort_skf_einrichtungen"),
      choices = c("Alle Einrichtungen",
                  "Kita",
                  "Hort",
                  "Grundschule"),
      multiple = FALSE,
      selected = c("Kita")

    )
  )

}

#' schule_kurse_verlauf Server Functions
#'
#' @noRd
mod_ausserschulisch_skf_einrichtungen_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_skf_einrichtungen, {
      r$date_skf_einrichtungen <- input$date_skf_einrichtungen
    })

    observeEvent(input$ort_skf_einrichtungen, {
      r$ort_skf_einrichtungen <- input$ort_skf_einrichtungen
    })

  })
}

## To be copied in the UI (in mod_schule_kurse)
# mod_ausserschulisch_skf_einrichtungen_ui("mod_ausserschulisch_skf_einrichtungen_ui_1")

## To be copied in the server (in mod_schule)
# mod_ausserschulisch_skf_einrichtungen_server("mod_ausserschulisch_skf_einrichtungen_ui_1")
