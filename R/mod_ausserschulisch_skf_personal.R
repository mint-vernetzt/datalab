#' mod_ausserschulisch_skf_personal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ausserschulisch_skf_personal_ui <- function(id){
  ns <- NS(id)

  tagList(

    p("Auswahl des Zeitraums:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("time_skf_personal"),
      label = NULL,
      choices = c("2012", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021", "2022"),
      selected = c("2017", "2022")
    ),

    p("Einrichtungsart:"),
    shinyWidgets::pickerInput(
      inputId = ns("ort_skf_personal"),
      choices = c("Alle Einrichtungen",
                  "Kita",
                  "Hort",
                  "Grundschule"),
      selected = c("Alle Einrichtungen", "Kita"),
      multiple = TRUE

    )
  )

}

#' ausserschulisch_skf_perosnal Server Functions
#'
#' @noRd
mod_ausserschulisch_skf_personal_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$time_skf_personal, {
      r$time_skf_personal <- input$time_skf_personal
    })

    observeEvent(input$ort_skf_personal, {
      r$ort_skf_personal <- input$ort_skf_personal
    })

  })
}

## To be copied in the UI (in mod_schule_kurse)
# mod_ausserschulisch_skf_personal_ui("mod_ausserschulisch_skf_personal_ui_1")

## To be copied in the server (in mod_schule)
# mod_ausserschulisch_skf_personal_server("mod_ausserschulisch_skf_personal_ui_1")
