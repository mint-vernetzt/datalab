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
      selected = c("Kita"),
      multiple = FALSE

    ),
    br(),
    shinyBS::bsPopover(id="ih_schule_ausserschulisch_2", title="",
                       content = paste0("Diese Darstellung zeigt, wie viele Lehrkräfte an SKf-Fortbildungen zur MINT-Bildung teilgenommen haben. Bis zum Jahr 2022 wurden bereits insgesamt 86.000 Fach- und Lehrkräfte an Kitas, Horten und Grundschulen durch SKf fortgebildet. Allerdings nehmen seit 2019 immer weniger Personen neu an Fortbildungen teil."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_ausserschulisch_2")
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
