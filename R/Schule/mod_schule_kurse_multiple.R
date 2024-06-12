#' schule_kurse_multiple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_multiple_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021,2022),
      selected = 2022
    ),
    p("Kursart:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_kurse_gender"),
      choices = c("Grundkurse", "Leistungskurse"),
      selected = "Grundkurse"
    ),
    br(),
    shinyBS::bsPopover(id="ih_schule_mint_4", title="",
                       content = paste0("In der Grafik ist zu lesen, dass 2021 deutschlandweit sowohl für Mädchen wie Jungen 24 von 100 Grundkursbelegungen in einem MINT-Fach sind. Unterschiede sieht man in den Leistungskursen. 29 von 100 Leistungskursbelegungen von Mädchen, aber 38 % der Leistungskursbelegungen von Jungen sind in einem MINT-Fach."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_mint_4")
  )
}

#' schule_kurse_multiple Server Functions
#'
#' @noRd
mod_schule_kurse_multiple_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse, {
      r$date_kurse <- input$date_kurse
    })

    observeEvent(input$indikator_kurse_gender, {
      r$indikator_kurse_gender <- input$indikator_kurse_gender
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_multiple_ui("schule_kurse_multiple_1")

## To be copied in the server
# mod_schule_kurse_multiple_server("schule_kurse_multiple_1")
