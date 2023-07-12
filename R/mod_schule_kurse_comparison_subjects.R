#' schule_kurse_comparison_subjects UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_comparison_subjects_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_comparison_subject"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021"),
      selected = "2021"
    ),
    p("Kursart:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_comparison_subject"),
      choices = c("Grundkurse", "Leistungskurse"),
      selected = "Leistungskurse"
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("state_comparison_subject"),
      choices = c("Deutschland",
                  "Baden-Württemberg",
                  "Bayern",
                  "Berlin",
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
                  "Thüringen"
                ,
                 "Westdeutschland (o. Berlin)",
                "Ostdeutschland (inkl. Berlin)"
                  ),
      selected = "Hessen"

    ),
    br(),
    shinyBS::bsPopover(id="ih_schule_fach_5", title="",
                       content = paste0("Diese Grafik zeigt, wie sich &quotMINT&quot und &quotnicht MINT&quot auf einzelne Fächer aufteilen. In Hessen macht beispielweise Biologie knapp vor Mathe den größten Anteil an Leistungskursbelegungen in MINT aus. Für die anderen Fächer sind Fremdsprachen Spitzenreiter."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_fach_5")

  )
}

#' schule_kurse_comparison_subjects Server Functions
#'
#' @noRd
mod_schule_kurse_comparison_subjects_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_comparison_subject, {
      r$date_comparison_subject <- input$date_comparison_subject
    })

    observeEvent(input$indikator_comparison_subject, {
      r$indikator_comparison_subject <- input$indikator_comparison_subject
    })

    observeEvent(input$state_comparison_subject, {
      r$state_comparison_subject <- input$state_comparison_subject
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_comparison_subjects_ui("schule_kurse_comparison_subjects_1")

## To be copied in the server
# mod_schule_kurse_comparison_subjects_server("schule_kurse_comparison_subjects_1")
