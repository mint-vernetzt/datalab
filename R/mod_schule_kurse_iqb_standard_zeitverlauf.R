#' mod_schule_kurse_iqb_standard_zeitverlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_iqb_standard_zeitverlauf_ui <- function(id){
  ns <- NS(id)

  tagList(

    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("land_iqb_standard_zeitverlauf"),
      choices = c("Deutschland",
                  "Baden-Württemberg",
                  "Bayern",
                  "Berlin",
                  "Brandenburg",
                  "Bremen",
                  "Hamburg",
                  "Hessen",
                 # "Mecklenburg-Vorpommern",
                  "Niedersachsen",
                  "Nordrhein-Westfalen",
                  "Rheinland-Pfalz",
                  "Saarland",
                  "Sachsen",
                  "Sachsen-Anhalt",
                  "Schleswig-Holstein",
                  "Thüringen"),

    selected = c("Deutschland",
                 "Bayern","Bremen"),
      multiple = TRUE,
      options =  list(
        "max-options" = 3,
        "max-options-text" = "Maximal 3 Bundesländer auswählen")
    ),
    br(),
    shinyBS::bsPopover(id="ih_schule_kompetenzen_1", title="",
                       content = paste0("Betrachtet man Deutschland zeigt sich: Während 2011 noch 11.9 % der Schüler:innen die Mindestanforderung im Mathematik-Kompetenztest nicht erfüllen, hat 2021 ein fast doppelt so großer Anteil an Schüler:innen wichtige Grundkenntnisse in Mathe nicht mehr (21.8 % Mindesstandard nicht erreicht)."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_kompetenzen_1")
  )

}

#' schule_kurse_verlauf Server Functions
#'
#' @noRd
mod_schule_kurse_iqb_standard_zeitverlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$land_iqb_standard_zeitverlauf, {
      r$land_iqb_standard_zeitverlauf <- input$land_iqb_standard_zeitverlauf
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_iqb_standard_zeitverlauf_ui("mod_schule_kurse_iqb_standard_zeitverlauf_1")

## To be copied in the server
# mod_schule_kurse_iqb_standard_zeitverlauf_server("mod_schule_kurse_iqb_standard_zeitverlauf_1")
