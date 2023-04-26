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

    p("Auwahl des Bundeslands:"),
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
                  "Mecklenburg-Vorpommern",
                  "Niedersachsen",
                  "Nordrhein-Westfalen",
                  "Rheinland-Pfalz",
                  "Saarland",
                  "Sachsen",
                  "Sachsen-Anhalt",
                  "Schleswig-Holstein",
                  "Thüringen"),
      multiple = FALSE,
      selected = c("Deutschland")
    )
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
