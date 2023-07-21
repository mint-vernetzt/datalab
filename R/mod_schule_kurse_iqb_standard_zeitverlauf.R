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
    p("Klassenstufe:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("klasse_iqb_standard_zeitverlauf"),
      choices = c("4. Klasse",
                  "9. Klasse")
    ),

    p("Region:"),
    conditionalPanel(condition = "input.klasse_iqb_standard_zeitverlauf == '4. Klasse'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("land_iqb_standard_zeitverlauf_4"),
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
                     )
                ),
    conditionalPanel(condition = "input.klasse_iqb_standard_zeitverlauf == '9. Klasse'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("land_iqb_standard_zeitverlauf_9"),
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

                       selected = c("Deutschland",
                                    "Bayern","Bremen"),
                       multiple = TRUE,
                       options =  list(
                         "max-options" = 3,
                         "max-options-text" = "Maximal 3 Bundesländer auswählen")
                     )
    )


  )

}

#' schule_kurse_verlauf Server Functions
#'
#' @noRd
mod_schule_kurse_iqb_standard_zeitverlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$klasse_iqb_standard_zeitverlauf, {
      r$klasse_iqb_standard_zeitverlauf <- input$klasse_iqb_standard_zeitverlauf
    })
    observeEvent(input$land_iqb_standard_zeitverlauf_4, {
      r$land_iqb_standard_zeitverlauf_4 <- input$land_iqb_standard_zeitverlauf_4
    })
    observeEvent(input$land_iqb_standard_zeitverlauf_9, {
      r$land_iqb_standard_zeitverlauf_9 <- input$land_iqb_standard_zeitverlauf_9
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_iqb_standard_zeitverlauf_ui("mod_schule_kurse_iqb_standard_zeitverlauf_1")

## To be copied in the server
# mod_schule_kurse_iqb_standard_zeitverlauf_server("mod_schule_kurse_iqb_standard_zeitverlauf_1")
