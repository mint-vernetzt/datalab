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
                  "9. Klasse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
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
                         "max-options-text" = "<span style='color: red;'>Maximal 3 Bundesländer auswählen</span>")
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
                         "max-options-text" = "<span style='color: red;'>Maximal 3 Bundesländer auswählen</span>")
                     ),

                     p("Schulfach:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("fach_iqb_standard_zeitverlauf_9"),
                       choices = c("Mathematik",
                                   "Biologie" = "Biologie (Fachwissen)",
                                   "Chemie" = "Chemie (Fachwissen)",
                                   "Physik" = "Physik (Fachwissen)"),
                       multiple = FALSE,
                       selected = "Mathematik"
                     )
    ),

    br(),
    darstellung(id = "leistungsschwache_schueler1"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_schule_kompetenzen_1", title="",
                       content = paste0("Betrachtet man Deutschland zeigt sich: Während 2011 noch 11,9 % der Schüler:innen die Mindestanforderung im Mathematik-Kompetenztest nicht erfüllen, hat 2021 ein fast doppelt so großer Anteil an Schüler:innen wichtige Grundkenntnisse in d. Mathematik nicht mehr (21,8 % Mindeststandard nicht erreicht)."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_kompetenzen_1")

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
    observeEvent(input$fach_iqb_standard_zeitverlauf_9, {
      r$fach_iqb_standard_zeitverlauf_9 <- input$fach_iqb_standard_zeitverlauf_9
    })


  })
}

## To be copied in the UI
# mod_schule_kurse_iqb_standard_zeitverlauf_ui("mod_schule_kurse_iqb_standard_zeitverlauf_1")

## To be copied in the server
# mod_schule_kurse_iqb_standard_zeitverlauf_server("mod_schule_kurse_iqb_standard_zeitverlauf_1")
