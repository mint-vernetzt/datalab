#' mod_schule_kurse_iqb_mathe_mittel_zeitverlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_iqb_mathe_mittel_zeitverlauf_ui <- function(id){
  ns <- NS(id)

  tagList(
    p("Klassenstufe:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("klasse_iqb_mathe_mittel_zeitverlauf"),
      choices = c("4. Klasse",
                  "9. Klasse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),

    p("Indikator:"),
    conditionalPanel(condition = "input.klasse_iqb_mathe_mittel_zeitverlauf == '4. Klasse'",
                     ns = ns,
    shinyWidgets::pickerInput(
      inputId = ns("indi_iqb_mathe_mittel_zeitverlauf_4"),
      choices = c("nach Geschlecht",
                  "nach Zuwanderungsgeschichte",
                  "nach Bildungskapital"),
      multiple = FALSE,
      selected = c("nach Geschlecht")
  )
  ),
  conditionalPanel(condition = "input.klasse_iqb_mathe_mittel_zeitverlauf == '9. Klasse'",
                   ns = ns,
                   shinyWidgets::pickerInput(
                     inputId = ns("indi_iqb_mathe_mittel_zeitverlauf_9"),
                     choices = c("nach Geschlecht",
                                 "nach Zuwanderungsgeschichte",
                                 "nach sozialem Status"),
                     multiple = FALSE,
                     selected = c("nach Geschlecht")

                   )
  ),

  p("Region:"),
  conditionalPanel(condition = "input.klasse_iqb_mathe_mittel_zeitverlauf == '4. Klasse'",
                   ns = ns,
                   shinyWidgets::pickerInput(
                     inputId = ns("land_iqb_mathe_mittel_zeitverlauf_4"),
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
                     multiple = FALSE,
                     selected = c("Deutschland")
                   )
  ),
  conditionalPanel(condition = "input.klasse_iqb_mathe_mittel_zeitverlauf == '9. Klasse' &
                   input.indi_iqb_mathe_mittel_zeitverlauf_9 == 'nach Zuwanderungsgeschichte'",
                   ns = ns,
                   shinyWidgets::pickerInput(
                     inputId = ns("land_iqb_mathe_mittel_zeitverlauf_9_zwg"),
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
  ),
  conditionalPanel(condition = "input.klasse_iqb_mathe_mittel_zeitverlauf == '9. Klasse' &
                   input.indi_iqb_mathe_mittel_zeitverlauf_9 == 'nach Geschlecht'",
                   ns = ns,
                   shinyWidgets::pickerInput(
                     inputId = ns("land_iqb_mathe_mittel_zeitverlauf_9_gen"),
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
  ),
  conditionalPanel(condition = "input.klasse_iqb_mathe_mittel_zeitverlauf == '9. Klasse' &
                   input.indi_iqb_mathe_mittel_zeitverlauf_9 == 'nach sozialem Status'",
                   ns = ns,
                   shinyWidgets::pickerInput(
                     inputId = ns("land_iqb_mathe_mittel_zeitverlauf_9_sozS"),
                     choices = c("Deutschland",
                                 "Baden-Württemberg",
                                 "Bayern",
                                 "Brandenburg",
                                 "Hessen",
                                 "Mecklenburg-Vorpommern",
                                 "Niedersachsen",
                                 "Nordrhein-Westfalen",
                                 "Rheinland-Pfalz",
                                 "Sachsen",
                                 "Sachsen-Anhalt",
                                 "Schleswig-Holstein",
                                 "Thüringen"),

                     selected = c("Deutschland"),
                     multiple = FALSE
                   )
  ),

  conditionalPanel(condition = "input.klasse_iqb_mathe_mittel_zeitverlauf == '9. Klasse'",
                   ns = ns,
                   p("Schulfach:"),
                   shinyWidgets::pickerInput(
                     inputId = ns("fach_iqb_mathe_mittel_zeitverlauf"),
                     choices = c("Mathematik",
                                 "Biologie",
                                 "Chemie",
                                 "Physik"),
                     multiple = FALSE,
                     selected = "Mathematik"
                   )),

  conditionalPanel(condition = "input.klasse_iqb_mathe_mittel_zeitverlauf == '4. Klasse' &
                   input.land_iqb_mathe_mittel_zeitverlauf_4 == 'Deutschland' &
                   input.indi_iqb_mathe_mittel_zeitverlauf_4 == 'nach Geschlecht'",
                   ns = ns,
                   p("Leistungsindikator:"),
                   shinyWidgets::pickerInput(
                     inputId =  ns("score_iqb_mathe_mittel_zeitverlauf"),
                     choices = c("Test-Punktzahl", "Mindeststandard"),
                     multiple = FALSE,
                     selected = "Test-Punktzahl"
                   )
                   ),

  br(),
  darstellung(id = "dh_schule_kompetenz_2"),
  br(),
  br(),
  shinyBS::bsPopover(id="ih_schule_kompetenz_2", title="",
                     content = paste0("In der ersten Einstellung ist zu sehen, dass in Deutschland 2011 bis 2021 die Mathekompetenzen von Jungen im Mittel höher als die von Mädchen sind. Ein möglicher Grund hierfür könnten Genderstereotype sein (Weiteres dazu ist in der Darstellung im nächsten Tab und in der Infobox über den Grafiken zu finden)."),
                     placement = "top",
                     trigger = "hover"),
  tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_kompetenz_2")

  )

}

#' schule_kurse_verlauf Server Functions
#'
#' @noRd
mod_schule_kurse_iqb_mathe_mittel_zeitverlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$klasse_iqb_mathe_mittel_zeitverlauf, {
      r$klasse_iqb_mathe_mittel_zeitverlauf <- input$klasse_iqb_mathe_mittel_zeitverlauf
    })

    observeEvent(input$land_iqb_mathe_mittel_zeitverlauf_4, {
      r$land_iqb_mathe_mittel_zeitverlauf_4 <- input$land_iqb_mathe_mittel_zeitverlauf_4
    })

    observeEvent(input$land_iqb_mathe_mittel_zeitverlauf_9_gen, {
      r$land_iqb_mathe_mittel_zeitverlauf_9_gen <- input$land_iqb_mathe_mittel_zeitverlauf_9_gen
    })

    observeEvent(input$land_iqb_mathe_mittel_zeitverlauf_9_zwg, {
      r$land_iqb_mathe_mittel_zeitverlauf_9_zwg <- input$land_iqb_mathe_mittel_zeitverlauf_9_zwg
    })

    observeEvent(input$land_iqb_mathe_mittel_zeitverlauf_9_sozS, {
      r$land_iqb_mathe_mittel_zeitverlauf_9_sozS <- input$land_iqb_mathe_mittel_zeitverlauf_9_sozS
    })

    observeEvent(input$indi_iqb_mathe_mittel_zeitverlauf_4, {
      r$indi_iqb_mathe_mittel_zeitverlauf_4 <- input$indi_iqb_mathe_mittel_zeitverlauf_4
    })

    observeEvent(input$indi_iqb_mathe_mittel_zeitverlauf_9, {
      r$indi_iqb_mathe_mittel_zeitverlauf_9 <- input$indi_iqb_mathe_mittel_zeitverlauf_9
    })

    observeEvent(input$fach_iqb_mathe_mittel_zeitverlauf, {
      r$fach_iqb_mathe_mittel_zeitverlauf <- input$fach_iqb_mathe_mittel_zeitverlauf
    })

    observeEvent(input$score_iqb_mathe_mittel_zeitverlauf, {
      r$score_iqb_mathe_mittel_zeitverlauf <- input$score_iqb_mathe_mittel_zeitverlauf
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_iqb_standard_zeitverlauf_ui("mod_schule_kurse_iqb_standard_zeitverlauf_1")

## To be copied in the server
# mod_schule_kurse_iqb_standard_zeitverlauf_server("mod_schule_kurse_iqb_standard_zeitverlauf_1")
