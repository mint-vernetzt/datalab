#' mod_schule_kurse_iqb_fragebogen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_iqb_fragen_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Klassenstufe:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("klasse_iqb_fragebogen"),
      choices = c("4. Klasse",
                  "9. Klasse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),

    conditionalPanel(condition = "input.klasse_iqb_fragebogen == '4. Klasse'",
                     ns = ns,

                     p("Jahre:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("jahr_iqb_fragebogen_4"),
                       label = NULL,
                       choices = c("2016", "2021"),
                       selected = c("2021")
                     ),

                     p("Schulfach:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("fach_iqb_fragebogen_4"),
                       choices = c("Mathematik",
                                   "Deutsch"),
                       multiple = FALSE,
                       selected = "Mathematik"
                     )
                     ),

    conditionalPanel(condition = "input.klasse_ipb_fragebogen != '4. Klasse' & input.klasse_iqb_fragebogen == '9. Klasse'",
                     ns = ns,

                     p("Jahre:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("jahr_iqb_fragebogen_9"),
                       label = NULL,
                       choices = c("2012", "2018", "2024"),
                       selected = c("2024")
                     ),

                     p("Region:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("region_iqb_fragebogen_9"),
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
                     ),

                     p("Schulfach:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("fach_iqb_fragebogen_9"),
                       choices = c("Mathematik",
                                   "Biologie",
                                   "Chemie",
                                   "Physik"),
                       multiple = FALSE,
                       selected = "Mathematik"
                     ),

                     p("Vergleich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("gruppe_iqb_fragebogen_9"),
                       choices = c("nach Geschlecht",
                                   "nach Zuwanderungsgeschichte",
                                   "nach Bildungskapital"),
                       multiple = FALSE,
                       selected = c("nach Geschlecht")
                     )
                     ),


    br(),
    darstellung(id = "leistungsschwache_schueler2"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_schule_kompetenz_3", title="",
                       content = paste0("Die erste Darstellung zeigt, dass sowohl das Interesse als auch die Einschätzung der eignenen Fähigkeiten in Mathematik bei Mädchen geringer als bei Jungen ist. Betrachtet man zum Vergleich Interesse und Einschätzung der Fähigkeiten für das Fach Deutsch, sieht man eine gegenteilige Tendenz. Hier geben Mädchen ein höheres Interesse an und schätzen sich als kompetenter ein."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_kompetenz_3")
  )
}

#' schule_kurse_verlauf Server Functions
#'
#' @noRd
mod_schule_kurse_iqb_fragen_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$klasse_iqb_fragebogen, {
      r$klasse_iqb_fragebogen <- input$klasse_iqb_fragebogen
    })

    observeEvent(input$jahr_iqb_fragebogen_4, {
      r$jahr_iqb_fragebogen_4 <- input$jahr_iqb_fragebogen_4
    })

    observeEvent(input$jahr_iqb_fragebogen_9, {
      r$jahr_iqb_fragebogen_9 <- input$jahr_iqb_fragebogen_9
    })

    observeEvent(input$region_iqb_fragebogen_9, {
      r$region_iqb_fragebogen_9 <- input$region_iqb_fragebogen_9
    })

    observeEvent(input$fach_iqb_fragebogen_4, {
      r$fach_iqb_fragebogen_4 <- input$fach_iqb_fragebogen_4
    })

    observeEvent(input$fach_iqb_fragebogen_9, {
      r$fach_iqb_fragebogen_9 <- input$fach_iqb_fragebogen_9
    })

    observeEvent(input$gruppe_iqb_fragebogen_9, {
      r$gruppe_iqb_fragebogen_9 <- input$gruppe_iqb_fragebogen_9
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_iqb_fragen_ui("mod_schule_kurse_iqb_fragen_ui_1")

## To be copied in the server
# mod_schule_kurse_iqb_fragen_server("mod_schule_kurse_iqb_fragen_ui_1")
