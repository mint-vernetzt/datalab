


mod_schule_kurse_iqb_linien_ui <- function(id){
  ns <- NS(id)

  tagList(
    p("Klassenstufe:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("klasse_iqb_linien"),
      choices = c("9. Klasse",
                  "4. Klasse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),

    p("Region:"),
    conditionalPanel(condition = "input.klasse_iqb_linien == '4. Klasse'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("land_iqb_linien_4"),
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
                       options = list(
                         "actions-box" = TRUE,
                         "deselect-all-text" = "Alle abwählen",
                         "select-all-text" = "Alle auswählen"
                       )
    )),
    conditionalPanel(condition = "input.klasse_iqb_linien == '9. Klasse'",
                     ns = ns,

                     shinyWidgets::pickerInput(
                       inputId = ns("land_iqb_linien_9"),
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
                       options = list(
                         "actions-box" = TRUE,
                         "deselect-all-text" = "Alle abwählen",
                         "select-all-text" = "Alle auswählen"
                       )),

                     p("Schulfach:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("fach_iqb_linien_9"),
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
mod_schule_kurse_iqb_linien_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$klasse_iqb_linien, {
      r$klasse_iqb_linien <- input$klasse_iqb_linien
    })
    observeEvent(input$land_iqb_linien_4, {
      r$land_iqb_linien_4 <- input$land_iqb_linien_4
    })
    observeEvent(input$land_iqb_linien_9, {
      r$land_iqb_linien_9 <- input$land_iqb_linien_9
    })
    observeEvent(input$fach_iqb_linien_9, {
      r$fach_iqb_linien_9 <- input$fach_iqb_linien_9
    })


  })
}

## To be copied in the UI
# mod_schule_kurse_iqb_linien_ui("mod_schule_kurse_iqb_linien_ui_1")

## To be copied in the server
# mod_schule_kurse_iqb_linien_server("mod_schule_kurse_iqb_linien_1")
