#' schule_kurse_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_map_ui <- function(id){
  ns <- NS(id)
  tagList(


    tags$head(
      tags$style(HTML("
        .dropdown-menu .bs-actionsbox .btn-group .btn {
          background-color: #e7f1ff !important;  /* Hellblau für die Alle auswählen/abwählen Buttons */
          color: #000000 !important;
        }
        .dropdown-menu .bs-actionsbox .btn-group .btn:hover {
          background-color: #d0e8ff !important;  /* Etwas dunkleres Blau beim Hover */
          color: #000000 !important;
        }
      "))
    ),

    p("Darstellungsart:"),
    shiny::radioButtons(
      inputId = ns("ansicht_map"),
      label = NULL,
      choices = c("Übersicht - Kartendiagramm",
                  "Zeitverlauf - Liniendiagramm",
                  "Vergleich - Balkendiagramm"),
      selected = "Übersicht - Kartendiagramm"
    ),

    conditionalPanel(condition = "input.ansicht_map == 'Übersicht - Kartendiagramm'",
      ns = ns,
      p("Jahr:"),
      shinyWidgets::sliderTextInput(
        inputId = ns("date_map"),
        label = NULL,
        choices = 2013:2022,
        selected = 2022
      ),
      p("Fach/Fächergruppe:"),
      shinyWidgets::pickerInput(
        inputId = ns("subject_map"),
        choices = c("MINT-Fächer (gesamt)",
                    "Mathematik",
                    "Informatik",
                    "Physik",
                    "Chemie",
                    "Biologie",
                    "andere Fächer (gesamt)",
                    "Deutsch",
                    "Fremdsprachen",
                    "Gesellschaftswissenschaften",
                    "Musik/Kunst",
                    "Religion/Ethik",
                    "Sport"),
        selected = "MINT-Fächer (gesamt)"
      ),
      br(),
      shinyBS::bsPopover(id="popover1_box2", title="",
                         content = paste0("In der ersten Einstellung ist zu sehen, dass der Anteil von MINT-Fächern an den Grundkursbelegungen mit 29 % in Sachsen deutschlandweit am höchsten ist. Bei den Leistungskursbelegungen ist der Anteil der MINT-Fächer in Sachsen-Anhalt mit 50 % am höchsten. Die Vergleiche zwischen den Bundesländern sind jedoch schwierig, da die Regelungen für die Wahl der Kurse in den Bundesländern sehr unterschiedlich sind."),
                         trigger = "hover"),
      tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="popover1_box2")

     ),

    conditionalPanel(condition = "input.ansicht_map == 'Zeitverlauf - Liniendiagramm'",
                     ns = ns,
                     p("Jahre:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("date_kurse_verlauf_multiple"),
                       label = NULL,
                       choices = 2013:2022,,
                       selected = c(2016, 2022)
                     ),
                     p("Kursart:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("topic_selected_multiple"),
                       choices = c("Grundkurse", "Leistungskurse"),
                       selected = "Leistungskurse"
                     ),
                     p("Fach/Fächergruppe:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("subject_selected_multiple"),
                       choices = c("MINT-Fächer (gesamt)",
                                   "Mathematik",
                                   "Informatik",
                                   "Physik",
                                   "Chemie",
                                   "Biologie",
                                   "andere Fächer (gesamt)",
                                   "Deutsch",
                                   "Fremdsprachen",
                                   "Gesellschaftswissenschaften",
                                   "Musik/Kunst",
                                   "Religion/Ethik",
                                   "Sport"),
                       selected = "MINT-Fächer (gesamt)",
                     ),
                     p("Regionen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("states_kurse_verlauf_multiple"),
                       choices = c("Deutschland",
                                   "Westdeutschland (o. Berlin)",
                                   "Ostdeutschland (inkl. Berlin)",
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
                       ),
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE,
                                      `deselect-all-text` = "Alle abwählen",
                                      `select-all-text` = "Alle auswählen"),
                       selected = c("Hessen", "Hamburg")
                     ),
                     p("Darstellungsart:"),
                     shinyWidgets::radioGroupButtons(
                       inputId = ns("abs_zahlen_kurse_verlauf_multiple"),
                       choices = c("In Prozent", "Anzahl"),
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     ),
                     br(),
                     shinyBS::bsPopover(id="popover2_box2", title="",
                                        content = paste0("In dieser Grafik wird in der ersten Einstellung der Anteil der MINT-Fächer an den Leistungskursbelegungen in Hamburg und Hessen verglichen. Hierbei erkennt man, dass Hamburg stets geringere MINT-Anteile hat als Hessen."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="popover2_box2")

    ),

    conditionalPanel(condition = "input.ansicht_map == 'Vergleich - Balkendiagramm'",
        ns = ns,
        p("Jahr:"),
        shinyWidgets::sliderTextInput(
          inputId = ns("date_comparison_bl"),
          label = NULL,
          choices = 2012:2022,
          selected = 2022),

        p("Kursart:"),
        shinyWidgets::pickerInput(
          inputId = ns("indikator_comparison_bl"),
          choices = c("Grundkurse", "Leistungskurse"),
          selected = "Leistunskurse"
        ),

        p("Fach/Fächergruppe:"),
        conditionalPanel(condition = "input.indikator_comparison_bl=='Grundkurse'",
                         ns= ns,
                         shinyWidgets::pickerInput(
                           inputId = ns("subject_comparison_bl1"),
                           choices =  c("MINT-Fächer (gesamt)",
                                        "Mathematik",
                                        "Informatik",
                                        "Physik",
                                        "Chemie",
                                        "Biologie",
                                        "andere Fächer (gesamt)",
                                        "Deutsch",
                                        "Fremdsprachen",
                                        "Gesellschaftswissenschaften",
                                        "Musik/Kunst",
                                        "Religion/Ethik",
                                        "Sport"))),
        conditionalPanel(condition = "input.indikator_comparison_bl=='Leistungskurse'",
                         ns= ns,
                         shinyWidgets::pickerInput(
                           inputId = ns("subject_comparison_bl2"),
                           choices =  c("MINT-Fächer (gesamt)",
                                        "Mathematik",
                                        "Informatik",
                                        "Physik",
                                        "Chemie",
                                        "Biologie",
                                        "andere Fächer (gesamt)",
                                        "Deutsch",
                                        "Fremdsprachen",
                                        "Gesellschaftswissenschaften",
                                        "Musik/Kunst",
                                        "Sport"))),

        br(),
        shinyBS::bsPopover(id="popover3_box2", title="",
                           content = paste0("In der ersten Einstellung werden die Länder direkt miteinander verglichen. In Sachsen-Anhalt entfallen nahezu die Hälfte (49 %) der Leistungskursbelegungen auf den MINT-Bereich, womit das Bundesland eine Spitzenposition einnimmt."),
                           trigger = "hover"),
        tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="popover3_box2")
    ),

  br(),
  darstellung(id="dh_schule_fach_y1"),
  br()
  )
}

#' schule_kurse_map Server Functions
#'
#' @noRd
mod_schule_kurse_map_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_map, {
      r$ansicht_map <- input$ansicht_map
    })

    # Karten
    observeEvent(input$date_map, {
      r$date_map <- input$date_map
    })

    observeEvent(input$subject_map, {
      r$subject_map <- input$subject_map
    })


    # Zeitverlauf
    observeEvent(input$states_kurse_verlauf_multiple, {
      r$states_kurse_verlauf_multiple <- input$states_kurse_verlauf_multiple
    })

    observeEvent(input$subject_selected_multiple, {
      r$subject_selected_multiple <- input$subject_selected_multiple
    })

    observeEvent(input$abs_zahlen_kurse_verlauf_multiple, {
      r$abs_zahlen_kurse_verlauf_multiple <- input$abs_zahlen_kurse_verlauf_multiple
    })

    observeEvent(input$topic_selected_multiple, {
      r$topic_selected_multiple <- input$topic_selected_multiple
    })

    observeEvent(input$date_kurse_verlauf_multiple, {
      r$date_kurse_verlauf_multiple <- input$date_kurse_verlauf_multiple
    })

    # Balkendiagramm
    observeEvent(input$date_comparison_bl, {
      r$date_comparison_bl <- input$date_comparison_bl
    })

    observeEvent(input$indikator_comparison_bl, {
      r$indikator_comparison_bl <- input$indikator_comparison_bl
    })

    observeEvent(input$subject_comparison_bl1, {
      r$subject_comparison_bl1 <- input$subject_comparison_bl1
    })

    observeEvent(input$subject_comparison_bl2, {
      r$subject_comparison_bl2 <- input$subject_comparison_bl2
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_map_ui("schule_kurse_map_1")

## To be copied in the server
# mod_schule_kurse_map_server("schule_kurse_map_1")
