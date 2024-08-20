#' schule_kurse_mint_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_schule_kurse_mint_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Betrachtungsart:"),
    shiny::radioButtons(
      inputId = ns("ansicht_mint_map"),
      label = NULL,
      choices = c("Übersicht - Kartendiagramm", "Zeitverlauf - Liniendiagramm"),
      selected = "Übersicht - Kartendiagramm"
    ),

    conditionalPanel(condition = "input.ansicht_mint_map == 'Übersicht - Kartendiagramm'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("date_mint_map"),
                       label = NULL,
                       choices = 2013:2022,
                       selected = 2022
                       ),

                     br(),
                     shinyBS::bsPopover(id="box1_neu", title="",
                                        content = paste0("In der ersten Einstellung ist zu sehen, dass der Anteil von MINT-Fächern an den Grundkursbelegung mit 29 % in Sachsen deutschlandweit am höchsten ist. Bei den Leistungskursen ist der Anteil der MINT-Fächer in Sachsen-Anhalt mit 50 % am höchsten. Die Vergleiche zwischen den Bundesländern sind jedoch schwierig, da die Regelungen für die Wahl der Kurse in den Bundesländern sehr unterschiedlich sind."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="box1_neu")

                     ),
    conditionalPanel(condition = "input.ansicht_mint_map == 'Zeitverlauf - Liniendiagramm'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("date_kurse_verlauf_mint"),
                       label = NULL,
                       choices = 2013:2022,
                       selected = c(2016, 2022)
                     ),
                     p("Kursart:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("topic_selected_mint"),
                       choices = c("Grundkurse", "Leistungskurse"),
                       selected = "Leistungskurse"
                     ),
                     p("Regionen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("states_kurse_verlauf_mint"),
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
                     p("Betrachtung:"),
                     shinyWidgets::radioGroupButtons(
                       inputId = ns("abs_zahlen_kurse_verlauf_mint"),
                       choices = c("In Prozent", "Anzahl"),
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     ),

                     br(),
                     shinyBS::bsPopover(id="ih_schule_fach_1b", title="",
                                        content = paste0("In der ersten Einstellung ist zu sehen, dass z.B. Hamburg im Jahr 2016 deutlich unter Hessen liegt im Bezug auf den Anteil der MINT-Fächer an den Leistungskursbelegungen (25% Hamburg, 41% Hessen). Dieser Trend setzt sich in den folgenden Jahren so fort. Die Vergleiche zwischen den Staaten sind aufgrund der föderalen Bildungspolitik der Länder jedoch noch schwierig zu vergleichen, da jedes Bundesland andere und besondere Regelungen in der Fächerbelegung hat. "),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_fach_1b")

    ),

    br(),
    shinyBS::bsPopover(id="dh_schule_fach_neu2", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_schule_fach_neu2"),
    br(),

  )
}

#' schule_kurse_map Server Functions
#'
#' @noRd
mod_schule_kurse_mint_map_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_mint_map, {
      r$ansicht_mint_map <- input$ansicht_mint_map
    })

    observeEvent(input$date_mint_map, {
      r$date_mint_map <- input$date_mint_map
    })

    observeEvent(input$ansicht_mint_map, {
      r$ansicht_mint_map <- input$ansicht_mint_map
    })

    observeEvent(input$states_kurse_verlauf_mint, {
      r$states_kurse_verlauf_mint <- input$states_kurse_verlauf_mint
    })

    observeEvent(input$abs_zahlen_kurse_verlauf_mint, {
      r$abs_zahlen_kurse_verlauf_mint <- input$abs_zahlen_kurse_verlauf_mint
    })

    observeEvent(input$topic_selected_mint, {
      r$topic_selected_mint <- input$topic_selected_mint
    })

    observeEvent(input$date_kurse_verlauf_mint, {
      r$date_kurse_verlauf_mint <- input$date_kurse_verlauf_mint
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_map_ui("schule_kurse_map_1")

## To be copied in the server
# mod_schule_kurse_map_server("schule_kurse_map_1")
