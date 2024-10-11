#' beruf_arbeitsmarkt_anforderungen_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_anforderungen_gender_ui <- function(id){
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
      inputId = ns("ansicht_arbeitsmarkt_wahl_gender"),
      label = NULL,
      choices = c("Einzelansicht - Kuchendiagramm",
                  "Übersicht - Kartendiagramm",
                  "Zeitverlauf - Liniendiagramm"),
      selected = "Einzelansicht - Kuchendiagramm"
    ),

    conditionalPanel(condition = "input.ansicht_arbeitsmarkt_wahl_gender == 'Einzelansicht - Kuchendiagramm'",
                     ns = ns,
                     p("Auswahl des Jahres:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("date_arbeitsmarkt_wahl_gender_pie"),
                       label = NULL,
                       choices = c(2021, 2022),
                       selected = 2022
                     ),
                     p("Beschäftigungsform:"),
                      shinyWidgets::pickerInput(
                        inputId = ns("level_arbeitsmarkt_wahl_gender_pie"),
                        choices = c("Beschäftigte",
                                    "Auszubildende",
                                    "Auszubildende mit neuem Lehrvertrag" =  "Auszubildende (1. Jahr)",
                                    "ausländische Beschäftigte",
                                    "ausländische Auszubildende"),
                        selected = "Beschäftigte",
                        multiple = FALSE
                      ),
                     shinyBS::bsPopover(id="ih_beruf_mint_4", title="",
                                        content = paste0("Die erste Einstellung zeigt, dass von allen weiblichen Beschäftigten 2022 nur 8.4 % eine MINT-Tätigkeit ausüben. Bei den Männern arbeitet ein weitaus größerer Anteil von rund 35 % in der MINT-Branche. Dabei sind die meisten Männer im Bereich Technik tätig (29 %)."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_4"),

                    br(),
                    br(),
                    shinyBS::bsPopover(id="poppp123", title = "",
                                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                                       trigger = "hover"),
                    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "poppp123"),
                    br(),
    ),

    conditionalPanel(condition = "input.ansicht_arbeitsmarkt_wahl_gender == 'Übersicht - Kartendiagramm'",
                     ns = ns,
                     p("Auswahl des Jahres:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("date_arbeitsmarkt_wahl_gender_karte"),
                       label = NULL,
                       choices = c(2021, 2022),
                       selected = 2022
                     ),
                     p("Beschäftigungsform:"),
                      shinyWidgets::pickerInput(
                        inputId = ns("level_arbeitsmarkt_wahl_gender_karte"),
                        choices = c("Beschäftigte",
                                    "Auszubildende",
                                    "Auszubildende mit neuem Lehrvertrag" =  "Auszubildende (1. Jahr)", #auskommentiert bis für 2022 auch da
                                    "ausländische Beschäftigte",
                                    "ausländische Auszubildende"),
                        selected = "Beschäftigte",
                        multiple = FALSE
                      ),

                     p("Regionen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("region_arbeitsmarkt_wahl_gender_pie"),
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
                                   "Thüringen",
                                   "Westdeutschland (o. Berlin)",
                                   "Ostdeutschland (inkl. Berlin)"
                       ),
                       multiple = FALSE,
                       selected = "Deutschland"
                     ),

                     p("Berufsfeld:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("fach_arbeitsmarkt_wahl_gender_karte"),
                       choices = c("MINT",
                                   "Mathematik/ Naturwissenschaften" = "Mathematik, Naturwissenschaften",
                                   "Informatik",
                                   "Technik (gesamt)",
                                   "Alle Berufsfelder außer MINT" = "Andere Berufsgruppen"),
                       multiple = FALSE,
                       selected = "MINT"),
                     br(),
                     shinyBS::bsPopover(id="dh_beruf_mint_5", title = "",
                                        content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                                        trigger = "hover"),
                     tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_beruf_mint_5"),

                     br(),
                     br(),
                     shinyBS::bsPopover(id="ih_beruf_mint_5", title="",
                                        content = paste0("Vergleicht man die Legenden der Karten sieht man, dass der Anteil von Frauen, die in MINT-Berufen arbeiten, weitaus geringer ist als der von Männern. In Rheinland-Pfalz arbeiten beispielsweise 2022 nur 7 % aller berufstätigen Frauen in MINT, dagegen aber 35 % der Männer."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_5")

    ),

    conditionalPanel(condition = "input.ansicht_arbeitsmarkt_wahl_gender == 'Zeitverlauf - Liniendiagramm'",
                     ns = ns,
                     p("Jahre:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("date_arbeitsmarkt_wahl_gender_verlauf"),
                       label = NULL,
                       choices = 2013:2022,
                       selected = c(2017, 2022)
                     ),
                     p("Beschäftigungsform:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("level_arbeitsmarkt_wahl_gender_verlauf"),
                       choices = c("Beschäftigte",
                                   "Auszubildende",
                                   "Auszubildende mit neuem Lehrvertrag" =  "Auszubildende (1. Jahr)", #auskommentiert bis für 2022 auch da
                                   "ausländische Beschäftigte",
                                   "ausländische Auszubildende"),
                       selected = "Beschäftigte",
                       multiple = FALSE
                     ),
                     p("Regionen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("states_arbeitsmarkt_wahl_gender_verlauf"),
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
                                   "Thüringen",
                                   "Westdeutschland (o. Berlin)",
                                   "Ostdeutschland (inkl. Berlin)"
                       ),
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE,
                                      `deselect-all-text` = "Alle abwählen",
                                      `select-all-text` = "Alle auswählen"),
                       selected = c("Deutschland", "Niedersachsen")
                     ),
                     p("Berufsfeld:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("fach_arbeitsmarkt_wahl_gender_verlauf"),
                       choices = c("MINT",
                                   "Mathematik/ Naturwissenschaften" = "Mathematik, Naturwissenschaften",
                                   "Informatik",
                                   "Technik (gesamt)",
                                   "Alle Berufsfelder außer MINT" = "Andere Berufsgruppen"),
                       multiple = FALSE,
                       selected = "MINT"
                       ),
                     p("Darstellungsart:"),
                     shinyWidgets::radioGroupButtons(
                       inputId = ns("abs_zahlen_arbeitsmarkt_wahl_gender_verlauf"),
                       choices = c("In Prozent", "Anzahl"),
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     ),

                     br(),
                     shinyBS::bsPopover(id="ih_beruf_mint_6", title="",
                                        content = paste0("Die erste Einstellung zeigt beispielsweise, dass in Niedersachsen im Vergleich zu Deutschland der Anteil an Frauen, die eine MINT-Tätigkeit ergreifen, um ein bis zwei Prozentpunkte höher liegt."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_6")

    )
  )
}

#' beruf_arbeitsmarkt_anforderungen_gender Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_anforderungen_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_arbeitsmarkt_wahl_gender, {
      r$ansicht_arbeitsmarkt_wahl_gender <- input$ansicht_arbeitsmarkt_wahl_gender
    })
    observeEvent(input$region_arbeitsmarkt_wahl_gender_pie, {
      r$region_arbeitsmarkt_wahl_gender_pie <- input$region_arbeitsmarkt_wahl_gender_pie
    })
    observeEvent(input$date_arbeitsmarkt_wahl_gender_pie, {
      r$date_arbeitsmarkt_wahl_gender_pie <- input$date_arbeitsmarkt_wahl_gender_pie
    })
    observeEvent(input$level_arbeitsmarkt_wahl_gender_pie, {
      r$level_arbeitsmarkt_wahl_gender_pie <- input$level_arbeitsmarkt_wahl_gender_pie
    })
    observeEvent(input$date_arbeitsmarkt_wahl_gender_karte, {
      r$date_arbeitsmarkt_wahl_gender_karte <- input$date_arbeitsmarkt_wahl_gender_karte
    })
    observeEvent(input$level_arbeitsmarkt_wahl_gender_karte, {
      r$level_arbeitsmarkt_wahl_gender_karte <- input$level_arbeitsmarkt_wahl_gender_karte
    })
    observeEvent(input$fach_arbeitsmarkt_wahl_gender_karte, {
      r$fach_arbeitsmarkt_wahl_gender_karte <- input$fach_arbeitsmarkt_wahl_gender_karte
    })
    observeEvent(input$date_arbeitsmarkt_wahl_gender_verlauf, {
      r$date_arbeitsmarkt_wahl_gender_verlauf <- input$date_arbeitsmarkt_wahl_gender_verlauf
    })
    observeEvent(input$level_arbeitsmarkt_wahl_gender_verlauf, {
      r$level_arbeitsmarkt_wahl_gender_verlauf <- input$level_arbeitsmarkt_wahl_gender_verlauf
    })
    observeEvent(input$states_arbeitsmarkt_wahl_gender_verlauf, {
      r$states_arbeitsmarkt_wahl_gender_verlauf <- input$states_arbeitsmarkt_wahl_gender_verlauf
    })
    observeEvent(input$fach_arbeitsmarkt_wahl_gender_verlauf, {
      r$fach_arbeitsmarkt_wahl_gender_verlauf <- input$fach_arbeitsmarkt_wahl_gender_verlauf
    })
    observeEvent(input$abs_zahlen_arbeitsmarkt_wahl_gender_verlauf, {
      r$abs_zahlen_arbeitsmarkt_wahl_gender_verlauf <- input$abs_zahlen_arbeitsmarkt_wahl_gender_verlauf
    })
  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_anforderungen_gender_ui("beruf_arbeitsmarkt_anforderungen_gender_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_anforderungen_gender_server("beruf_arbeitsmarkt_anforderungen_gender_1")
