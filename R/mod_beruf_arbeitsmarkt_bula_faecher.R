#' beruf_arbeitsmarkt_bl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_bula_faecher_ui <- function(id){
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
      inputId = ns("ansicht_beruf_faecher_bula"),
      label = NULL,
      choices = c("Übersicht - Kartendiagramm", "Zeitverlauf - Liniendiagramm",
                  "Gruppenvergleich - Balkendiagramm"),
      selected = "Übersicht - Kartendiagramm"
    ),


    conditionalPanel(condition = "input.ansicht_beruf_faecher_bula == 'Übersicht - Kartendiagramm'",
                     ns = ns,
                     p("Auswahl des Jahres:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("zeit_beruf_faecher_bula_karte"),
                       label = NULL,
                       choices = 2013:2023,
                       selected = 2023
                     ),
                     p("Beschäftigtengruppe:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator_beruf_faecher_bula_karte"),
                       choices = c("Auszubildende",
                                   "Beschäftigte"),
                       # justified = TRUE,
                       # checkIcon = list(yes = icon("ok",
                       #                             lib = "glyphicon")),
                       selected= "Beschäftigte",
                     ),
                     p("Berufsfeld:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("fachbereich_beruf_faecher_bula_karte"),
                       choices = c( "MINT",
                                    "Mathematik/ Naturwissenschaften" = "Mathematik, Naturwissenschaften",
                                    "Informatik",
                                    "Technik (gesamt)",
                                    "Bau- und Gebäudetechnik",
                                    "Gesundheitstechnik",
                                    "Landtechnik",
                                    "Produktionstechnik",
                                    "Verkehrs-, Sicherheits- u. Veranstaltungstechnik"
                       ),
                       selected = "Technik (gesamt)"
                     ),
                     br(),
                     darstellung(id="dh_beruf_fach_1"),
                     br(),
                     br(),
                     shinyBS::bsPopover(id="ih_beruf_fach_1", title="",
                                        content = paste0("Die Karte in der ersten Einstellung zeigt beispielsweise, dass 2023 der Anteil an Beschäftigten in Technik von allen Bundesländern in Berlin am geringsten ausfällt. Den höchsten Anteil an Beschäftigten in Technik haben Thüringen und Baden-Württemberg mit 21,5 %. Jede:r Fünfte arbeitet hier im Technik-Bereich."),
                                        trigger = "hover", placement = "top"
                                        ),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_fach_1")

    ),

    conditionalPanel(condition = "input.ansicht_beruf_faecher_bula == 'Zeitverlauf - Liniendiagramm'",
                     ns = ns,
                     p("Jahre:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("zeit_beruf_faecher_bula_verlauf"),
                       label = NULL,
                       choices = 2013:2023,
                       selected = c(2017, 2023)
                     ),
                     p("Beschäftigtengruppe:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator_beruf_faecher_bula_verlauf"),
                       choices = c("Auszubildende",
                                   "Beschäftigte"),
                       # justified = TRUE,
                       # checkIcon = list(yes = icon("ok",
                       #                             lib = "glyphicon")),
                       selected= "Beschäftigte",

                     ),
                     p("Berufsfeld:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("fachbereich_beruf_faecher_bula_verlauf"),
                       choices = c( "MINT",
                                    "Mathematik/ Naturwissenschaften" = "Mathematik, Naturwissenschaften",
                                    "Informatik",
                                    "Technik (gesamt)",
                                    "Bau- und Gebäudetechnik",
                                    "Gesundheitstechnik",
                                    "Landtechnik",
                                    "Produktionstechnik",
                                    "Verkehrs-, Sicherheits- u. Veranstaltungstechnik"
                       ),
                       selected = "Technik (gesamt)"
                     ),
                     p("Regionen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("region_beruf_faecher_bula_verlauf"),
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
                                   "Thüringen"
                                   ,
                                   "Westdeutschland (o. Berlin)",
                                   "Ostdeutschland (inkl. Berlin)"
                       ),
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE,
                                      `deselect-all-text` = "Alle abwählen",
                                      `select-all-text` = "Alle auswählen"),
                       selected = c("Ostdeutschland (inkl. Berlin)", "Nordrhein-Westfalen")
                     ),
                     p("Darstellungsart:"),
                     shinyWidgets::radioGroupButtons(
                       inputId = ns("abs_beruf_faecher_bula_verlauf"),
                       choices = c("In Prozent", "Anzahl"),
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     ),

                     br(),
                     shinyBS::bsPopover(id="ih_beruf_faecher_7", title="",
                                        content = paste0("Die erste Darstellung zeigt z. B., dass sich der Anteil von Beschäftigten im Berufsfeld Technik in den ostdeutschen Bundesländern und Nordrhein-Westfalen ähnlich entwickelt hat. Der Anteil ist in beiden Regionen nur leicht um rund 1 Prozentpunkt gesunken."),
                                        trigger = "hover", placement = "top"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_faecher_7")
    ),

    conditionalPanel(condition = "input.ansicht_beruf_faecher_bula == 'Gruppenvergleich - Balkendiagramm'",
                     ns = ns,
                     p("Auswahl des Jahres:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("zeit_beruf_faecher_bula_balken"),
                       label = NULL,
                       choices = 2013:2023,
                       selected = 2023
                     ),
                     p("Beschäftigtengruppe:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator_beruf_faecher_bula_balken"),
                       choices = c("Auszubildende",
                                   "Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)",
                                   "Beschäftigte",
                                   "ausländische Auszubildende",
                                   "ausländische Beschäftigte",
                                   "Beschäftigte 25-55",
                                   "Beschäftigte u25",
                                   "Beschäftigte ü55"),
                       selected = "Beschäftigte",
                       multiple = FALSE
                     ),

                     p("Berufsfeld:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("fachbereich_beruf_faecher_bula_balken"),
                       choices = c( "MINT",
                                    "Mathematik/ Naturwissenschaften" = "Mathematik, Naturwissenschaften",
                                    "Informatik",
                                    "Technik (gesamt)",
                                    "Bau- und Gebäudetechnik",
                                    "Gesundheitstechnik",
                                    "Landtechnik",
                                    "Produktionstechnik",
                                    "Verkehrs-, Sicherheits- u. Veranstaltungstechnik"
                       ),
                       selected = "Informatik"
                     ),


                     br(),
                     shinyBS::bsPopover(id="ih_beruf_faecher_3", title="",
                                        content = paste0("Diese Darstellung gibt einen Überblick darürber, wie hoch der Anteil von MINT-Beschäftigten in den Bundesländern ist. Beispielsweise sind 2023 etwa 3,7 % der Beschäftigten in Bayern im Bereich Informatik tätig. Damit liegt Bayern etwas über dem gesamtdeutschen Durchschnitt von 3,1 %."),
                                        trigger = "hover", placement = "top"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_faecher_3")
                     # br(),
                     # shinyBS::bsPopover(id="ih_beruf_fach_3", title="",
                     #                    content = paste0("Diese Darstellung gibt einen Überblick darürber, wie hoch der Anteil von MINT-Beschäftigten in den Bundesländern ist. Beispielsweise sind 2022 etwa 3,5 % der Beschäftigten in Bayern im Bereich Informatik tätig. Damit liegt Bayern etwas über dem gesamtdeutschen Durchschnitt von knapp 3 %."),
                     #                    trigger = "hover"),
                     # tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_fach_3")
    )
  )
}

#' beruf_arbeitsmarkt_bl_verlauf Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_bula_faecher_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_beruf_faecher_bula, {
      r$ansicht_beruf_faecher_bula <- input$ansicht_beruf_faecher_bula
    })

    observeEvent(input$zeit_beruf_faecher_bula_karte, {
      r$zeit_beruf_faecher_bula_karte <- input$zeit_beruf_faecher_bula_karte
    })

    observeEvent(input$indikator_beruf_faecher_bula_karte, {
      r$indikator_beruf_faecher_bula_karte <- input$indikator_beruf_faecher_bula_karte
    })

    observeEvent(input$fachbereich_beruf_faecher_bula_karte, {
      r$fachbereich_beruf_faecher_bula_karte <- input$fachbereich_beruf_faecher_bula_karte
    })

    observeEvent(input$zeit_beruf_faecher_bula_verlauf, {
      r$zeit_beruf_faecher_bula_verlauf <- input$zeit_beruf_faecher_bula_verlauf
    })

    observeEvent(input$indikator_beruf_faecher_bula_verlauf, {
      r$indikator_beruf_faecher_bula_verlauf <- input$indikator_beruf_faecher_bula_verlauf
    })

    observeEvent(input$fachbereich_beruf_faecher_bula_verlauf, {
      r$fachbereich_beruf_faecher_bula_verlauf <- input$fachbereich_beruf_faecher_bula_verlauf
    })

    observeEvent(input$region_beruf_faecher_bula_verlauf, {
      r$region_beruf_faecher_bula_verlauf <- input$region_beruf_faecher_bula_verlauf
    })

    observeEvent(input$abs_beruf_faecher_bula_verlauf, {
      r$abs_beruf_faecher_bula_verlauf <- input$abs_beruf_faecher_bula_verlauf
    })

    observeEvent(input$zeit_beruf_faecher_bula_balken, {
      r$zeit_beruf_faecher_bula_balken <- input$zeit_beruf_faecher_bula_balken
    })

    observeEvent(input$indikator_beruf_faecher_bula_balken, {
      r$indikator_beruf_faecher_bula_balken <- input$indikator_beruf_faecher_bula_balken
    })

    observeEvent(input$fachbereich_beruf_faecher_bula_balken, {
      r$fachbereich_beruf_faecher_bula_balken <- input$fachbereich_beruf_faecher_bula_balken
    })

  })
}

#' beruf_arbeitsmarkt_bl Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_bl_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_bl, {
      r$date_arbeitsmarkt_bl <- input$date_arbeitsmarkt_bl
    })

    observeEvent(input$pick_i, {
      r$pick_i <- input$pick_i
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_bl_ui("beruf_arbeitsmarkt_bl_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_bl_server("beruf_arbeitsmarkt_bl_1")
