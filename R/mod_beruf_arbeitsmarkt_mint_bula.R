#' beruf_arbeitsmarkt_mint_bula UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_mint_bula_ui <- function(id){
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
      inputId = ns("ansicht_beruf_mint_bula"),
      label = NULL,
      choices = c("Übersicht - Kartendiagramm", "Zeitverlauf - Liniendiagramm",
                  "Gruppenvergleich - Balkendiagramm"),
      selected = "Übersicht - Kartendiagramm"
    ),


    conditionalPanel(condition = "input.ansicht_beruf_mint_bula == 'Übersicht - Kartendiagramm'",
                     ns = ns,
                     p("Auswahl des Jahres:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("zeit_beruf_mint_bula_karte"),
                       label = NULL,
                       choices = c(2021, 2022),
                       selected = 2022
                     ),
                     p("Beschäftigtengruppe:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator_beruf_mint_bula_karte"),
                       choices = c("Auszubildende",
                                   "Beschäftigte"),
                       # justified = TRUE,
                       # checkIcon = list(yes = icon("ok",
                       #                             lib = "glyphicon")),
                       selected= "Beschäftigte",
                     ),
                     br(),
                     shinyBS::bsPopover(id="dh_beruf_mint_bula_1", title = "",
                                        content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                                        trigger = "hover"),
                     tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_beruf_mint_bula_1"),

                     br(),
                     br(),
                     shinyBS::bsPopover(id="ih_beruf_mint_bula_1", title="",
                                        content = paste0("Die Karten in der ersten Einstellung zeigen beispielsweise, dass 2022 der Anteil an Auszubildenden und Beschäftigten in Technik von allen Bundesländern in Berlin am geringsten ausfällt. In Thüringen lernt dagegen rund ein Drittel der Auszubildenden im Bereich Technik. Den höchsten Anteil an Beschäftigten in Technik weist noch knapp vor Thüringen Baden-Württemberg auf (21,7 %)."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_bula_1")

    ),

    conditionalPanel(condition = "input.ansicht_beruf_mint_bula == 'Zeitverlauf - Liniendiagramm'",
                     ns = ns,
    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("zeit_beruf_mint_bula_verlauf"),
      label = NULL,
      choices = 2013:2022,
      selected = c(2017, 2022)
    ),
    p("Beschäftigtengruppe:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_beruf_mint_bula_verlauf"),
      choices = c("Auszubildende",
                   "Beschäftigte"),
      # justified = TRUE,
      # checkIcon = list(yes = icon("ok",
      #                             lib = "glyphicon")),
      selected= "Beschäftigte",

    ),
    # p("Auswahl des Fachs:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("pick_i"),
    #   choices = c( "Bau- und Gebäudetechnik",  "Gesundheitstechnik",
    #
    #                "Informatik",    "Landtechnik", "Mathematik, Naturwissenschaften",
    #
    #                "MINT",  "Produktionstechnik","Technik (gesamt)",
    #
    #                "Verkehrs-, Sicherheits- u. Veranstaltungstechnik"
    #   ),
    #   selected = "Informatik"
    # ),
    # p("Auswahl des Anforderungsniveaus:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("anforderungsniveau_beruf_arbeitsmarkt_bl_verlauf"),
    #   choices = c("Gesamt", "Fachkraft",  "Spezialist:in"="Spezialist", "Expert:in"="Experte"), kab
    #   selected = "Gesamt"
    # ),
    p("Regionen:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_beruf_mint_bula_verlauf"),
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
      selected = c("Ostdeutschland (inkl. Berlin)", "Nordrhein-Westfalen")
    ),
    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_beruf_mint_bula_verlauf"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),

    br(),
    shinyBS::bsPopover(id="ih_beruf_mint_7", title="",
                       content = paste0("Die erste Darstellung zeigt z. B., dass sich der Anteil von Beschäftigten in MINT an allen Beschäftigten deutschlandweit in den ostdeutschen Bundesländern und Nordrhein-Westfalen ählich entwickelt. Der Anteil bleibt relativ konstant und nimmt von 2020 auf 2021 um ca. einen Prozentpunkt ab."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_7")
    ),

    conditionalPanel(condition = "input.ansicht_beruf_mint_bula == 'Gruppenvergleich - Balkendiagramm'",
                     ns = ns,
                     p("Auswahl des Jahres:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("zeit_beruf_mint_bula_balken"),
                       label = NULL,
                       choices = 2021:2022,
                       selected = 2022
                     ),
                     p("Beschäftigtengruppe:"),
                      shinyWidgets::pickerInput(
                        inputId = ns("indikator_beruf_mint_bula_balken"),
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

                     br(),
                     shinyBS::bsPopover(id="ih_beruf_fach_3", title="",
                                        content = paste0("Diese Darstellung gibt einen Überblick darürber, wie hoch der Anteil von MINT-Beschäftigten in den Bundesländern ist. Beispielsweise sind 2022 etwa 3,5 % der Beschäftigten in Bayern im Bereich Informatik tätig. Damit liegt Bayern etwas über dem gesamtdeutschen Durchschnitt von knapp 3 %."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_fach_3")
    )
  )
}

#' beruf_arbeitsmarkt_bl_verlauf Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_mint_bula_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_beruf_mint_bula, {
      r$ansicht_beruf_mint_bula <- input$ansicht_beruf_mint_bula
    })

    observeEvent(input$zeit_beruf_mint_bula_karte, {
      r$zeit_beruf_mint_bula_karte <- input$zeit_beruf_mint_bula_karte
    })

    observeEvent(input$indikator_beruf_mint_bula_karte, {
      r$indikator_beruf_mint_bula_karte <- input$indikator_beruf_mint_bula_karte
    })

    observeEvent(input$zeit_beruf_mint_bula_verlauf, {
      r$zeit_beruf_mint_bula_verlauf <- input$zeit_beruf_mint_bula_verlauf
    })

    observeEvent(input$indikator_beruf_mint_bula_verlauf, {
      r$indikator_beruf_mint_bula_verlauf <- input$indikator_beruf_mint_bula_verlauf
    })
    observeEvent(input$region_beruf_mint_bula_verlauf, {
      r$region_beruf_mint_bula_verlauf <- input$region_beruf_mint_bula_verlauf
    })

    observeEvent(input$abs_beruf_mint_bula_verlauf, {
      r$abs_beruf_mint_bula_verlauf <- input$abs_beruf_mint_bula_verlauf
    })

    observeEvent(input$zeit_beruf_mint_bula_balken, {
      r$zeit_beruf_mint_bula_balken <- input$zeit_beruf_mint_bula_balken
    })

    observeEvent(input$indikator_beruf_mint_bula_balken, {
      r$indikator_beruf_mint_bula_balken <- input$indikator_beruf_mint_bula_balken
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_bl_verlauf_ui("beruf_arbeitsmarkt_bl_verlauf_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_bl_verlauf_server("beruf_arbeitsmarkt_bl_verlauf_1")
