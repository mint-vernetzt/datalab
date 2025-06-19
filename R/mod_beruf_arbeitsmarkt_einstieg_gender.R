#' beruf_arbeitsmarkt_einstieg_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_einstieg_gender_ui <- function(id){
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
      inputId = ns("ansicht_arbeitsmarkt_einstieg_gender"),
      label = NULL,
      choices = c("Gruppenvergleich - Balkendiagramm","Einzelansicht - Kuchendiagramm"),
      selected = "Gruppenvergleich - Balkendiagramm"
    ),
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_einstieg_gender"),
      label = NULL,
      choices = 2013:2023,
      selected = 2023
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_arbeitsmarkt_einstieg_gender"),
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
      selected = c("Deutschland")
    ),
    p("Berufsfeld:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachbereich_arbeitsmarkt_einstieg_gender"),
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
      selected = "MINT"
    ),

    conditionalPanel(condition = "input.ansicht_arbeitsmarkt_einstieg_gender == 'Einzelansicht - Kuchendiagramm'",
                    ns = ns,
                     p("Beschäftigtengruppe (max. 2):"),
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator_arbeitsmarkt_einsteig_gender_pie"),
                       choices = c("Auszubildende",
                                   "Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)",
                                   "Beschäftigte",
                                   "ausländische Auszubildende",
                                   "ausländische Beschäftigte"),
                       selected = "Beschäftigte",
                       multiple = TRUE,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "<span style='color: red;'>Maximal 2 Indikatoren auswählen</span>")
                     ),
                     p("Nicht-MINT als Vergleich anzeigen?", style = "color: #b16fab;"),
                     shinyWidgets::radioGroupButtons(
                       inputId = ns("arbeitsmarkt_gender_gegenwert_pie"),
                       choices = c("Ja", "Nein"),
                       selected = "Nein",
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     ),
                     br(),
                     darstellung(id="dh_beruf_frauen_1"),
                     br(),
                     br(),
                     shinyBS::bsPopover(id="ih_beruf_frauen_1", title="",
                                        content = paste0("Der Frauenanteil unter MINT-Beschäftigten beträgt 2023 rund 17 %. Lässt man sich den Nicht-MINT-Bereich dazu anzeigen, sieht man: Hier sind die Frauen dagegen sogar in der knappen Überzahl (55 %)."),
                                        trigger = "hover", placement = "top"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_frauen_1")
    ),
    conditionalPanel(condition = "input.ansicht_arbeitsmarkt_einstieg_gender == 'Gruppenvergleich - Balkendiagramm'",
                     ns = ns,
                     p("Beschäftigtengruppe:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator_arbeitsmarkt_einsteig_gender_balken"),
                       choices = c("Auszubildende",
                                   "Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)",
                                   "Beschäftigte",
                                   "ausländische Auszubildende",
                                   "ausländische Beschäftigte"),
                       selected = c("Beschäftigte", "Auszubildende"),
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE,
                                      `deselect-all-text` = "Alle abwählen",
                                      `select-all-text` = "Alle auswählen"),
                     ),
                    p("Nicht-MINT als Vergleich anzeigen?", style = "color: #b16fab;"),
                    shinyWidgets::radioGroupButtons(
                      inputId = ns("arbeitsmarkt_gender_gegenwert_balken"),
                      choices = c("Ja", "Nein"),
                      selected = "Nein",
                      justified = TRUE,
                      checkIcon = list(yes = icon("ok",
                                                  lib = "glyphicon"))
                    ),
                     br(),
                     shinyBS::bsPopover(id="ih_beruf_frauen_3", title="",
                                        content = paste0("In dieser Übersicht sieht man, dass der Frauenanteil in MINT-Berufen unter den Auszubildenden noch einmal ein wenig geringer ist, als unter den Beschäftigten. Nur 13 % der MINT-Azubis 2023 sind Frauen."),
                                        trigger = "hover", placement = "top"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_frauen_3")
    ))
}

#' beruf_arbeitsmarkt_einstieg_gender Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_einstieg_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_arbeitsmarkt_einstieg_gender, {
      r$ansicht_arbeitsmarkt_einstieg_gender <- input$ansicht_arbeitsmarkt_einstieg_gender
    })
    observeEvent(input$date_arbeitsmarkt_einstieg_gender, {
      r$date_arbeitsmarkt_einstieg_gender <- input$date_arbeitsmarkt_einstieg_gender
    })
    observeEvent(input$region_arbeitsmarkt_einstieg_gender, {
      r$region_arbeitsmarkt_einstieg_gender <- input$region_arbeitsmarkt_einstieg_gender
    })
    observeEvent(input$fachbereich_arbeitsmarkt_einstieg_gender, {
      r$fachbereich_arbeitsmarkt_einstieg_gender <- input$fachbereich_arbeitsmarkt_einstieg_gender
    })
    observeEvent(input$indikator_arbeitsmarkt_einsteig_gender_pie, {
      r$indikator_arbeitsmarkt_einsteig_gender_pie <- input$indikator_arbeitsmarkt_einsteig_gender_pie
    })
    observeEvent(input$arbeitsmarkt_gender_gegenwert_pie, {
      r$arbeitsmarkt_gender_gegenwert_pie <- input$arbeitsmarkt_gender_gegenwert_pie
    })
    observeEvent(input$indikator_arbeitsmarkt_einsteig_gender_balken, {
      r$indikator_arbeitsmarkt_einsteig_gender_balken <- input$indikator_arbeitsmarkt_einsteig_gender_balken
    })
    observeEvent(input$arbeitsmarkt_gender_gegenwert_balken, {
      r$arbeitsmarkt_gender_gegenwert_balken <- input$arbeitsmarkt_gender_gegenwert_balken
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_einstieg_gender_ui("beruf_arbeitsmarkt_einstieg_gender_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_einstieg_gender_server("beruf_arbeitsmarkt_einstieg_gender_1")
